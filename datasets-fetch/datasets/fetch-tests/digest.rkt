#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-missing? dataset-table-column dataset-table-names
                  dataset-table-row-count dataset-table?)
         (only-in file/sha1 bytes->hex-string)
         (only-in json read-json)
         (only-in racket/contract/base -> any/c contract-out listof)
         (only-in racket/list append*)
         (only-in racket/match match)
         (only-in racket/port call-with-output-bytes)
         (only-in racket/runtime-path define-runtime-path)
         (only-in threading ~>))
(provide (contract-out [column-digest (-> vector? string?)]
                       [expect (-> any/c any/c any/c (listof string?))]
                       [parity-problems (-> symbol? dataset-table? (listof string?))]
                       [parity-reference (-> symbol? hash?)]))
(define-runtime-path parity-file "fixtures/parity.json")
(define parity (call-with-input-file parity-file read-json))

(define (column-digest column)
  (~> (call-with-output-bytes
       (lambda (out)
         (for ([v (in-vector column)])
           (match v
             [(? dataset-missing?) (write-bytes #"m" out)]
             [(? exact-integer?) (write-bytes #"i" out) (write-bytes (integer->integer-bytes v 8 #t #f) out)]
             [(? flonum?) (write-bytes #"f" out) (write-bytes (real->floating-point-bytes v 8 #f) out)]
             [(? string?)
              (define b (string->bytes/utf-8 v))
              (write-bytes #"s" out)
              (write-bytes (integer->integer-bytes (bytes-length b) 4 #f #f) out)
              (write-bytes b out)]))))
      sha256-bytes
      bytes->hex-string))

(define (expect what actual expected)
  (if (equal? actual expected) '() (list (format "~a: expected ~s, actual ~s" what expected actual))))

(define (parity-reference name)
  (hash-ref parity name))

(define (parity-problems name table)
  (define reference (parity-reference name))
  (define digests (hash-ref reference 'columns))
  (define names (dataset-table-names table))
  (append (expect (list name "parity rows") (dataset-table-row-count table) (hash-ref reference 'rows))
          (expect (list name "parity names") (sort (map string->symbol names) symbol<?)
                  (sort (hash-keys digests) symbol<?))
          (append* (for/list ([column (in-list names)])
                     (expect (list name column "parity digest")
                             (column-digest (dataset-table-column table column))
                             (hash-ref digests (string->symbol column) #f))))))
