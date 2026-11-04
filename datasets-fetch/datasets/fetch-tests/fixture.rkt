#lang racket/base
(require (only-in datasets/core-tests/fixture call-with-temporary-fixture gzip-bytes)
         (only-in file/gzip deflate)
         (only-in file/tar tar->output)
         (only-in file/zip zip->output zip-entry)
         (only-in racket/contract/base -> ->* any any/c contract-out hash/c list/c listof non-empty-listof
                  or/c)
         (only-in racket/file file->bytes make-directory*)
         (only-in racket/list append* append-map)
         (only-in racket/match == match match-define match-lambda)
         (only-in racket/path path-only)
         (only-in racket/port call-with-output-bytes open-output-nowhere)
         (only-in racket/string string-contains? string-prefix?))
(provide (contract-out
          [call-with-upstream-files (-> (listof (list/c string? upstream/c))
                                        (-> (hash/c string? complete-path? #:immutable #t) any)
                                        any)]
          [error-with? (-> symbol? string? ... (-> any/c boolean?))]
          [idx-bytes (->* ((listof exact-nonnegative-integer?) bytes?) (#:magic bytes?) bytes?)]
          [tar-bytes (-> (listof member/c) bytes?)]
          [cifar-10-member-names (listof string?)]
          [cifar-100-member-names (listof string?)]
          [cifar-record-bytes (-> (listof (listof byte?)) bytes?)]
          [cifar-10-upstream (-> (listof member/c) list?)]
          [cifar-100-upstream (-> (listof member/c) list?)]
          [emnist-members (-> string? (list/c idx/c idx/c idx/c idx/c) (listof (list/c string? bytes?)))]))

(define member/c (list/c string? (or/c string? bytes?)))
(define idx/c (list/c (listof exact-nonnegative-integer?) bytes?))

(define upstream/c
  (or/c (list/c 'gz string?) (list/c 'tgz (listof (list/c string? (or/c string? bytes?)))) (list/c 'plain string?)
        (list/c 'idx-gz (non-empty-listof exact-nonnegative-integer?) bytes?)
        (list/c 'zip (listof (list/c string? bytes?))) (list/c 'bytes bytes?)))

(define (upstream-bytes upstream)
  (match upstream
    [(list 'plain text) (string->bytes/utf-8 text)]
    [(list 'gz text) (gzip-bytes (string->bytes/utf-8 text))]
    [(list 'tgz members) (gzip-bytes (tar-bytes members))]
    [(list 'idx-gz dims data) (gzip-bytes (idx-bytes dims data))]
    [(list 'zip members) (zip-bytes members)]
    [(list 'bytes data) data]))

(define (idx-bytes dims data #:magic [magic (bytes 0 0 8 (length dims))])
  (apply bytes-append magic (append (map (lambda (d) (integer->integer-bytes d 4 #f #t)) dims) (list data))))

(define ((error-with? who . needles) e)
  (and (exn:fail? e) (not (exn:fail:contract? e))
       (string-prefix? (exn-message e) (format "~a: " who))
       (for/and ([needle (in-list needles)]) (string-contains? (exn-message e) needle))))

(define (tar-bytes members)
  (call-with-temporary-fixture
   (lambda (dir)
     (for ([member (in-list members)])
       (write-member! dir member))
     (parameterize ([current-directory dir])
       (call-with-output-bytes
        (lambda (out)
          (tar->output (map (match-lambda [(list name _) (string->path name)]) members) out
                       #:get-timestamp (lambda (_) 0))))))))

(define (write-member! dir member)
  (match-define (list name content) member)
  (define file (build-path dir name))
  (make-directory* (path-only file))
  (call-with-output-file file
    (lambda (out) (if (bytes? content) (write-bytes content out) (write-string content out)))))

(define stored-level 0)
(define deflated-level 6)

(define (deflated-size data)
  (define-values (_in out _crc) (deflate (open-input-bytes data) (open-output-nowhere)))
  out)

(define (zip-bytes members)
  (call-with-temporary-fixture
   (lambda (dir)
     (define file (build-path dir "members.zip"))
     (call-with-output-file file
       (lambda (out)
         (zip->output (for/list ([member (in-list members)])
                        (match-define (list name data) member)
                        (zip-entry 'file (string->path name) data (bytes-length data) (hash)
                                   (if (< (deflated-size data) (bytes-length data)) deflated-level stored-level)))
                      out
                      #:get-timestamp (lambda (_) 0))))
     (file->bytes file))))

(define (call-with-upstream-files upstream proc)
  (call-with-temporary-fixture
   (lambda (dir)
     (proc (for/hash ([file (in-list upstream)])
             (match file
               [(list name content)
                (define path (build-path dir name))
                (call-with-output-file path (lambda (out) (write-bytes (upstream-bytes content) out)))
                (values name path)]))))))

(define cifar-10-member-names
  (append (for/list ([k (in-range 1 6)]) (format "cifar-10-batches-bin/data_batch_~a.bin" k))
          '("cifar-10-batches-bin/test_batch.bin")))
(define cifar-100-member-names '("cifar-100-binary/train.bin" "cifar-100-binary/test.bin"))

(define (cifar-record-bytes records)
  (apply bytes (append* records)))

(define (in-tar-order names members)
  (append-map (lambda (name) (filter (match-lambda [(list (== name) _) #t] [_ #f]) members)) names))

(define (cifar-10-upstream members)
  (match-define (list batch-1 batch-2 batch-3 batch-4 batch-5 test) cifar-10-member-names)
  `(("cifar-10-binary.tar.gz" (tgz ,(in-tar-order (list batch-3 batch-1 test batch-5 batch-2 batch-4) members)))))

(define (cifar-100-upstream members)
  `(("cifar-100-binary.tar.gz" (tgz ,members))))

(define (emnist-members split contents)
  (for/list ([part (in-list '("train-images-idx3" "train-labels-idx1" "test-images-idx3" "test-labels-idx1"))]
             [content (in-list contents)])
    (match-define (list dims data) content)
    (list (format "gzip/emnist-~a-~a-ubyte.gz" split part) (gzip-bytes (idx-bytes dims data)))))
