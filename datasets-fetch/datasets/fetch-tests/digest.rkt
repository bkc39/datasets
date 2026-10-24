#lang racket/base
(require (only-in datasets/core dataset-missing?)
         (only-in file/sha1 bytes->hex-string)
         (only-in racket/contract/base -> contract-out)
         (only-in racket/match match)
         (only-in racket/port call-with-output-bytes))
(provide (contract-out [column-digest (-> vector? string?)]))

(define (column-digest column)
  (bytes->hex-string
   (sha256-bytes
    (call-with-output-bytes
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
            (write-bytes b out)])))))))
