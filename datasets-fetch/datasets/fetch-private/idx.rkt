#lang racket/base
(require (only-in racket/contract/base -> contract-out non-empty-listof)
         (only-in racket/match match)
         (only-in racket/port call-with-output-bytes copy-port make-limited-input-port))
(provide (contract-out
          [read-idx (-> input-port? (values (non-empty-listof exact-positive-integer?) bytes?))]))

(define (read-idx in)
  (match (bytes->list (read-exactly in 4 "magic number"))
    [(list 0 0 8 0) (error 'read-idx "no dimensions")]
    [(list 0 0 8 rank)
     (define dims
       (for/list ([k (in-range rank)])
         (integer-bytes->integer (read-exactly in 4 "dimension") #f #t)))
     (define data (read-exactly in (apply * dims) "data"))
     (cond
       [(memv 0 dims) (error 'read-idx "zero dimension\n  dimensions: ~a" dims)]
       [(eof-object? (peek-byte in)) (values dims data)]
       [else (error 'read-idx "trailing data\n  dimensions: ~a" dims)])]
    [(list 0 0 type _) (error 'read-idx "unsupported element type\n  type: ~a" type)]
    [magic (error 'read-idx "not an IDX file\n  magic: ~a" magic)]))

(define (read-exactly in n what)
  (define data
    (call-with-output-bytes (lambda (out) (copy-port (make-limited-input-port in n #f) out))))
  (cond
    [(= (bytes-length data) n) data]
    [else (error 'read-idx "short ~a\n  expected bytes: ~a\n  given bytes: ~a" what n (bytes-length data))]))
