#lang racket/base
(require (only-in racket/contract/base -> contract-out non-empty-listof)
         (only-in racket/match match match*)
         (only-in racket/port call-with-output-bytes copy-port make-limited-input-port))
(provide (contract-out
          [read-idx (-> input-port? (values (non-empty-listof exact-positive-integer?) bytes?))]))

(define (read-idx in)
  (define magic (bytes->list (read-exactly-incrementally in 4 "magic number")))
  (define dims
    (match magic
      [(list 0 0 8 rank)
       (for/list ([_ (in-range rank)])
         (integer-bytes->integer (read-exactly-incrementally in 4 "dimension") #f #t))]
      [_ #f]))
  (match* (magic dims)
    [((list 0 0 type _) #f) (error 'read-idx "unsupported element type\n  type: ~a" type)]
    [(_ #f) (error 'read-idx "not an IDX file\n  magic: ~a" magic)]
    [(_ '()) (error 'read-idx "no dimensions")]
    [(_ (list _ ... 0 _ ...)) (error 'read-idx "zero dimension\n  dimensions: ~a" dims)]
    [(_ _) (values dims (read-idx-data in dims))]))

(define (read-idx-data in dims)
  (define data (read-exactly-incrementally in (apply * dims) "data"))
  (cond
    [(eof-object? (peek-byte in)) data]
    [else (error 'read-idx "trailing data\n  dimensions: ~a" dims)]))

(define (read-exactly-incrementally in n what)
  (define data
    (call-with-output-bytes (lambda (out) (copy-port (make-limited-input-port in n #f) out))))
  (cond
    [(= (bytes-length data) n) data]
    [else (error 'read-idx "short ~a\n  expected bytes: ~a\n  given bytes: ~a" what n (bytes-length data))]))
