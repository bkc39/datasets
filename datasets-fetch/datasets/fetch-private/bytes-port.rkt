#lang racket/base
(require (only-in racket/contract/base -> ->* contract-out))
(provide (contract-out
          [bytes->input-port (->* (bytes?) (exact-nonnegative-integer?) (values input-port? (-> boolean?)))]))

(define (bytes->input-port data [start 0])
  (define size (bytes-length data))
  (define position (box start))
  (define end-seen? (box #f))
  (define (copy! buffer skip)
    (define from (min size (+ (unbox position) skip)))
    (define n (min (bytes-length buffer) (- size from)))
    (bytes-copy! buffer 0 data from (+ from n))
    (set-box! end-seen? (or (unbox end-seen?) (zero? n)))
    n)
  (define (result n) (if (zero? n) eof n))
  (values (make-input-port 'bytes
                           (lambda (buffer)
                             (define n (copy! buffer 0))
                             (set-box! position (+ (unbox position) n))
                             (result n))
                           (lambda (buffer skip _progress) (result (copy! buffer skip)))
                           void)
          (lambda () (unbox end-seen?))))
