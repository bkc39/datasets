#lang racket/base
(require (only-in racket/format ~a)
         (only-in racket/match match))
(provide raise-registry-error)

(define (raise-registry-error who message . fields)
  (define details
    (let loop ([fields fields])
      (match fields
        ['() '()]
        [(list* label value more)
         (list* "\n  " label ": " (if (path? value) (path->string value) (~a value))
                (loop more))])))
  (raise (exn:fail (apply string-append (~a who) ": " message details)
                   (current-continuation-marks))))
