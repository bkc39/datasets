#lang racket/base
(require (only-in file/gunzip gunzip-through-ports)
         (only-in racket/port call-with-output-bytes))
(provide gunzip-file hex)

(define (gunzip-file path)
  (call-with-input-file path
    (lambda (in) (call-with-output-bytes (lambda (out) (gunzip-through-ports in out))))))

(define (hex bs)
  (apply string-append
         (for/list ([b (in-bytes bs)])
           (string-append (if (< b 16) "0" "") (number->string b 16)))))
