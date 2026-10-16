#lang racket/base
(require (only-in racket/match match))
(provide apply/plist)

(define (apply/plist f plist)
  (define table
    (let loop ([p plist])
      (match p
        ['() (hasheq)]
        [(list* k v rest) (hash-set (loop rest) k v)])))
  (define keywords (sort (hash-keys table) keyword<?))
  (keyword-apply f keywords (map (λ (k) (hash-ref table k)) keywords) '()))
