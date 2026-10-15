#lang racket/base
(require (only-in racket/flonum fl* fl+ fl- flcos fllog flsin flsqrt)
         (only-in racket/math pi)
         (only-in racket/random crypto-random-bytes))
(provide seed->generator seeded uniform uniform-in random-index normal-source permutation
         shuffled-order)

;; Stream version 1. Any change to how a value below is drawn from the generator
;; changes every generated table and must bump the `stream` number in the metadata.

(define seed-limit 2147483648)

(define (seeded k)
  (define g (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator g]) (random-seed k))
  g)

(define (fresh-seed)
  (modulo (integer-bytes->integer (crypto-random-bytes 4) #f) seed-limit))

(define (seed->generator seed)
  (cond [(pseudo-random-generator? seed)
         (values seed (vector->immutable-vector (pseudo-random-generator->vector seed)))]
        [(vector? seed)
         (values (vector->pseudo-random-generator seed) (vector->immutable-vector seed))]
        [else (define k (or seed (fresh-seed)))
              (values (seeded k) k)]))

(define (uniform g) (random g))
(define (uniform-in g a b) (fl+ a (fl* (fl- b a) (random g))))
(define (random-index g k) (random k g))

(define (normal-source g)
  (define spare #f)
  (λ ()
    (cond [spare (begin0 spare (set! spare #f))]
          [else (define r (flsqrt (fl* -2.0 (fllog (random g)))))
                (define θ (fl* (fl* 2.0 pi) (random g)))
                (set! spare (fl* r (flsin θ)))
                (fl* r (flcos θ))])))

(define (permutation g n)
  (define p (build-vector n values))
  (for ([i (in-range (sub1 n) 0 -1)])
    (define j (random (add1 i) g))
    (define x (vector-ref p i))
    (vector-set! p i (vector-ref p j))
    (vector-set! p j x))
  p)

(define (shuffled-order g shuffle? n) (if shuffle? (permutation g n) (build-vector n values)))
