#lang racket/base
;; Writes datasets-core/datasets/core-tests/fixtures/synthetic-golden.rktd, the stream-1
;; golden values. Regenerate only when the stream version is bumped:
;;   racket scripts/synthetic-golden.rkt
(require racket/match racket/runtime-path datasets/core)
(define-runtime-path golden "../datasets-core/datasets/core-tests/fixtures/synthetic-golden.rktd")

(define cases
  `((make-blobs (#:n-samples 5 #:seed 0))
    (make-moons (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-circles (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-classification (#:n-samples 5 #:n-features 6 #:n-repeated 1 #:shift #f #:scale #f #:seed 0))
    (make-regression (#:n-samples 5 #:n-features 4 #:n-informative 2 #:n-targets 2 #:noise 0.1 #:seed 0))
    (make-friedman1 (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-friedman2 (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-friedman3 (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-hastie-10-2 (#:n-samples 5 #:seed 0))
    (make-swiss-roll (#:n-samples 5 #:noise 0.1 #:hole? #t #:seed 0))
    (make-s-curve (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-gaussian-quantiles (#:n-samples 5 #:seed 0))))

(define (call name plist)
  (define pairs
    (sort (let loop ([p plist])
            (match p ['() '()] [(list* k v rest) (cons (cons k v) (loop rest))]))
          keyword<? #:key car))
  (keyword-apply (dynamic-require 'datasets/synthetic name) (map car pairs) (map cdr pairs) '()))

(define entries
  (for/list ([c (in-list cases)])
    (match-define (list name args) c)
    (define t (call name args))
    (list name args
          (for/hash ([n (in-list (dataset-table-names t))] [v (in-vector (dataset-table-columns t))])
            (values n (vector->list v))))))

(with-output-to-file golden #:exists 'truncate
  (λ ()
    (displayln "(")
    (for ([e (in-list entries)]) (write e) (newline))
    (displayln ")")))
