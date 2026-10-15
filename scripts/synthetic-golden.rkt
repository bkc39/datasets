#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/match match-define)
         (only-in racket/runtime-path define-runtime-path)
         (only-in datasets/core dataset-table-columns dataset-table-names)
         (only-in datasets/core-tests/plist apply/plist))
(define-runtime-path golden "../datasets-core/datasets/core-tests/fixtures/synthetic-golden.rktd")

(define cases
  `((make-blobs (#:n-samples 5 #:seed 0))
    (make-moons (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-circles (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-classification
     (#:n-samples 5 #:n-features 6 #:n-repeated 1 #:shift #f #:scale #f #:seed 0))
    (make-regression
     (#:n-samples 5 #:n-features 4 #:n-informative 2 #:n-targets 2 #:noise 0.1 #:seed 0))
    (make-friedman1 (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-friedman2 (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-friedman3 (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-hastie-10-2 (#:n-samples 5 #:seed 0))
    (make-swiss-roll (#:n-samples 5 #:noise 0.1 #:hole? #t #:seed 0))
    (make-s-curve (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-gaussian-quantiles (#:n-samples 5 #:seed 0))
    (make-classification (#:n-samples 5 #:n-features 4 #:hypercube? #f #:seed 0))
    (make-regression (#:n-samples 5 #:n-features 4 #:effective-rank 2 #:noise 0.1 #:seed 0))
    (make-swiss-roll (#:n-samples 5 #:noise 0.1 #:seed 0))
    (make-classification
     (#:n-samples 3 #:n-features 31 #:n-informative 31 #:n-redundant 0 #:n-clusters-per-class 1
      #:seed 0))))

(define entries
  (for/list ([c (in-list cases)])
    (match-define (list name args) c)
    (define t (apply/plist (dynamic-require 'datasets/synthetic name) args))
    (list name args
          (for/hash ([n (in-list (dataset-table-names t))]
                     [v (in-vector (dataset-table-columns t))])
            (values n (vector->list v))))))

(with-output-to-file golden #:exists 'truncate
  (λ ()
    (displayln "(")
    (for ([e (in-list entries)]) (write e) (newline))
    (displayln ")")))
