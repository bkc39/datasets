#lang racket/base
(require (only-in racket/contract/base contract-out)
         (only-in datasets/core-private/synthetic-contracts synthetic/c))
(provide
 (contract-out
  [make-blobs (synthetic/c make-blobs)]
  [make-moons (synthetic/c make-moons)]
  [make-circles (synthetic/c make-circles)]
  [make-classification (synthetic/c make-classification)]
  [make-regression (synthetic/c make-regression)]
  [make-friedman1 (synthetic/c make-friedman1)]
  [make-friedman2 (synthetic/c make-friedman2)]
  [make-friedman3 (synthetic/c make-friedman3)]
  [make-hastie-10-2 (synthetic/c make-hastie-10-2)]
  [make-swiss-roll (synthetic/c make-swiss-roll)]
  [make-s-curve (synthetic/c make-s-curve)]
  [make-gaussian-quantiles (synthetic/c make-gaussian-quantiles)]))

(define (make-blobs #:n-samples [n-samples 100] #:n-features [n-features #f] #:centers [centers #f]
                    #:cluster-std [cluster-std 1.0] #:center-box [center-box '(-10.0 10.0)]
                    #:shuffle? [shuffle? #t] #:seed [seed #f])
  (error 'make-blobs "unimplemented"))
(define (make-moons #:n-samples [n-samples 100] #:shuffle? [shuffle? #t] #:noise [noise #f]
                    #:seed [seed #f])
  (error 'make-moons "unimplemented"))
(define (make-circles #:n-samples [n-samples 100] #:shuffle? [shuffle? #t] #:noise [noise #f]
                      #:factor [factor 0.8] #:seed [seed #f])
  (error 'make-circles "unimplemented"))
(define (make-classification #:n-samples [n-samples 100] #:n-features [n-features 20]
                             #:n-informative [n-informative 2] #:n-redundant [n-redundant 2]
                             #:n-repeated [n-repeated 0] #:n-classes [n-classes 2]
                             #:n-clusters-per-class [n-clusters-per-class 2] #:weights [weights #f]
                             #:flip-y [flip-y 0.01] #:class-sep [class-sep 1.0]
                             #:hypercube? [hypercube? #t] #:shift [shift 0.0] #:scale [scale 1.0]
                             #:shuffle? [shuffle? #t] #:seed [seed #f])
  (error 'make-classification "unimplemented"))
(define (make-regression #:n-samples [n-samples 100] #:n-features [n-features 100]
                         #:n-informative [n-informative 10] #:n-targets [n-targets 1]
                         #:bias [bias 0.0] #:effective-rank [effective-rank #f]
                         #:tail-strength [tail-strength 0.5] #:noise [noise 0.0]
                         #:shuffle? [shuffle? #t] #:seed [seed #f])
  (error 'make-regression "unimplemented"))
(define (make-friedman1 #:n-samples [n-samples 100] #:n-features [n-features 10] #:noise [noise 0.0]
                        #:seed [seed #f])
  (error 'make-friedman1 "unimplemented"))
(define (make-friedman2 #:n-samples [n-samples 100] #:noise [noise 0.0] #:seed [seed #f])
  (error 'make-friedman2 "unimplemented"))
(define (make-friedman3 #:n-samples [n-samples 100] #:noise [noise 0.0] #:seed [seed #f])
  (error 'make-friedman3 "unimplemented"))
(define (make-hastie-10-2 #:n-samples [n-samples 12000] #:seed [seed #f])
  (error 'make-hastie-10-2 "unimplemented"))
(define (make-swiss-roll #:n-samples [n-samples 100] #:noise [noise 0.0] #:hole? [hole? #f]
                         #:seed [seed #f])
  (error 'make-swiss-roll "unimplemented"))
(define (make-s-curve #:n-samples [n-samples 100] #:noise [noise 0.0] #:seed [seed #f])
  (error 'make-s-curve "unimplemented"))
(define (make-gaussian-quantiles #:mean [mean #f] #:cov [cov 1.0] #:n-samples [n-samples 100]
                                 #:n-features [n-features #f] #:n-classes [n-classes 3]
                                 #:shuffle? [shuffle? #t] #:seed [seed #f])
  (error 'make-gaussian-quantiles "unimplemented"))
