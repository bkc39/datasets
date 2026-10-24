#lang info
(define collection 'multi)
(define version "0.1")
(define deps '(("base" #:version "9.3") "datasets-core"))
(define build-deps '("rackunit-lib" "threading-lib"))
(define pkg-desc "Classic machine-learning datasets from the UCI Machine Learning Repository")
(define pkg-authors '(bkc))
(define license '((Apache-2.0 OR MIT) AND CC-BY-4.0))
