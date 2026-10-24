#lang info
(define collection 'multi)
(define version "0.1")
(define deps '(("base" #:version "9.3") ("datasets-core" #:version "0.2")))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Datasets converted locally from upstream files on first use")
(define pkg-authors '(bkc))
(define license '(Apache-2.0 OR MIT))
