#lang info
(define collection 'multi)
(define version "0.1")
(define deps '(("base" #:version "9.3") "datasets-core" "polars" "math-lib" "data-frame"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define pkg-desc "Bundled, attributed datasets and dataframe adapters")
(define pkg-authors '(bkc))
(define license '(Apache-2.0 OR MIT))
