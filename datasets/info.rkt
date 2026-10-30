#lang info
(define collection 'multi)
(define version "0.2")
(define deps
  '(("base" #:version "9.3") ("datasets-core" #:version "0.3") "datasets-r" "datasets-uci" "datasets-islr" "datasets-fetch"
    "polars"
    "math-lib" "data-frame"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc" "math-doc" "plot-lib" "threading-lib"))
(define pkg-desc "Bundled, attributed datasets and dataframe adapters")
(define pkg-authors '(bkc))
(define license '(Apache-2.0 OR MIT))
