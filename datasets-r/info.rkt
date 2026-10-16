#lang info
(define collection 'multi)
(define version "0.1")
(define deps '(("base" #:version "9.3") "datasets-core"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Teaching datasets from CRAN packages: palmerpenguins, gapminder, datasauRus, titanic")
(define pkg-authors '(bkc))
(define license '((Apache-2.0 OR MIT) AND (CC0-1.0 AND MIT)))
