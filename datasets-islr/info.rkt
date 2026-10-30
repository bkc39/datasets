#lang info
(define collection 'multi)
(define version "0.1")
(define deps '(("base" #:version "9.3") "datasets-core"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "The datasets of ISLR2 and of The Elements of Statistical Learning")
(define pkg-authors '(bkc))
(define license '((Apache-2.0 OR MIT) AND (GPL-2.0-only AND GPL-2.0-or-later)))
