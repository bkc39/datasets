#lang info
(define collection 'multi)
(define version "0.1")
(define deps '(("base" #:version "9.3")))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define pkg-desc "Bundled, attributed datasets and immutable Racket tables")
(define pkg-authors '(bkc))
(define license
  '((Apache-2.0 OR MIT)
    AND ((GPL-2.0-only OR GPL-3.0-only) AND (GPL-2.0-only AND CC-BY-4.0))))
