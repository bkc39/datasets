#lang racket/base
(require (only-in racket/contract/base -> and/c flat-contract-predicate flat-named-contract hash/c
                  or/c))
(provide convert-fetch? converter/c row-value?)

(define converter-files/c
  (flat-named-contract 'converter-files (hash/c string? complete-path? #:immutable #t)))
(define converter-info/c (flat-named-contract 'dataset-info (and/c hash? immutable?)))
(define converter/c (-> converter-files/c converter-info/c sequence?))
(define row-value/c
  (flat-named-contract 'row-value (or/c exact-integer? flonum? string? 'missing)))
(define row-value? (flat-contract-predicate row-value/c))

(define (convert-fetch? fetch)
  (and (hash? fetch) (equal? (hash-ref fetch "format" #f) "convert")))
