#lang racket/base
(require (only-in datasets/core dataset-info))
(provide column-meta column-names)

(define (columns id) (hash-ref (dataset-info id) "columns"))

(define (column-names id keep?)
  (for/list ([meta (in-list (columns id))] #:when (keep? meta)) (hash-ref meta "name")))

(define (column-meta id name key)
  (hash-ref (findf (lambda (meta) (equal? (hash-ref meta "name") name)) (columns id)) key))
