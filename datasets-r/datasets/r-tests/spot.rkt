#lang racket/base
(require (only-in datasets/core dataset-info dataset-missing? dataset-table-column
                  dataset-table-columns dataset-table-row-count load-dataset-table)
         (only-in racket/contract/base -> any/c contract-out listof)
         (only-in racket/match match)
         (only-in racket/vector vector-count))
(provide (contract-out
          [column-meta (-> symbol? string? string? any/c)]
          [spot-results (-> list? (listof list?))]
          [typed-columns (-> symbol? string? (listof string?))]))

(define (column id name) (dataset-table-column (load-dataset-table id) name))
(define (column-meta id name key)
  (for/first ([meta (in-list (hash-ref (dataset-info id) "columns"))]
              #:when (equal? (hash-ref meta "name") name))
    (hash-ref meta key)))
(define (column-count id name keep?) (vector-count keep? (column id name)))
(define (column-sum id name)
  (for/sum ([v (in-vector (column id name))] #:unless (dataset-missing? v)) v))
(define (typed-columns id type)
  (for/list ([meta (in-list (hash-ref (dataset-info id) "columns"))]
             #:when (equal? (hash-ref meta "type") type))
    (hash-ref meta "name")))

(define (spot-results spot)
  (match spot
    [(list 'shape id rows columns)
     (define t (load-dataset-table id))
     (list (list 'equal (list (dataset-table-row-count t) (vector-length (dataset-table-columns t)))
                 (list rows columns)))]
    [(list 'series id frequency start end index)
     (list (list 'equal (hash-ref (dataset-info id) "time-series")
                 (hash "frequency" frequency "start" start "end" end "index" index)))]
    [(list 'row id row names expected)
     (list (list 'equal (for/list ([name (in-list names)]) (vector-ref (column id name) row))
                 expected))]
    [(list 'missing id names expected)
     (list (list 'equal (for/list ([name (in-list names)]) (column-count id name dataset-missing?))
                 expected))]
    [(list 'sum id name expected) (list (list 'equal (column-sum id name) expected))]
    [(list 'sum id name expected epsilon) (list (list 'close (column-sum id name) expected epsilon))]
    [(list 'level-counts id name expected)
     (list (list 'equal
                 (for/list ([level (in-list (column-meta id name "levels"))])
                   (column-count id name (lambda (v) (equal? v level))))
                 expected))]
    [(list 'categorical id expected) (list (list 'equal (typed-columns id "categorical") expected))]
    [(list 'identifiers id expected)
     (list (list 'equal (hash-ref (dataset-info id) "identifiers") expected)
           (list 'equal (typed-columns id "identifier") expected))]
    [(list 'meta id name key expected) (list (list 'equal (column-meta id name key) expected))]
    [(list 'info id key expected) (list (list 'equal (hash-ref (dataset-info id) key) expected))]
    [(list 'info-match id key pattern) (list (list 'match (hash-ref (dataset-info id) key) pattern))]))
