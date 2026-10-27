#lang racket/base
(module+ test
  (require (only-in datasets/core dataset-info dataset-missing? dataset-table-column
                    dataset-table-columns dataset-table-row-count load-dataset-table)
           (only-in datasets/r-private/objects r-objects)
           (only-in racket/contract/base -> any contract-out listof)
           (only-in racket/format ~s)
           (only-in racket/list remove-duplicates)
           (only-in racket/match match)
           (only-in racket/vector vector-count)
           (only-in rackunit check-= check-equal? check-regexp-match))
  (provide (contract-out
            [check-spot (-> list? any)]
            [r-dataset-ids (listof symbol?)]))

  (define r-dataset-ids
    (remove-duplicates
     (for*/list ([object (in-list r-objects)]
                 [id (in-value (match object
                                 [(list _ _ (list 'dataset id)) id]
                                 [_ #f]))]
                 #:when (and id (equal? (hash-ref (dataset-info id) "package") "datasets-r")))
       id)))

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

  (define (check-spot spot)
    (define message (~s spot))
    (match spot
      [(list 'shape id rows columns)
       (define t (load-dataset-table id))
       (check-equal? (list (dataset-table-row-count t) (vector-length (dataset-table-columns t)))
                     (list rows columns) message)]
      [(list 'series id frequency start end index)
       (check-equal? (hash-ref (dataset-info id) "time-series")
                     (hash "frequency" frequency "start" start "end" end "index" index) message)]
      [(list 'row id row names expected)
       (check-equal? (for/list ([name (in-list names)]) (vector-ref (column id name) row)) expected
                     message)]
      [(list 'types id (list names types) ...)
       (check-equal? (map (lambda (name) (column-meta id name "type")) names) types message)]
      [(list 'missing id names expected)
       (check-equal? (for/list ([name (in-list names)]) (column-count id name dataset-missing?))
                     expected message)]
      [(list 'sum id name expected) (check-equal? (column-sum id name) expected message)]
      [(list 'sum id name expected epsilon) (check-= (column-sum id name) expected epsilon message)]
      [(list 'level-counts id name expected)
       (check-equal? (for/list ([level (in-list (column-meta id name "levels"))])
                       (column-count id name (lambda (v) (equal? v level))))
                     expected message)]
      [(list 'categorical id expected) (check-equal? (typed-columns id "categorical") expected message)]
      [(list 'identifiers id expected)
       (check-equal? (hash-ref (dataset-info id) "identifiers") expected message)
       (check-equal? (typed-columns id "identifier") expected message)]
      [(list 'meta id name key expected) (check-equal? (column-meta id name key) expected message)]
      [(list 'info id key expected) (check-equal? (hash-ref (dataset-info id) key) expected message)]
      [(list 'info-match id key pattern)
       (check-regexp-match pattern (hash-ref (dataset-info id) key) message)])))
