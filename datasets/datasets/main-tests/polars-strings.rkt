#lang racket/base
(require (only-in datasets dataset-missing? dataset-table-column dataset-table-names
                  dataset-table-row-count load-dataset)
         (only-in datasets/polars table->polars)
         (prefix-in pl: (only-in polars dataframe-column polars-null? series-ref)))

(define (collection-count)
  (define stats (make-vector 12 0))
  (vector-set-performance-stats! stats)
  (vector-ref stats 3))

(define (call-with-minor-collections thunk)
  (define before (collection-count))
  (define collector
    (thread (λ ()
              (let loop ()
                (collect-garbage 'minor)
                (sleep 0)
                (loop)))))
  (define result (dynamic-wind void thunk (λ () (kill-thread collector))))
  (values result (- (collection-count) before)))

(define (same-cell? cell value)
  (cond [(dataset-missing? cell) (pl:polars-null? value)]
        [else (equal? value cell)]))

(module+ test
  (require (only-in rackunit check-equal? check-true))
  (define table (load-dataset 'adult #:format 'table))
  (define rows (dataset-table-row-count table))
  (define string-columns
    (for/list ([name (in-list (dataset-table-names table))]
               #:when (for/or ([v (in-vector (dataset-table-column table name))]) (string? v)))
      name))
  (check-true (>= (length string-columns) 8))
  (for ([trial (in-range 4)])
    (define-values (frame collections)
      (call-with-minor-collections (λ () (table->polars table))))
    (check-true (positive? collections))
    (for ([name (in-list string-columns)])
      (define column (dataset-table-column table name))
      (define series (pl:dataframe-column frame name))
      (define wrong
        (for/sum ([row (in-range rows)]
                  #:unless (same-cell? (vector-ref column row) (pl:series-ref series row)))
          1))
      (check-equal? wrong 0 (format "trial ~a, column ~a" trial name)))))
