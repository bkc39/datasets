#lang racket/base
(require (only-in racket/match match)
         (only-in datasets/core
                  dataset-missing? dataset-table-columns dataset-table-info dataset-table-names)
         (prefix-in pl: (only-in polars dataframe polars-null polars-null? series-head
                                 series-new-f64 series-new-i64 series-new-str)))
(provide table->polars)

(define (empty-series name type)
  (pl:series-head (match type
                    ["real" (pl:series-new-f64 name '(0.0))]
                    ["integer" (pl:series-new-i64 name '(0))]
                    [_ (pl:series-new-str name '(""))])
                  0))

(define (filled-series name column)
  (define values
    (for/list ([v (in-vector column)])
      (if (dataset-missing? v) pl:polars-null v)))
  (define sample (for/first ([v (in-vector column)] #:unless (dataset-missing? v)) v))
  (cond [(string? sample) (pl:series-new-str name values)]
        [(for/and ([v (in-vector column)]) (or (dataset-missing? v) (exact-integer? v)))
         (pl:series-new-i64 name values)]
        [else (pl:series-new-f64 name (map (λ (v) (if (pl:polars-null? v) v (exact->inexact v)))
                                           values))]))

(define (table->polars table)
  (define types
    (for/hash ([c (in-list (hash-ref (dataset-table-info table) "columns"))])
      (values (hash-ref c "name") (hash-ref c "type"))))
  (pl:dataframe
   (for/list ([name (in-list (dataset-table-names table))]
              [column (in-vector (dataset-table-columns table))])
     (cond [(zero? (vector-length column)) (empty-series name (hash-ref types name))]
           [else (filled-series name column)]))))
