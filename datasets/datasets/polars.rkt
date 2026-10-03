#lang racket/base
(require datasets/core (prefix-in pl: polars))
(provide table->polars)
(define (table->polars table)
  (pl:dataframe
   (for/list ([name (in-list (dataset-table-names table))]
              [column (in-vector (dataset-table-columns table))])
     (define values
       (for/list ([v (in-vector column)])
         (if (dataset-missing? v) pl:polars-null v)))
     (define sample (for/first ([v (in-vector column)] #:unless (dataset-missing? v)) v))
     (define constructor
       (cond [(string? sample) pl:series-new-str]
             [(for/and ([v (in-vector column)]) (or (dataset-missing? v) (exact-integer? v)))
              pl:series-new-i64]
             [else pl:series-new-f64]))
     (constructor name
                  (if (eq? constructor pl:series-new-f64)
                      (map (lambda (v) (if (pl:polars-null? v) v (exact->inexact v))) values)
                      values)))))
