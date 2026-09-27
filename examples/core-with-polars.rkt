#lang racket/base
;; A downstream consumer depends on datasets-core and polars, never datasets.
(require datasets/core (prefix-in pl: polars))
(define iris (load-dataset-table 'iris))
(define frame
  (pl:dataframe-new
   (for/list ([name (in-list (dataset-table-names iris))])
     (define values (vector->list (dataset-table-column iris name)))
     ((if (string? (car values)) pl:series-new-str pl:series-new-f64) name values))))
(unless (= 150 (pl:dataframe-height frame)) (error 'example "unexpected height"))
(void (pl:~> frame (pl:group-by "species") (pl:agg (pl:mean (pl:col "sepal-length")))))
