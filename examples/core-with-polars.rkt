#lang racket/base
;; A downstream consumer depends on datasets-core and polars, never datasets.
(require datasets/core (prefix-in pl: polars))
(define iris (load-dataset-table 'iris))
(define frame
  (pl:dataframe
   (for/list ([name (in-list (dataset-table-names iris))])
     (pl:series (dataset-table-column iris name) #:name name))))
(unless (= 150 (pl:dataframe-height frame)) (error 'example "unexpected height"))
(void (pl:~> frame (pl:group-by "species") (pl:agg (pl:mean (pl:col "sepal-length")))))
