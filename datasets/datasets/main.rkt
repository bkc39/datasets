#lang racket/base
(require datasets/core datasets/polars datasets/matrix datasets/data-frame
         (only-in racket/contract/base ->* any/c contract-out non-empty-listof or/c))
(define format/c (or/c 'polars 'table 'matrix 'data-frame))
(define columns/c (or/c #f (non-empty-listof string?)))
(define loader/c (->* () (#:format format/c #:columns columns/c) any/c))
(provide (contract-out
          [load-dataset (->* (symbol?)
                             (#:format format/c #:columns columns/c #:download? boolean?)
                             any/c)]
          [load-air-passengers loader/c]
          [load-airquality loader/c]
          [load-anscombe loader/c]
          [load-breast-cancer loader/c]
          [load-diabetes loader/c]
          [load-faithful loader/c]
          [load-iris loader/c]
          [load-mtcars loader/c]
          [load-plant-growth loader/c]
          [load-titanic loader/c]
          [load-tooth-growth loader/c]
          [load-us-arrests loader/c]
          [load-wine loader/c])
         (all-from-out datasets/core))
(define (load-dataset name #:format [format 'polars] #:columns [columns #f]
                      #:download? [download? #t])
  (define table (load-dataset-table name #:columns columns #:download? download?))
  (case format
    [(table) table]
    [(polars) (table->polars table)]
    [(matrix) (table->matrix table)]
    [(data-frame) (table->data-frame table)]))
(define (load-air-passengers #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'air-passengers #:format format #:columns columns))
(define (load-airquality #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'airquality #:format format #:columns columns))
(define (load-anscombe #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'anscombe #:format format #:columns columns))
(define (load-breast-cancer #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'breast-cancer #:format format #:columns columns))
(define (load-diabetes #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'diabetes #:format format #:columns columns))
(define (load-faithful #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'faithful #:format format #:columns columns))
(define (load-iris #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'iris #:format format #:columns columns))
(define (load-mtcars #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'mtcars #:format format #:columns columns))
(define (load-plant-growth #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'plant-growth #:format format #:columns columns))
(define (load-titanic #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'titanic #:format format #:columns columns))
(define (load-tooth-growth #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'tooth-growth #:format format #:columns columns))
(define (load-us-arrests #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'us-arrests #:format format #:columns columns))
(define (load-wine #:format [format 'polars] #:columns [columns #f])
  (load-dataset 'wine #:format format #:columns columns))
