#lang racket/base
(require (for-syntax racket/base (only-in racket/syntax format-id) (only-in syntax/parse attribute))
         (only-in racket/contract/base ->* any any/c contract-out)
         (only-in syntax/parse/define define-syntax-parse-rule)
         datasets/core
         (only-in datasets/polars table->polars)
         (only-in datasets/matrix table->matrix)
         (only-in datasets/data-frame table->data-frame)
         (only-in datasets/core-private/synthetic-contracts dataset-format/c generator synthetic/c)
         (prefix-in s: datasets/synthetic))
(provide (all-from-out datasets/core)
         (contract-out
          [load-dataset (->* (symbol?) (#:format dataset-format/c #:columns any/c) any)]
          [load-air-passengers loader/c] [load-airquality loader/c] [load-anscombe loader/c]
          [load-breast-cancer loader/c] [load-diabetes loader/c] [load-faithful loader/c]
          [load-iris loader/c] [load-mtcars loader/c] [load-plant-growth loader/c]
          [load-titanic loader/c] [load-tooth-growth loader/c] [load-us-arrests loader/c]
          [load-wine loader/c]))

(define loader/c (->* () (#:format dataset-format/c #:columns any/c) any))

(define (convert-table table format)
  (case format
    [(table) table]
    [(polars) (table->polars table)]
    [(matrix) (table->matrix table)]
    [(data-frame) (table->data-frame table)]))

(define (load-dataset name #:format [format 'polars] #:columns [columns #f])
  (convert-table (load-dataset-table name #:columns columns) format))

(define-syntax-parse-rule (define-synthetic-wrappers g:generator ...)
  #:with (core ...) (for/list ([name (in-list (attribute g))]) (format-id name "s:~a" name))
  (begin
    (provide (contract-out [g (synthetic/c g #:format)] ...))
    (define (g #:format [format 'polars] (~@ g.arg.keyword [g.arg.name g.arg.default]) ...
               #:seed [seed #f])
      (convert-table (core (~@ g.arg.keyword g.arg.name) ... #:seed seed) format))
    ...))

(define-synthetic-wrappers
  make-blobs make-moons make-circles make-classification make-regression make-friedman1
  make-friedman2 make-friedman3 make-hastie-10-2 make-swiss-roll make-s-curve
  make-gaussian-quantiles)
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
