#lang racket/base
(require (for-syntax racket/base racket/syntax syntax/parse)
         (only-in racket/contract/base contract-out)
         (only-in syntax/parse/define define-syntax-parse-rule)
         datasets/core datasets/polars datasets/matrix datasets/data-frame
         (only-in datasets/core-private/synthetic-contracts synthetic/c)
         (prefix-in s: datasets/synthetic))
(provide (all-from-out datasets/core) load-dataset
         load-air-passengers load-airquality load-anscombe load-breast-cancer load-diabetes load-faithful load-iris load-mtcars load-plant-growth load-titanic load-tooth-growth load-us-arrests load-wine)
(define (convert-table table format)
  (case format
    [(table) table]
    [(polars) (table->polars table)]
    [(matrix) (table->matrix table)]
    [(data-frame) (table->data-frame table)]))
(define (load-dataset name #:format [format 'polars] #:columns [columns #f])
  (unless (memq format '(polars table matrix data-frame))
    (raise-argument-error 'load-dataset "(or/c 'polars 'table 'matrix 'data-frame)" format))
  (convert-table (load-dataset-table name #:columns columns) format))

(define (format-wrapper name generate)
  (define-values (_ keywords) (procedure-keywords generate))
  (procedure-reduce-keyword-arity
   (make-keyword-procedure
    (λ (kws vals)
      (define fmt (for/first ([k (in-list kws)] [v (in-list vals)] #:when (eq? k '#:format)) v))
      (define pairs (for/list ([k (in-list kws)] [v (in-list vals)] #:unless (eq? k '#:format)) (cons k v)))
      (convert-table (keyword-apply generate (map car pairs) (map cdr pairs) '()) (or fmt 'polars))))
   0 '() (sort (cons '#:format keywords) keyword<?) name))

(define-syntax-parse-rule (define-synthetic-wrappers generator:id ...)
  #:with (core ...) (for/list ([g (in-list (attribute generator))]) (format-id g "s:~a" g))
  (begin
    (provide (contract-out [generator (synthetic/c generator #:format)] ...))
    (define generator (format-wrapper 'generator core)) ...))

(define-synthetic-wrappers
  make-blobs make-moons make-circles make-classification make-regression make-friedman1
  make-friedman2 make-friedman3 make-hastie-10-2 make-swiss-roll make-s-curve make-gaussian-quantiles)
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
