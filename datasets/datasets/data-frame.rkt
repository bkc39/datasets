#lang racket/base
(require datasets/core data-frame racket/vector)
(provide table->data-frame data-frame-column-names)
(define (data-frame-column-names frame)
  (df-get-property frame 'datasets:columns
                   (lambda () (raise-argument-error 'data-frame-column-names "frame created by table->data-frame" frame))))
(define (table->data-frame table)
  (define frame (make-data-frame))
  (for ([name (in-list (dataset-table-names table))]
        [column (in-vector (dataset-table-columns table))])
    (df-add-series! frame (make-series name #:data (vector-copy column) #:na dataset-missing)))
  (df-put-property! frame 'datasets:columns (dataset-table-names table))
  frame)
