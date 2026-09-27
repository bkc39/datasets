#lang racket/base
(require datasets/core math/matrix)
(provide table->matrix)
(define (table->matrix table)
  (for ([name (in-list (dataset-table-names table))]
        [column (in-vector (dataset-table-columns table))])
    (for ([v (in-vector column)] [row (in-naturals)])
      (unless (and (number? v) (not (dataset-missing? v)))
        (raise-arguments-error 'table->matrix "requires numeric, nonmissing values"
                               "column" name "row (zero-based)" row "value" v))))
  (build-matrix (dataset-table-row-count table) (length (dataset-table-names table))
                (lambda (i j) (vector-ref (vector-ref (dataset-table-columns table) j) i))))
