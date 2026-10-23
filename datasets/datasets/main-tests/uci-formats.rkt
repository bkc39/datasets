#lang racket/base
(require (prefix-in df: (only-in data-frame df-is-na? df-ref df-row-count df-select))
         (only-in datasets dataset-info dataset-missing? dataset-names dataset-table-column
                  dataset-table-names dataset-table-row-count load-dataset)
         (only-in datasets/data-frame data-frame-column-names)
         (only-in math/matrix matrix-num-cols matrix-num-rows matrix-ref)
         (prefix-in pl: (only-in polars dataframe-column dataframe-column-names polars-null?
                                 series-null-count series-ref shape))
         (only-in racket/vector vector-count))
(module+ test
  (require (only-in rackunit check-= check-equal? check-exn check-true))
  (define ids (dataset-names #:package "datasets-uci"))
  (check-equal? (length ids) 18)
  (check-equal? (pl:shape (load-dataset 'digits)) '(1797 65))
  (check-equal? (hash-ref (dataset-info 'digits) "suggested-targets") '("digit"))

  (for ([id (in-list ids)])
    (define label (symbol->string id))
    (define table (load-dataset id #:format 'table))
    (define frame (load-dataset id))
    (define df (load-dataset id #:format 'data-frame))
    (define rows (dataset-table-row-count table))
    (define names (dataset-table-names table))
    (define columns (hash-ref (dataset-info id) "columns"))
    (check-equal? (pl:shape frame) (list rows (length names)) label)
    (check-equal? (pl:dataframe-column-names frame) names)
    (check-equal? (df:df-row-count df) rows)
    (check-equal? (data-frame-column-names df) names)
    (for ([meta (in-list columns)])
      (define name (hash-ref meta "name"))
      (define column (dataset-table-column table name))
      (define series (pl:dataframe-column frame name))
      (define missing (hash-ref meta "missing-count"))
      (check-equal? (pl:series-null-count series) missing (format "~a ~a" id name))
      (check-equal? (vector-count (lambda (v) (df:df-is-na? df name v)) (df:df-select df name))
                    missing)
      (for ([row (in-list (list 0 (quotient rows 2) (sub1 rows)))])
        (define value (vector-ref column row))
        (define polars (pl:series-ref series row))
        (check-equal? (df:df-ref df row name) value)
        (cond [(dataset-missing? value) (check-true (pl:polars-null? polars))]
              [(number? value) (check-= polars value 0)]
              [else (check-equal? polars value)])))
    (define numeric
      (for/list ([meta (in-list columns)]
                 #:when (and (member (hash-ref meta "type") '("integer" "real"))
                             (zero? (hash-ref meta "missing-count"))))
        (hash-ref meta "name")))
    (cond
      [(null? numeric) (check-exn exn:fail? (lambda () (load-dataset id #:format 'matrix)))]
      [else
       (define matrix (load-dataset id #:format 'matrix #:columns numeric))
       (check-equal? (list (matrix-num-rows matrix) (matrix-num-cols matrix))
                     (list rows (length numeric))
                     label)
       (for ([row (in-list (list 0 (sub1 rows)))])
         (for ([name (in-list numeric)] [j (in-naturals)])
           (check-equal? (matrix-ref matrix row j)
                         (vector-ref (dataset-table-column table name) row))))])))
