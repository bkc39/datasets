#lang racket/base
(require (prefix-in df: (only-in data-frame df-is-na? df-na-value df-ref))
         (only-in datasets
                  dataset-info dataset-missing dataset-names dataset-table dataset-table-columns
                  dataset-table-info dataset-table-names dataset-table-row-count load-dataset-table
                  make-blobs make-circles make-regression)
         (only-in datasets/core-tests/fixture column-types datum->cells rebuild)
         (only-in datasets/data-frame data-frame-column-names table->data-frame)
         (only-in datasets/matrix table->matrix)
         (only-in datasets/polars table->polars)
         (only-in math/matrix matrix->list*)
         (prefix-in pl: (only-in polars dataframe-column dataframe-column-names series-dtype
                                 series-ref shape))
         (only-in racket/list append-map)
         (only-in racket/match match match-define))
(module+ test
  (require (only-in rackunit check-eq? check-equal? check-not-false check-true test-case))
  (define (rows t) (dataset-table-row-count t))
  (define (view format t)
    (match format
      ['table t]
      ['polars
       (define p (table->polars t))
       (define names (pl:dataframe-column-names p))
       (list (pl:shape p) names
             (for/list ([n (in-list names)]) (pl:series-dtype (pl:dataframe-column p n)))
             (for/list ([n (in-list names)])
               (define s (pl:dataframe-column p n))
               (for/list ([i (in-range (rows t))]) (pl:series-ref s i))))]
      ['data-frame
       (define d (table->data-frame t))
       (define names (data-frame-column-names d))
       (list names
             (for/list ([n (in-list names)])
               (for/list ([i (in-range (rows t))]) (df:df-ref d i n))))]
      ['matrix
       (with-handlers ([exn:fail:contract? exn-message]) (matrix->list* (table->matrix t)))]))
  (define compatible
    (hash "integer" '("integer") "real" '("real" "integer") "categorical" '("string")
          "string" '("string") "identifier" '("string" "integer")))
  (define bundled
    (for/list ([name (in-list (dataset-names))]
               #:when (equal? (hash-ref (dataset-info name) "tier" #f) "bundled"))
      name))
  (define loaded
    (append-map (λ (name)
                  (define t (load-dataset-table name))
                  (list t (load-dataset-table name #:columns (reverse (dataset-table-names t)))))
                bundled))
  (define formats '(table polars data-frame matrix))

  (define generated
    (list (make-circles #:format 'table #:n-samples 0 #:seed 0)
          (make-blobs #:format 'table #:n-samples 5 #:seed 0)
          (make-regression #:format 'table #:n-samples 4 #:n-features 2 #:n-targets 3 #:seed 0)))
  (define (label t)
    (format "~a/~a" (hash-ref (dataset-table-info t) "name") (car (dataset-table-names t))))

  (check-true (pair? bundled))
  (for* ([t (in-list (append loaded generated))]
         [f (in-list (if (zero? (rows t)) '(table polars data-frame) formats))])
    (test-case (format "TF1 exact: a rebuilt ~a is indistinguishable as ~a" (label t) f)
      (check-equal? (view f (rebuild t)) (view f t))))

  (test-case "TF2 exact: the polars dtype follows the derived type"
    (for ([row (in-list `((() int64) ((missing missing) int64) ((1 2) int64) ((1 2.5) float64)
                          ((+nan.0) float64) ((missing 1.0) float64) (("a" missing) string)
                          ((,(expt 2 62)) int64)))])
      (match-define (list column dtype) row)
      (define p (table->polars (dataset-table '("c") (list (datum->cells column)))))
      (check-equal? (pl:series-dtype (pl:dataframe-column p "c")) dtype (format "~s" column))))

  (test-case "TF3 exact: derived types agree with every bundled registry"
    (for ([name (in-list bundled)])
      (define t (load-dataset-table name))
      (define derived (column-types (dataset-table (dataset-table-names t)
                                                   (vector->list (dataset-table-columns t)))))
      (for ([c (in-list (hash-ref (dataset-info name) "columns"))] [d (in-list derived)])
        (check-not-false (member d (hash-ref compatible (hash-ref c "type")))
                         (format "~a/~a: ~a as ~a" name (hash-ref c "name") (hash-ref c "type") d)))))

  (test-case "TF4 exact: a rebuilt airquality keeps dataset-missing as the data-frame NA"
    (define d (table->data-frame (rebuild (load-dataset-table 'airquality))))
    (check-true (df:df-is-na? d "ozone" (df:df-ref d 4 "ozone")))
    (check-eq? (df:df-na-value d "ozone") dataset-missing)))
