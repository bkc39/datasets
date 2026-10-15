#lang racket/base
(require (prefix-in df: (only-in data-frame df-is-na? df-na-value df-ref df-set!))
         (only-in datasets dataset-missing dataset-missing? dataset-names dataset-table-column
                  dataset-table-names dataset-table-row-count load-airquality load-dataset
                  load-dataset-table load-iris)
         (only-in datasets/core-private/table dataset-table)
         (only-in datasets/core-tests/fixture call-with-registry-fixtures call-with-temporary-fixture
                  gen-entry gzip-bytes hex registry-fixture rows->rktd-bytes write-registry-fixture!)
         (only-in datasets/data-frame data-frame-column-names)
         (only-in datasets/polars table->polars)
         (only-in math/matrix matrix-ref)
         (prefix-in pl: (only-in polars column-names dataframe-column dataframe-column-names
                                 dataframe-height dataframe-rename dataframe? polars-null? series-dtype
                                 series-ref shape))
         (only-in racket/list last))
(module+ test
  (require (only-in rackunit check-= check-eq? check-equal? check-exn check-true test-case))
  (define (check-formats)
    (for ([id (in-list (dataset-names #:package "datasets-core"))])
      (define t (load-dataset id #:format 'table))
      (define names (reverse (dataset-table-names t)))
      (define p (load-dataset id #:columns names))
      (define d (load-dataset id #:format 'data-frame #:columns names))
      (check-true (pl:dataframe? p))
      (check-equal? (pl:shape p) (list (dataset-table-row-count t) (length names)))
      (check-equal? (pl:column-names p) names)
      (check-equal? (pl:dataframe-column-names p) names)
      (check-equal? (data-frame-column-names d) names)
      (check-equal? (pl:dataframe-height p) (dataset-table-row-count t))
      (for ([name (in-list names)])
        (define s (pl:dataframe-column p name))
        (for ([v (in-vector (dataset-table-column t name))] [i (in-naturals)])
          (check-equal? (df:df-ref d i name) v)
          (cond [(dataset-missing? v) (check-true (pl:polars-null? (pl:series-ref s i)))]
                [(number? v) (check-= (pl:series-ref s i) v 1e-14)]
                [else (check-equal? (pl:series-ref s i) v)])))
      (define numeric
        (filter (lambda (n) (for/and ([v (in-vector (dataset-table-column t n))]) (number? v))) names))
      (when (pair? numeric)
        (define m (load-dataset id #:format 'matrix #:columns numeric))
        (for* ([i (in-range (dataset-table-row-count t))] [j (in-range (length numeric))])
          (check-equal? (matrix-ref m i j) (vector-ref (dataset-table-column t (list-ref numeric j)) i))))))
  (define (check-loaders)
    (for ([name (in-list (dataset-names #:package "datasets-core"))])
      (define loader (dynamic-require 'datasets (string->symbol (format "load-~a" name))))
      (check-equal? (loader #:format 'table) (load-dataset name #:format 'table))))
  (check-formats)
  (check-loaders)
  (define air (load-airquality #:format 'data-frame))
  (check-eq? (df:df-na-value air "ozone") dataset-missing)
  (check-true (df:df-is-na? air "ozone" (df:df-ref air 4 "ozone")))
  (define d (load-iris #:format 'data-frame))
  (df:df-set! d 0 -100 "sepal-length")
  (check-equal? (df:df-ref (load-iris #:format 'data-frame) 0 "sepal-length") 5.1)
  (define p (load-iris))
  (define renamed (pl:dataframe-rename p "species" "label"))
  (check-equal? (last (pl:dataframe-column-names (load-iris))) "species")
  (check-exn #rx"species" (lambda () (load-iris #:format 'matrix)))
  (check-exn #rx"ozone" (lambda () (load-airquality #:format 'matrix #:columns '("ozone"))))
  (check-exn #rx"format|polars" (lambda () (load-iris #:format 'bad)))
  (check-exn #rx"unknown column" (lambda () (load-iris #:columns '("bad"))))
  (call-with-registry-fixtures (list (registry-fixture "sibling"))
    (lambda ()
      (check-equal? (load-dataset 'toy #:format 'table) (load-dataset-table 'toy))
      (check-equal? (pl:dataframe-height (load-dataset 'toy)) 3)
      (check-formats)
      (check-loaders)))
  (define (dtypes p)
    (map (lambda (c) (pl:series-dtype (pl:dataframe-column p c))) (pl:dataframe-column-names p)))
  (define untyped-rows (hash "nt" '((1 2.5) (2 3.5)) "nz" '()))
  (define untyped-text
    (for/hash ([(name rows) (in-hash untyped-rows)]) (values name (rows->rktd-bytes rows))))
  (test-case "registry columns without \"type\" convert to polars"
    (call-with-temporary-fixture
     (lambda (dir)
       (write-registry-fixture!
        dir #:info '("registry.rktd")
        #:registries (hash "registry.rktd"
                           (for/hash ([(name rows) (in-hash untyped-rows)])
                             (values name
                                     (hash-set* (gen-entry name
                                                           #:sha256 (hex (sha256-bytes
                                                                          (hash-ref untyped-text name))))
                                                "rows" (length rows) "column-count" 2
                                                "columns" (list (hash "name" "a") (hash "name" "b"))))))
        #:data (for/hash ([(name text) (in-hash untyped-text)]) (values name (gzip-bytes text))))
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-equal? (pl:dataframe-height (load-dataset 'nt)) 2)
           (define empty (load-dataset 'nz))
           (check-equal? (pl:dataframe-height empty) 0)
           (check-equal? (dtypes empty) '(string string)))))))
  (test-case "info without \"columns\" converts to polars"
    (define no-columns (table->polars (dataset-table '("a" "b") #(#(1 2) #(2.5 3.5)) 2 (hash))))
    (check-equal? (pl:dataframe-height no-columns) 2)
    (check-equal? (dtypes no-columns) '(int64 float64)))
  (test-case "zero rows and no \"columns\" give empty string columns"
    (define no-columns-empty (table->polars (dataset-table '("a" "b") #(#() #()) 0 (hash))))
    (check-equal? (pl:dataframe-height no-columns-empty) 0)
    (check-equal? (dtypes no-columns-empty) '(string string))))
