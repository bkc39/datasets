#lang racket/base
(require (prefix-in df: (only-in data-frame df-is-na? df-na-value df-ref df-set!))
         (only-in datasets dataset-missing dataset-missing? dataset-names
                  dataset-table-column dataset-table-names dataset-table-row-count
                  load-air-passengers load-airquality load-anscombe load-breast-cancer load-dataset
                  load-dataset-table load-diabetes load-faithful load-iris load-mtcars
                  load-plant-growth load-titanic load-tooth-growth load-us-arrests load-wine)
         (only-in datasets/core-tests/fixture cache-files call-with-artifact-url
                  call-with-fetch-fixture call-with-registry-fixtures fetch-entry registry-fixture
                  table-digest toy-artifact toy-table-digest)
         (only-in datasets/data-frame data-frame-column-names)
         (only-in math/matrix matrix-ref)
         (prefix-in pl: (only-in polars column-names dataframe-column dataframe-column-names
                                 dataframe-height dataframe-rename dataframe? polars-null? series-ref
                                 shape))
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
  (define loaders
    (hash 'air-passengers load-air-passengers 'airquality load-airquality 'anscombe load-anscombe
          'breast-cancer load-breast-cancer 'diabetes load-diabetes 'faithful load-faithful
          'iris load-iris 'mtcars load-mtcars 'plant-growth load-plant-growth
          'titanic load-titanic 'tooth-growth load-tooth-growth 'us-arrests load-us-arrests
          'wine load-wine))
  (define (check-loaders)
    (check-equal? (sort (hash-keys loaders) symbol<?) (dataset-names #:package "datasets-core"))
    (for ([(name loader) (in-hash loaders)])
      (check-equal? (loader #:format 'table) (load-dataset name #:format 'table))
      (check-exn #rx"blaming: [^\n]*formats\\.rkt" (lambda () (loader #:format 'bad)))))
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
  (test-case "M1 load-dataset passes #:download? through; blame names the caller"
    (call-with-artifact-url toy-artifact
      (lambda (url)
        (call-with-fetch-fixture (list (fetch-entry (list url)))
          (lambda (root registry)
            (check-exn #rx"reason: #:download\\? #f"
                       (lambda () (load-dataset 'toy-remote #:format 'table #:download? #f)))
            (check-equal? (cache-files root) '())
            (check-equal? (table-digest (load-dataset 'toy-remote #:format 'table))
                          toy-table-digest)
            (check-equal? (pl:dataframe-height (load-dataset 'toy-remote)) 3)))))
    (check-exn #rx"blaming: [^\n]*formats\\.rkt" (lambda () (load-dataset 'iris #:download? 1)))))
