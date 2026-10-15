#lang racket/base
(require (only-in math/matrix matrix-num-cols matrix-num-rows matrix-ref)
         (only-in racket/match match-define)
         datasets
         (only-in datasets/data-frame data-frame-column-names)
         (prefix-in df: (only-in data-frame df-ref df-row-count))
         (prefix-in pl: (only-in polars ~> agg col dataframe-column dataframe-column-names
                                 dataframe-height dataframe-sort group-by head mean polars-null?
                                 series-ref shape)))
(module+ test
  (require rackunit)
  (define cases
    '((penguins 344 8 ("species" 0 "Adelie") ("bill-length-mm" 343 50.2) ("year" 343 2009)
                ("flipper-length-mm" 0 181))
      (penguins-raw 344 17 ("individual-id" 0 "N1A1") ("delta-13-c" 1 -24.69454)
                    ("body-mass-g" 343 3775))
      (gapminder 1704 6 ("country" 1703 "Zimbabwe") ("life-exp" 0 28.801) ("pop" 0 8425333))
      (datasaurus-dozen 1846 3 ("dataset" 0 "dino") ("x" 0 55.3846) ("y" 1845 89.5889019877))
      (titanic-passengers 891 12 ("name" 0 "Braund, Mr. Owen Harris") ("age" 0 22)
                          ("fare" 1 71.2833) ("ticket" 890 "370376"))))
  (define missing-cells
    '((penguins "bill-length-mm" 3) (penguins "sex" 3) (penguins-raw "comments" 1)
      (titanic-passengers "age" 5) (titanic-passengers "cabin" 0)))
  (define matrix-columns
    '((penguins ("year")) (penguins-raw ("sample-number"))
      (gapminder ("year" "life-exp" "pop" "gdp-percap")) (datasaurus-dozen ("x" "y"))
      (titanic-passengers ("passenger-id" "survived" "pclass" "sib-sp" "parch" "fare"))))
  (define (polars-value frame name row)
    (pl:series-ref (pl:dataframe-column frame name) row))

  (for ([c (in-list cases)])
    (match-define (list* id rows cols cells) c)
    (define table (load-dataset id #:format 'table))
    (define frame (load-dataset id))
    (define df (load-dataset id #:format 'data-frame))
    (check-equal? (dataset-table-row-count table) rows)
    (check-equal? (length (dataset-table-names table)) cols)
    (check-equal? (pl:shape frame) (list rows cols))
    (check-equal? (pl:dataframe-column-names frame) (dataset-table-names table))
    (check-equal? (df:df-row-count df) rows)
    (check-equal? (data-frame-column-names df) (dataset-table-names table))
    (for ([cell (in-list cells)])
      (match-define (list name row value) cell)
      (check-equal? (vector-ref (dataset-table-column table name) row) value)
      (check-equal? (df:df-ref df row name) value)
      (define polars (polars-value frame name row))
      (cond [(number? value) (check-= polars value 0)]
            [else (check-equal? polars value)])))

  (for ([m (in-list missing-cells)])
    (match-define (list id name row) m)
    (check-true (dataset-missing? (vector-ref (dataset-table-column (load-dataset id #:format 'table) name) row)))
    (check-eq? (df:df-ref (load-dataset id #:format 'data-frame) row name) dataset-missing)
    (check-true (pl:polars-null? (polars-value (load-dataset id) name row))))

  (for ([m (in-list matrix-columns)])
    (match-define (list id columns) m)
    (define table (load-dataset id #:format 'table #:columns columns))
    (define matrix (load-dataset id #:format 'matrix #:columns columns))
    (check-equal? (list (matrix-num-rows matrix) (matrix-num-cols matrix))
                  (list (dataset-table-row-count table) (length columns)))
    (for ([name (in-list columns)] [j (in-naturals)])
      (check-equal? (matrix-ref matrix 0 j) (vector-ref (dataset-table-column table name) 0))))

  (check-exn #rx"bill-length-mm" (lambda () (load-dataset 'penguins #:format 'matrix
                                                         #:columns '("bill-length-mm"))))
  (check-exn #rx"species" (lambda () (load-dataset 'penguins #:format 'matrix)))

  (define penguins (load-dataset 'penguins))
  (check-equal? (pl:shape (pl:head penguins 4)) '(4 8))
  (check-true (pl:polars-null? (polars-value (pl:head penguins 4) "body-mass-g" 3)))
  (define by-species
    (pl:~> penguins
           (pl:group-by "species")
           (pl:agg (pl:mean (pl:col "body-mass-g")))
           (pl:dataframe-sort '("species"))))
  (check-equal? (for/list ([row (in-range (pl:dataframe-height by-species))])
                  (polars-value by-species "species" row))
                '("Adelie" "Chinstrap" "Gentoo"))
  (for ([row (in-range 3)] [mean (in-list '(3700.662251655629 3733.0882352941176 5076.016260162602))])
    (check-= (polars-value by-species "body-mass-g" row) mean 1e-9)))
