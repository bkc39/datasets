#lang racket/base
(module+ test
  (require (only-in datasets dataset-info dataset-table-column load-dataset)
           (only-in datasets/fetch-private/convert csv-header-rows)
           (only-in datasets/fetch-tests/digest parity-problems parity-reference)
           (only-in racket/sequence sequence->list)
           (only-in racket/set for/set list->set set-member?)
           (only-in rackunit check-equal? check-true)
           (only-in threading ~>))
  (define (column id name)
    (~> id (load-dataset #:format 'table) (dataset-table-column name)))
  (define flights-info (dataset-info 'nycflights13-flights))
  (define flights
    (let ([rows (~> (collection-file-path "flights-mini.csv" "datasets" "fetch-tests" "fixtures")
                    (hash "flights.csv" _)
                    (csv-header-rows flights-info)
                    sequence->list)])
      (for/hash ([c (in-list (hash-ref flights-info "columns"))]
                 [column-values (in-list (apply map list rows))])
        (values (hash-ref c "name") column-values))))
  (define carriers (list->set (vector->list (column 'nycflights13-airlines "carrier"))))
  (define airports (list->set (vector->list (column 'nycflights13-airports "faa"))))
  (define weather-hours
    (for/set ([origin (in-vector (column 'nycflights13-weather "origin"))]
              [time-hour (in-vector (column 'nycflights13-weather "time-hour"))])
      (list origin time-hour)))

  (check-equal? (hash-ref (parity-reference 'versions) 'nycflights13) "1.0.2")
  (for ([id (in-list '(nycflights13-airlines nycflights13-airports nycflights13-planes
                                             nycflights13-weather))])
    (check-equal? (parity-problems id (load-dataset id #:format 'table)) '()))

  (check-equal? (length (hash-ref flights "carrier")) 4)
  (for ([carrier (in-list (hash-ref flights "carrier"))]
        [origin (in-list (hash-ref flights "origin"))]
        [dest (in-list (hash-ref flights "dest"))]
        [time-hour (in-list (hash-ref flights "time-hour"))])
    (check-true (set-member? carriers carrier) carrier)
    (check-true (set-member? airports origin) origin)
    (check-true (set-member? airports dest) dest)
    (check-true (set-member? weather-hours (list origin time-hour)) (format "~a ~a" origin time-hour))))
