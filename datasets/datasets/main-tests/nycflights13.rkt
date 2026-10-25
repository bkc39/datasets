#lang racket/base
(require (only-in datasets dataset-info dataset-table-column dataset-table-names
                  dataset-table-row-count load-dataset)
         (only-in datasets/fetch-tests/digest column-digest)
         (only-in json read-json)
         (only-in racket/match match-define))
(module+ test
  (require (only-in rackunit check-equal? check-not-false))
  (define parity
    (call-with-input-file (collection-file-path "parity.json" "datasets" "fetch-tests" "fixtures")
      read-json))
  (define (column-type id name)
    (for/first ([c (in-list (hash-ref (dataset-info id) "columns"))]
                #:when (equal? (hash-ref c "name") name))
      (hash-ref c "type")))

  (check-equal? (hash-ref (hash-ref parity 'versions) 'nycflights13) "1.0.2")
  (for ([id (in-list '(nycflights13-airlines nycflights13-airports nycflights13-planes
                                             nycflights13-weather))])
    (define table (load-dataset id #:format 'table))
    (define reference (hash-ref parity id))
    (check-equal? (dataset-table-row-count table) (hash-ref reference 'rows) (symbol->string id))
    (check-equal? (sort (map string->symbol (dataset-table-names table)) symbol<?)
                  (sort (hash-keys (hash-ref reference 'columns)) symbol<?)
                  (symbol->string id))
    (for ([name (in-list (dataset-table-names table))])
      (check-equal? (column-digest (dataset-table-column table name))
                    (hash-ref (hash-ref reference 'columns) (string->symbol name))
                    (format "~a ~a" id name))))

  (for ([key (in-list '((nycflights13-flights "carrier" nycflights13-airlines "carrier")
                        (nycflights13-flights "tailnum" nycflights13-planes "tailnum")
                        (nycflights13-flights "origin" nycflights13-airports "faa")
                        (nycflights13-flights "dest" nycflights13-airports "faa")
                        (nycflights13-flights "origin" nycflights13-weather "origin")
                        (nycflights13-flights "time-hour" nycflights13-weather "time-hour")))])
    (match-define (list left left-name right right-name) key)
    (for ([type (in-list (list (column-type left left-name) (column-type right right-name)))])
      (check-not-false (member type '("string" "identifier")) (format "~s" key)))))
