#lang racket/base
(module+ test
  (require (only-in datasets dataset-info load-dataset)
           (only-in datasets/fetch-tests/digest parity-problems parity-reference)
           (only-in rackunit check-equal?))
  (define ids
    '(diamonds economics economics-long faithfuld luv-colours midwest mpg msleep presidential seals
               txhousing))

  (check-equal? (hash-ref (parity-reference 'versions) 'ggplot2) "4.0.3")
  (for ([id (in-list ids)])
    (check-equal? (hash-ref (dataset-info id) "source-version") "ggplot2 4.0.3" (symbol->string id))
    (check-equal? (parity-problems id (load-dataset id #:format 'table)) '() (symbol->string id))))
