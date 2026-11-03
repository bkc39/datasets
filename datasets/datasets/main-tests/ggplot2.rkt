#lang racket/base
(module+ test
  (require (only-in datasets dataset-info load-dataset)
           (only-in datasets/fetch-tests/digest parity-problems parity-reference)
           (only-in (submod datasets/r-tests/spot test) ggplot2-ids)
           (only-in rackunit check-equal?))

  (check-equal? (hash-ref (parity-reference 'versions) 'ggplot2) "4.0.3")
  (for ([id (in-list ggplot2-ids)])
    (check-equal? (hash-ref (dataset-info id) "source-version") "ggplot2 4.0.3" (symbol->string id))
    (check-equal? (parity-problems id (load-dataset id #:format 'table)) '() (symbol->string id))))
