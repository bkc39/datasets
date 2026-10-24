#lang racket/base
(require (only-in datasets dataset-names)
         (only-in datasets/main-doc/islr-chapters islr-chapter-cells islr-chapter-rows)
         (only-in racket/list append-map remove-duplicates)
         (only-in racket/match match-lambda))
(module+ test
  (require (only-in rackunit check-equal?))
  (define rows (islr-chapter-rows))
  (define used
    (append-map (match-lambda [(list _ text exercises) (append text exercises)]) rows))
  (check-equal? (length rows) 13)
  (check-equal? (list-ref rows 1) '("2 Statistical Learning" (auto) (auto boston college)))
  (check-equal? (list-ref rows 2)
                '("3 Linear Regression" (auto boston carseats credit) (auto boston carseats)))
  (check-equal? (list-ref rows 11) '("12 Unsupervised Learning" (nci60) ()))
  (check-equal? (list-ref (islr-chapter-cells symbol->string) 8)
                '("9 Support Vector Machines" ("khan") ("auto" ", " "oj")))
  (check-equal? (sort (remove-duplicates used) symbol<?) (dataset-names #:package "datasets-islr")))
