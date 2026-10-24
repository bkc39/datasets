#lang racket/base
(require (only-in datasets/core dataset-info dataset-names)
         (only-in racket/contract/base -> any/c contract-out list/c listof)
         (only-in racket/list add-between)
         (only-in racket/match match-define))
(provide (contract-out
          [islr-chapter-rows (-> (listof (list/c string? (listof symbol?) (listof symbol?))))]
          [islr-chapter-cells (-> (-> symbol? any/c) (listof (list/c string? list? list?)))]))

(define chapters
  '("Introduction" "Statistical Learning" "Linear Regression" "Classification"
    "Resampling Methods" "Linear Model Selection and Regularization" "Moving Beyond Linearity"
    "Tree-Based Methods" "Support Vector Machines" "Deep Learning" "Survival Analysis"
    "Unsupervised Learning" "Multiple Testing"))

(define (islr-chapter-rows)
  (define ids (dataset-names #:package "datasets-islr"))
  (define uses (for/list ([id (in-list ids)]) (hash-ref (dataset-info id) "islr2-chapters")))
  (for/list ([title (in-list chapters)] [chapter (in-naturals 1)])
    (define (using part)
      (for/list ([id (in-list ids)] [parts (in-list uses)]
                 #:when (member chapter (hash-ref parts part)))
        id))
    (list (format "~a ~a" chapter title) (using "text") (using "exercises"))))

(define (islr-chapter-cells render)
  (define (names ids) (add-between (map render ids) ", "))
  (for/list ([row (in-list (islr-chapter-rows))])
    (match-define (list chapter text exercises) row)
    (list chapter (names text) (names exercises))))
