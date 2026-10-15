#lang racket/base
(provide (struct-out dataset-table) dataset-missing dataset-missing?)

(struct missing-value ())
(define dataset-missing (missing-value))
(define (dataset-missing? v) (eq? v dataset-missing))
(struct dataset-table (names columns row-count info) #:transparent)
