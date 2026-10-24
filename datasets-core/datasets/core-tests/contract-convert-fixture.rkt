#lang racket/base
(require (only-in racket/contract/base -> any/c contract-out))
(provide (contract-out [non-sequence-rows (-> any/c any/c sequence?)]))

(define (non-sequence-rows files info) 'rows)
