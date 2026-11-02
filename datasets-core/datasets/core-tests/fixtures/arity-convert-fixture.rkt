#lang racket/base
(require (only-in racket/contract/base -> any/c contract-out))
(provide (contract-out [one-argument-rows (-> any/c any/c sequence?)]))

(define (one-argument-rows files) '())
