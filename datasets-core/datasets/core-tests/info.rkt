#lang info
(define compile-omit-paths '("fixtures"))
(define test-omit-paths '("arity-convert-fixture.rkt" "contract-convert-fixture.rkt" "fixtures"))
(define test-timeouts
  '(("distribute.rkt" 300) ("fetch-set.rkt" 600) ("fetch.rkt" 300) ("reader.rkt" 600) ("registries.rkt" 600)))
