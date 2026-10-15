#lang racket/base
(require (only-in compiler/distribute assemble-distribution)
         (only-in compiler/embed create-embedding-executable)
         (only-in racket/file find-files make-directory*)
         (only-in racket/port open-output-nowhere)
         (only-in "fixture.rkt" call-with-temporary-fixture))
(module+ test
  (require (only-in rackunit check-equal? check-pred test-case))
  (test-case "raco exe + raco distribute keep the data directory beside the registry"
    (call-with-temporary-fixture
     (lambda (dir)
       (define main (build-path dir "main.rkt"))
       (define exe (build-path dir "build" "main"))
       (define dist (build-path dir "dist"))
       (make-directory* (build-path dir "build"))
       (call-with-output-file main
         (lambda (out) (write '(module main racket/base (require datasets/core) (dataset-names)) out)))
       (parameterize ([current-namespace (make-base-namespace)])
         (create-embedding-executable exe #:modules (list (list #f main))))
       (parameterize ([current-output-port (open-output-nowhere)])
         (assemble-distribution dist (list exe)))
       (define registries
         (find-files (lambda (p) (regexp-match? #rx"(^|/)registry\\.rktd$" (path->string p))) dist))
       (check-equal? (length registries) 1)
       (for ([registry (in-list registries)])
         (define-values (registry-dir _name _dir?) (split-path registry))
         (check-pred file-exists? (build-path registry-dir "data" "iris.rktd.gz")))))))
