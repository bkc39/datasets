#lang racket/base
(require (only-in pkg/lib pkg-directory)
         (only-in racket/file find-files)
         (only-in racket/list append-map)
         (only-in racket/match == match match-define)
         (only-in setup/getinfo get-info/full)
         (only-in version/utils version<=?))
(module+ test
  (require (only-in rackunit check-equal? check-not-false check-true test-case))
  (define dataset-table-release "0.3")
  (define release-bindings
    '((datasets/core dataset-table)
      (datasets/core-tests/fixture column-types datum->cells rebuild)))
  (define packages '("datasets" "datasets-fetch" "datasets-islr" "datasets-r" "datasets-uci"))
  (define (package-info name) (get-info/full (pkg-directory name)))
  (define (pinned-version package dependency)
    (for/or ([d (in-list ((package-info package) 'deps))])
      (match d
        [(list (== dependency) '#:version version) version]
        [_ #f])))
  (define (module-forms file)
    (parameterize ([read-accept-reader #t] [read-accept-lang #t])
      (call-with-input-file file read)))
  (define release-modules (map car release-bindings))
  (define release-symbols (append-map cdr release-bindings))
  (define (mentions-release? form)
    (match form
      [(list 'only-in (? symbol?) names ...) (mentions-release? names)]
      [(cons a d) (or (mentions-release? a) (mentions-release? d))]
      [(? symbol?) (and (or (memq form release-modules) (memq form release-symbols)) #t)]
      [_ #f]))
  (define (scanned-file? path)
    (regexp-match? #rx"[.](rkt|scrbl)$" path))
  (define (uses-release? package)
    (for/or ([file (in-list (find-files scanned-file? (pkg-directory package)))])
      (mentions-release? (module-forms file))))
  (define dependents (filter uses-release? packages))

  (for ([row (in-list
               '([(require datasets/core) #t]
                 [(require (only-in datasets/core dataset-table)) #t]
                 [(require (only-in datasets/core [dataset-table table])) #t]
                 [(require (rename-in datasets/core [dataset-table table])) #t]
                 [(require (prefix-in core: datasets/core)) #t]
                 [(require (except-in datasets/core dataset-names)) #t]
                 [(require datasets/core-tests/fixture) #t]
                 [(require (only-in datasets/core-tests/fixture rebuild)) #t]
                 [(module m racket/base (module+ test (require (for-label datasets/core)))) #t]
                 [(module doc scribble/manual "Build one with " (racket dataset-table) ".") #t]
                 [(namespace-require 'datasets/core) #t]
                 [(require (only-in datasets/core dataset-info load-dataset-table)) #f]
                 [(require (only-in datasets/core-tests/fixture gunzip-file hex)) #f]
                 [(require (only-in datasets/core)) #f]
                 [(module m racket/base (define t (load-dataset-table 'iris))) #f]))])
    (match-define (list form expected) row)
    (test-case (format "the scan reads ~s as ~a" form (if expected "a release import" "no release import"))
      (check-equal? (mentions-release? form) expected)))

  (test-case "the scan reads Racket modules and Scribble documents"
    (check-equal? (map scanned-file? '("a/core.rkt" "a/guide.scrbl" "a/registry.rktd" "a/info.rkt~"))
                  '(#t #t #f #f)))

  (test-case "the scan finds the packages that import the dataset-table release"
    (check-equal? dependents '("datasets" "datasets-fetch")))

  (for ([package (in-list dependents)])
    (test-case (format "~a pins a datasets-core that has dataset-table" package)
      (define pin (pinned-version package "datasets-core"))
      (check-not-false pin)
      (check-true (and pin (version<=? dataset-table-release pin))))))
