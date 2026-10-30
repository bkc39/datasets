#lang racket/base
(require (only-in pkg/lib pkg-directory)
         (only-in racket/file find-files)
         (only-in racket/match == match)
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
  (define (imports-release-binding? form)
    (match form
      [(list 'only-in module names ...)
       (define released (cond [(assq module release-bindings) => cdr] [else '()]))
       (for/or ([n (in-list names)]) (and (memq n released) #t))]
      [(cons a d) (or (imports-release-binding? a) (imports-release-binding? d))]
      [_ #f]))
  (define (uses-release? package)
    (for/or ([file (in-list (find-files (λ (p) (regexp-match? #rx"[.]rkt$" p))
                                        (pkg-directory package)))])
      (imports-release-binding? (module-forms file))))
  (define dependents (filter uses-release? packages))

  (test-case "the scan finds the packages that import the dataset-table release"
    (check-equal? dependents '("datasets" "datasets-fetch")))

  (for ([package (in-list dependents)])
    (test-case (format "~a pins a datasets-core that has dataset-table" package)
      (define pin (pinned-version package "datasets-core"))
      (check-not-false pin)
      (check-true (and pin (version<=? dataset-table-release pin))))))
