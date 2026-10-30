#lang racket/base
(require (only-in pkg/lib pkg-directory)
         (only-in racket/match == match)
         (only-in setup/getinfo get-info/full)
         (only-in version/utils version<=?))
(module+ test
  (require (only-in rackunit check-not-false check-true test-case))
  (define (package-info name) (get-info/full (pkg-directory name)))
  (define (pinned-version package dependency)
    (for/or ([d (in-list ((package-info package) 'deps))])
      (match d
        [(list (== dependency) '#:version version) version]
        [_ #f])))

  (test-case "datasets pins the datasets-core version whose dataset-table it re-exports"
    (define pin (pinned-version "datasets" "datasets-core"))
    (check-not-false pin)
    (check-true (and pin (version<=? ((package-info "datasets-core") 'version) pin)))))
