#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info)
         (only-in racket/file file->value)
         (only-in racket/format ~s)
         (only-in racket/list check-duplicates remove-duplicates)
         (only-in racket/match match match-define)
         (only-in racket/runtime-path define-runtime-path)
         (only-in "spot.rkt" spot-results))
(define-runtime-path r-data "../r-data")
(define-runtime-path spots-file "r-datasets-spots.rktd")
(module+ test
  (require (only-in rackunit check-= check-equal? check-false check-not-false check-regexp-match
                    check-true))
  (define objects (file->value (build-path r-data "r-objects.rktd")))
  (define exceptions
    (hash "BJsales" "bj-sales" "BJsales.lead" "bj-sales" "CO2" "co2-uptake" "DNase" "dnase"
          "UKgas" "uk-gas" "UScitiesD" "us-cities-d" "WWWusage" "www-usage"
          "fdeaths" "uk-lung-deaths" "ldeaths" "uk-lung-deaths" "mdeaths" "uk-lung-deaths"))
  (define r-name-steps
    '((#px"([a-z0-9])([A-Z])" "\\1-\\2") (#px"([A-Z])(?=[A-Z][a-z])" "\\1-")
      (#px"[^A-Za-z0-9]+" "-") (#px"^-+|-+$" "")))
  (define (r-name->id name)
    (string-downcase
     (for/fold ([s name]) ([step (in-list r-name-steps)])
       (match-define (list pattern replacement) step)
       (regexp-replace* pattern s replacement))))
  (define (expected-id name)
    (cond
      [(hash-ref exceptions name #f)]
      [(regexp-match? #rx"^state[.]" name) "state"]
      [else (r-name->id name)]))
  (define dataset-ids
    (remove-duplicates
     (for/list ([object (in-list objects)] #:when (equal? (caddr object) "dataset"))
       (string->symbol (cadddr object)))))

  (check-equal? (map r-name->id '("UKDriverDeaths" "sunspot.year" "state.x77" "Harman23.cor"
                                  "USPersonalExpenditure" "occupationalStatus" "Solar.R"))
                '("uk-driver-deaths" "sunspot-year" "state-x77" "harman23-cor"
                  "us-personal-expenditure" "occupational-status" "solar-r"))
  (check-equal? (length objects) 104)
  (check-false (check-duplicates (map car objects)))
  (for ([object (in-list objects)])
    (match-define (list name class disposition target) object)
    (check-true (string? class) name)
    (match disposition
      ["dataset"
       (define info (dataset-info (string->symbol target)))
       (check-equal? target (expected-id name))
       (check-equal? (hash-ref info "package") "datasets-r")
       (check-equal? (hash-ref info "source-version") "R 4.3.3")
       (check-equal? (hash-ref info "license") "GPL-2.0-only OR GPL-3.0-only")
       (check-regexp-match
        (regexp (string-append "R objects? ([^ ]+, )*" (regexp-quote name) "[,.]"))
        (hash-ref info "notes"))
       (check-true (file-exists? (build-path r-data "provenance" (string-append target ".Rd"))))]
      ["core"
       (check-equal? target (r-name->id name))
       (check-equal? (hash-ref (dataset-info (string->symbol target)) "package") "datasets-core")]
      ["skipped" (check-regexp-match #rx"^[A-Z].*[.]$" target)]))
  (check-equal? (length dataset-ids) 79)

  (define spot-checks (file->value spots-file))

  (check-equal? (sort (remove-duplicates
                       (for/list ([spot (in-list spot-checks)] #:when (eq? (car spot) 'shape))
                         (cadr spot)))
                      symbol<?)
                (sort dataset-ids symbol<?))
  (for* ([spot (in-list spot-checks)] [result (in-list (spot-results spot))])
    (match result
      [(list 'equal actual expected) (check-equal? actual expected (~s spot))]
      [(list 'close actual expected epsilon) (check-= actual expected epsilon (~s spot))]
      [(list 'match actual pattern) (check-regexp-match pattern actual)])))
