#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-names)
         (only-in racket/file file->value)
         (only-in racket/list check-duplicates filter-map remove-duplicates)
         (only-in racket/match match match-define)
         (only-in racket/runtime-path define-runtime-path))
(define-runtime-path r-data "../r-data")
(define-runtime-path spots-file "r-datasets-spots.rktd")
(module+ test
  (require (only-in rackunit check-equal? check-false check-regexp-match check-true)
           (only-in (submod "spot.rkt" test) check-spot r-dataset-ids r-objects))
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
  (define (r-object-row? row)
    (match row
      [(list (list 'tt (? string?)) (? string?) (list 'dataset (? symbol? id)))
       (and (memq id (dataset-names)) #t)]
      [(list (list 'tt (? string?)) (? string?) (? string? reason))
       (regexp-match? #rx"^Skipped[.] [A-Z].*[.]$" reason)]
      [_ #f]))

  (check-equal? (map r-name->id '("UKDriverDeaths" "sunspot.year" "state.x77" "Harman23.cor"
                                  "USPersonalExpenditure" "occupationalStatus" "Solar.R"))
                '("uk-driver-deaths" "sunspot-year" "state-x77" "harman23-cor"
                  "us-personal-expenditure" "occupational-status" "solar-r"))
  (check-equal? (length r-objects) 104)
  (check-equal? (filter (lambda (row) (not (r-object-row? row))) r-objects) '())
  (define object-names
    (for/list ([object (in-list r-objects)])
      (match-define (list (list 'tt name) (? string?) _) object)
      name))
  (check-false (check-duplicates object-names))
  (for ([object (in-list r-objects)] [name (in-list object-names)])
    (match object
      [(list _ _ (list 'dataset id))
       (define info (dataset-info id))
       (define target (symbol->string id))
       (match (hash-ref info "package")
         ["datasets-r"
          (check-equal? target (expected-id name))
          (check-equal? (hash-ref info "source-version") "R 4.3.3")
          (check-equal? (hash-ref info "license") "GPL-2.0-only OR GPL-3.0-only")
          (check-regexp-match
           (regexp (string-append "R objects? ([^ ]+, )*" (regexp-quote name) "[,.]"))
           (hash-ref info "notes"))
          (check-true (file-exists? (build-path r-data "provenance" (string-append target ".Rd"))))]
         [package (check-equal? (list package target) (list "datasets-core" (r-name->id name)))])]
      [_ (void)]))
  (check-equal? (length r-dataset-ids) 79)

  (define spot-checks (file->value spots-file))

  (check-equal? (sort (remove-duplicates
                       (filter-map (lambda (spot)
                                     (match spot
                                       [(list 'shape id _ ...) id]
                                       [_ #f]))
                                   spot-checks))
                      symbol<?)
                (sort r-dataset-ids symbol<?))
  (for-each check-spot spot-checks))
