#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-missing dataset-missing? dataset-names
                  dataset-packages dataset-table-columns dataset-table-names
                  dataset-table-row-count load-dataset-table)
         (only-in datasets/core-tests/fixture gunzip-file hex)
         (only-in racket/file file->lines file->value)
         (only-in racket/format ~s)
         (only-in racket/list append-map make-list remove-duplicates)
         (only-in racket/match match match-define)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/vector vector-count)
         (only-in setup/getinfo get-info/full)
         (only-in "spot.rkt" spot-results))
(define-runtime-path r-data "../r-data")
(define-runtime-path package-root "../..")
(module+ test
  (require (only-in rackunit check-= check-equal? check-false check-not-false check-regexp-match
                    check-true))
  (define penguins-names
    '("species" "island" "bill-length-mm" "bill-depth-mm" "flipper-length-mm" "body-mass-g" "sex"
      "year"))
  (define gapminder-names '("country" "continent" "year" "life-exp" "pop" "gdp-percap"))
  (define datasaurus-names '("dataset" "x" "y"))
  (define titanic-names
    '("passenger-id" "survived" "pclass" "name" "sex" "age" "sib-sp" "parch" "ticket" "fare" "cabin"
      "embarked"))
  (define shapes
    `((datasaurus-dozen 1846 ,datasaurus-names)
      (gapminder 1704 ,gapminder-names)
      (penguins 344 ,penguins-names)
      (penguins-raw 344 ("study-name" "sample-number" "species" "region" "island" "stage"
                         "individual-id" "clutch-completion" "date-egg" "culmen-length-mm"
                         "culmen-depth-mm" "flipper-length-mm" "body-mass-g" "sex"
                         "delta-15-n" "delta-13-c" "comments"))
      (titanic-passengers 891 ,titanic-names)))
  (define licenses
    (hash 'datasaurus-dozen "MIT" 'gapminder "CC0-1.0" 'penguins "CC0-1.0"
          'penguins-raw "CC0-1.0" 'titanic-passengers "CC0-1.0"))
  (define r-ids
    (remove-duplicates
     (for/list ([object (in-list (file->value (build-path r-data "r-objects.rktd")))]
                #:when (equal? (caddr object) "dataset"))
       (string->symbol (cadddr object)))))
  (define ids (sort (append (map car shapes) r-ids) symbol<?))
  (define (data-file id) (build-path r-data "data" (format "~a.rktd.gz" id)))
  (define (column-type name levels identifiers present)
    (cond
      [(member name identifiers) "identifier"]
      [(pair? levels) "categorical"]
      [(andmap exact-integer? present) "integer"]
      [(andmap real? present) "real"]
      [else "string"]))
  (define (key-values lines)
    (for*/hash ([line (in-list lines)]
                [pair (in-value (regexp-match #px"^([^:]+): (.*)$" line))]
                #:when pair)
      (match-define (list _ key value) pair)
      (values key value)))
  (define (license-ids expression)
    (match expression
      [(list left (or 'AND 'OR) right) (append (license-ids left) (license-ids right))]
      [(? symbol?) (list expression)]))
  (define (conjuncts expression)
    (match expression
      [(list left 'AND right) (append (conjuncts left) (conjuncts right))]
      [_ (list expression)]))
  (define (registry-expression license)
    (match (read (open-input-string (string-append "(" license ")")))
      [(list single) single]
      [expression expression]))
  (define spot-checks
    `((categorical datasaurus-dozen ("dataset"))
      (categorical gapminder ("country" "continent"))
      (categorical penguins ("species" "island" "sex"))
      (categorical penguins-raw
                   ("study-name" "species" "region" "island" "stage" "clutch-completion" "sex"))
      (categorical titanic-passengers ("sex" "embarked"))
      (identifiers datasaurus-dozen ())
      (identifiers gapminder ())
      (identifiers penguins ())
      (identifiers penguins-raw ())
      (identifiers titanic-passengers ("passenger-id"))
      (row penguins 0 ,penguins-names ("Adelie" "Torgersen" 39.1 18.7 181 3750 "male" 2007))
      (row penguins 3 ("bill-length-mm" "bill-depth-mm" "flipper-length-mm" "body-mass-g" "sex")
           ,(make-list 5 dataset-missing))
      (row penguins 343 ("species" "bill-length-mm") ("Chinstrap" 50.2))
      (missing penguins ("bill-length-mm" "sex") (2 11))
      (sum penguins "body-mass-g" 1437000)
      (level-counts penguins "species" (152 68 124))
      (meta penguins "species" "levels" ("Adelie" "Chinstrap" "Gentoo"))
      (info penguins "suggested-targets" ("species"))
      (row penguins-raw 0 ("study-name" "date-egg" "comments")
           ("PAL0708" "2007-11-11" "Not enough blood for isotopes."))
      (row penguins-raw 1 ("delta-15-n" "delta-13-c") (8.94956 -24.69454))
      (row penguins-raw 343 ("individual-id" "species")
           ("N100A2" "Chinstrap penguin (Pygoscelis antarctica)"))
      (missing penguins-raw ("delta-15-n" "delta-13-c" "comments") (14 13 290))
      (meta penguins-raw "delta-15-n" "original-name" "Delta 15 N (o/oo)")
      (meta penguins-raw "study-name" "type" "categorical")
      (meta penguins-raw "study-name" "levels" ("PAL0708" "PAL0809" "PAL0910"))
      (row gapminder 0 ,gapminder-names ("Afghanistan" "Asia" 1952 28.801 8425333 779.4453145))
      (row gapminder 1703 ,gapminder-names ("Zimbabwe" "Africa" 2007 43.487 12311143 469.7092981))
      (sum gapminder "pop" 50440465801)
      (level-counts gapminder "country" ,(make-list 142 12))
      (meta gapminder "continent" "levels" ("Africa" "Americas" "Asia" "Europe" "Oceania"))
      (info gapminder "suggested-targets" ("life-exp"))
      (row datasaurus-dozen 0 ,datasaurus-names ("dino" 55.3846 97.1795))
      (row datasaurus-dozen 1845 ,datasaurus-names ("wide_lines" 34.5838289299 89.5889019877))
      (sum datasaurus-dozen "x" 100174.47351619641 1e-6)
      (level-counts datasaurus-dozen "dataset" ,(make-list 13 142))
      (info-match datasaurus-dozen "citation"
                  #rx"^Gillespie, C\\., Locke, S\\., Davies, R\\. and D'Agostino McGowan, L\\. \\(2025\\)")
      (row titanic-passengers 0 ,titanic-names
           (1 0 3 "Braund, Mr. Owen Harris" "male" 22 1 0 "A/5 21171" 7.25 ,dataset-missing "S"))
      (row titanic-passengers 1 ("cabin" "fare") ("C85" 71.2833))
      (row titanic-passengers 890 ("name" "ticket") ("Dooley, Mr. Patrick" "370376"))
      (missing titanic-passengers ("age" "cabin" "embarked") (177 687 2))
      (sum titanic-passengers "survived" 342)
      (sum titanic-passengers "fare" 28693.9493 1e-6)
      (info titanic-passengers "suggested-targets" ("survived"))
      (meta titanic-passengers "passenger-id" "original-name" "PassengerId")))

  (check-equal? (dataset-names #:package "datasets-r") ids)
  (check-not-false (member "datasets-r" (dataset-packages)))
  (for ([shape (in-list shapes)])
    (match-define (list id rows names) shape)
    (define t (load-dataset-table id))
    (check-equal? (dataset-table-row-count t) rows)
    (check-equal? (dataset-table-names t) names)
    (check-equal? (hash-ref (dataset-info id) "license") (hash-ref licenses id)))
  (for ([id (in-list ids)])
    (define t (load-dataset-table id))
    (define info (dataset-info id))
    (define rows (hash-ref info "rows"))
    (check-equal? (dataset-table-row-count t) rows)
    (check-equal? (dataset-table-names t)
                  (for/list ([meta (in-list (hash-ref info "columns"))]) (hash-ref meta "name")))
    (check-equal? (hash-ref info "column-count") (vector-length (dataset-table-columns t)))
    (check-equal? (hash-ref info "tier") "bundled")
    (check-equal? (hash-ref info "package") "datasets-r")
    (check-false (hash-ref info "fetch"))
    (check-equal? (hex (sha256-bytes (gunzip-file (data-file id)))) (hash-ref info "sha256"))
    (define header (call-with-input-file (data-file id) (lambda (in) (read-bytes 8 in))))
    (check-equal? (subbytes header 0 4) (bytes #x1f #x8b 8 0))
    (check-equal? (subbytes header 4 8) (bytes 0 0 0 0))
    (check-true (immutable? info))
    (for ([c (in-vector (dataset-table-columns t))] [meta (in-list (hash-ref info "columns"))])
      (check-true (immutable? c))
      (check-equal? (vector-length c) rows)
      (check-equal? (vector-count dataset-missing? c) (hash-ref meta "missing-count"))
      (define present (for/list ([v (in-vector c)] #:unless (dataset-missing? v)) v))
      (define type (hash-ref meta "type"))
      (check-equal? type
                    (column-type (hash-ref meta "name") (hash-ref meta "levels")
                                 (hash-ref info "identifiers") present)
                    (hash-ref meta "name"))
      (check-true (or (andmap string? present) (not (member type '("categorical" "string"))))
                  (hash-ref meta "name")))
    (for ([c (in-vector (dataset-table-columns t))] [meta (in-list (hash-ref info "columns"))]
          #:when (pair? (hash-ref meta "levels")))
      (check-equal? (sort (remove-duplicates
                           (for/list ([v (in-vector c)] #:unless (dataset-missing? v)) v))
                          string<?)
                    (sort (hash-ref meta "levels") string<?))))
  (check-equal? (sort (map path->string (directory-list (build-path r-data "data"))) string<?)
                (sort (for/list ([id (in-list ids)]) (format "~a.rktd.gz" id)) string<?))

  (define license-files (directory-list (build-path r-data "licenses")))
  (match-define (list _ 'AND data-license) ((get-info/full package-root) 'license))
  (define registry-licenses
    (remove-duplicates
     (for/list ([id (in-list ids)]) (registry-expression (hash-ref (dataset-info id) "license")))))
  (check-equal? (registry-expression "GPL-2.0-only OR GPL-3.0-only") '(GPL-2.0-only OR GPL-3.0-only))
  (check-equal? (registry-expression "MIT") 'MIT)
  (check-equal? (license-ids '(CC0-1.0 AND (MIT OR GPL-2.0-only))) '(CC0-1.0 MIT GPL-2.0-only))
  (check-equal? (conjuncts '(CC0-1.0 AND (MIT AND (A OR B)))) '(CC0-1.0 MIT (A OR B)))
  (check-equal? (sort (map ~s registry-licenses) string<?)
                (sort (map ~s (conjuncts data-license)) string<?))
  (for ([license (in-list (remove-duplicates
                            (map symbol->string (append-map license-ids registry-licenses))))])
    (define file-name (pregexp (string-append "^" (regexp-quote license) "[-.]")))
    (check-not-false (for/or ([file (in-list license-files)])
                       (regexp-match? file-name (path->string file)))
                     license))
  (check-equal? (key-values '("YEAR: 2022" "" "no separator" "COPYRIGHT HOLDER: someone"))
                (hash "YEAR" "2022" "COPYRIGHT HOLDER" "someone"))
  (define datasaurus-copyright
    (key-values (file->lines (build-path r-data "provenance" "datasauRus-LICENSE"))))
  (check-not-false (member (format "Copyright (c) ~a ~a" (hash-ref datasaurus-copyright "YEAR")
                                   (hash-ref datasaurus-copyright "COPYRIGHT HOLDER"))
                           (file->lines (build-path r-data "licenses" "MIT-datasauRus.txt"))))

  (for* ([spot (in-list spot-checks)] [result (in-list (spot-results spot))])
    (match result
      [(list 'equal actual expected) (check-equal? actual expected (~s spot))]
      [(list 'close actual expected epsilon) (check-= actual expected epsilon (~s spot))]
      [(list 'match actual pattern) (check-regexp-match pattern actual)])))
