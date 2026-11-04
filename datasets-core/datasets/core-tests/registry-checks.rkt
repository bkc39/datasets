#lang racket/base
(module+ test
  (require (only-in datasets/core dataset-info dataset-missing dataset-missing? dataset-names
                    dataset-packages dataset-table-columns dataset-table-names
                    dataset-table-row-count load-dataset-table)
           (only-in racket/contract/base ->* and/c contract-out listof or/c)
           (only-in racket/list append-map check-duplicates remove-duplicates take)
           (only-in racket/match match match-define)
           (only-in racket/vector vector-count)
           (only-in rackunit check-equal? check-false check-not-false check-true
                    current-check-around exn:test:check?)
           (only-in setup/getinfo get-info/full)
           (only-in "fixture.rkt" conjuncts gunzip-file hex license-ids))
  (provide (contract-out
            [check-bundled-registry
             (->* (string? (and/c path? complete-path?) (listof symbol?))
                  (#:types (or/c 'inferred 'declared))
                  void?)]))

  (define (inferred-type name levels identifiers present)
    (cond
      [(member name identifiers) "identifier"]
      [(pair? levels) "categorical"]
      [(andmap exact-integer? present) "integer"]
      [(andmap flonum? present) "real"]
      [else "string"]))

  (define (declared-value? type levels)
    (match type
      ["integer" exact-integer?]
      ["real" real?]
      ["categorical" (lambda (v) (and (member v levels) #t))]
      [_ string?]))

  (define (check-column info column meta types label)
    (define name (hash-ref meta "name"))
    (define type (hash-ref meta "type"))
    (define levels (hash-ref meta "levels"))
    (define present (for/list ([v (in-vector column)] #:unless (dataset-missing? v)) v))
    (define missing (vector-count dataset-missing? column))
    (define identifier?
      (or (equal? type "identifier") (and (member name (hash-ref info "identifiers")) #t)))
    (define where (format "~a ~a" label name))
    (check-true (immutable? column) where)
    (check-equal? (vector-length column) (hash-ref info "rows") where)
    (check-equal? missing (hash-ref meta "missing-count") where)
    (check-true (or (andmap string? present) (not (member type '("categorical" "string")))) where)
    (check-equal? (null? levels) (not (equal? type "categorical")) where)
    (check-false (and identifier? (or (positive? missing) (check-duplicates present))) where)
    (cond
      [(eq? types 'inferred)
       (check-equal? type (inferred-type name levels (hash-ref info "identifiers") present) where)
       (check-true (or (andmap exact-integer? present) (andmap flonum? present)
                       (not (member type '("integer" "real"))))
                   where)
       (check-true (or (null? levels)
                       (equal? (sort (remove-duplicates present) string<?) (sort levels string<?)))
                   where)]
      [else (check-true (andmap (declared-value? type levels) present) where)]))

  (define (check-dataset package data-dir id types)
    (define label (symbol->string id))
    (define info (dataset-info id))
    (define data-file (build-path data-dir "data" (format "~a.rktd.gz" id)))
    (define header (call-with-input-file data-file (lambda (in) (read-bytes 8 in))))
    (check-equal? (hex (sha256-bytes (gunzip-file data-file))) (hash-ref info "sha256") label)
    (check-equal? (subbytes header 0 4) (bytes #x1f #x8b 8 0) label)
    (check-equal? (subbytes header 4 8) (bytes 0 0 0 0) label)
    (define table (load-dataset-table id))
    (define names (dataset-table-names table))
    (check-true (immutable? info) label)
    (check-equal? (hash-ref info "package") package label)
    (check-equal? (hash-ref info "tier") "bundled" label)
    (check-false (hash-ref info "fetch") label)
    (check-equal? (dataset-table-row-count table) (hash-ref info "rows") label)
    (check-equal? names (map (lambda (meta) (hash-ref meta "name")) (hash-ref info "columns")) label)
    (check-equal? (vector-length (dataset-table-columns table)) (hash-ref info "column-count") label)
    (for ([target (in-list (hash-ref info "suggested-targets"))])
      (check-not-false (member target names) label))
    (match-define (hash-table ("start" start) ("end" end) ("index" index))
      (or (hash-ref info "time-series") (hash "start" '() "end" '() "index" '())))
    (check-equal? (length start) (length index) label)
    (check-equal? (length end) (length index) label)
    (check-equal? (take names (length index)) index label)
    (for ([column (in-vector (dataset-table-columns table))]
          [meta (in-list (hash-ref info "columns"))])
      (check-column info column meta types label)))

  (define (check-licenses data-dir ids)
    (match-define (list _ 'AND data-license)
      ((get-info/full (simplify-path (build-path data-dir 'up 'up))) 'license))
    (define registry-licenses
      (remove-duplicates (for/list ([id (in-list ids)]) (hash-ref (dataset-info id) "license"))))
    (check-equal? (sort registry-licenses string<?) (sort (conjuncts data-license) string<?))
    (check-license-files (map path->string (directory-list (build-path data-dir "licenses")))
                         registry-licenses
                         (path->string data-dir)))

  (define (check-license-files files licenses where)
    (check-equal? (sort files string<?)
                  (sort (for/list ([id (in-list (remove-duplicates (append-map license-ids licenses)))])
                          (string-append id ".txt"))
                        string<?)
                  where))

  (define (failures thunk)
    (define failed 0)
    (define (count-failure check)
      (with-handlers ([exn:test:check? (lambda (_) (set! failed (add1 failed)))])
        (check)))
    (parameterize ([current-check-around count-failure])
      (thunk))
    failed)

  (define (check-bundled-registry package data-dir ids #:types [types 'inferred])
    (check-equal? (dataset-names #:package package) ids)
    (check-not-false (member package (dataset-packages)))
    (for ([id (in-list ids)])
      (check-dataset package data-dir id types))
    (check-equal? (sort (map path->string (directory-list (build-path data-dir "data"))) string<?)
                  (sort (for/list ([id (in-list ids)]) (format "~a.rktd.gz" id)) string<?))
    (check-equal? (for/list ([p (in-list (directory-list data-dir))]
                             #:when (regexp-match? #rx"[.]rkt$" (path->string p)))
                    (path->string p))
                  '("info.rkt"))
    (check-licenses data-dir ids))

  (define (id-column-failures . cells)
    (define info (hash "rows" (length cells) "identifiers" '("id")))
    (define meta
      (hash "name" "id" "type" "identifier" "levels" '()
            "missing-count" (length (filter dataset-missing? cells))))
    (failures (lambda () (check-column info (apply vector-immutable cells) meta 'inferred "fixture"))))
  (check-equal? (id-column-failures 1 2 3) 0)
  (check-equal? (id-column-failures 1 dataset-missing dataset-missing) 1)
  (check-equal? (id-column-failures 1 1 2) 1)
  (for ([row (in-list '([("MIT.txt" "CC-BY-4.0.txt") ("CC-BY-4.0" "MIT AND CC-BY-4.0") 0]
                        [("CC-BY-4.0.txt" "CC-BY-4.0-draft.txt") ("CC-BY-4.0") 1]
                        [("MIT-datasauRus.txt") ("MIT") 1]
                        [("CC-BY-4.0.txt") ("CC-BY-4.0" "MIT") 1]))])
    (match-define (list files licenses expected) row)
    (check-equal? (failures (lambda () (check-license-files files licenses "fixture"))) expected
                  (format "~s" row)))
  (check-equal? (license-ids "CC0-1.0 AND (MIT OR GPL-2.0-only)") '("CC0-1.0" "MIT" "GPL-2.0-only"))
  (check-equal? (license-ids "MIT") '("MIT"))
  (check-equal? (conjuncts '(CC0-1.0 AND (MIT AND (A OR (B AND C)))))
                '("CC0-1.0" "MIT" "A OR (B AND C)")))
