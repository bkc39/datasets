#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core-private/codec gunzip-file hex)
         (only-in datasets/core-private/table
                  dataset-missing dataset-missing? dataset-table dataset-table-columns
                  dataset-table-info dataset-table-names dataset-table-row-count dataset-table?)
         (only-in racket/contract/base -> ->* contract-out listof or/c)
         (only-in racket/format ~a ~e ~s)
         (only-in racket/list append-map check-duplicates index-of remove-duplicates)
         (only-in racket/match == match)
         (only-in racket/runtime-path define-runtime-path)
         (only-in setup/getinfo find-relevant-directories get-info/full))
(provide (contract-out
          [dataset-names (->* () (#:package (or/c #f string?)) (listof symbol?))]
          [dataset-packages (-> (listof string?))])
         dataset-info load-dataset-table
         dataset-missing dataset-missing?
         dataset-table? dataset-table-names dataset-table-columns
         dataset-table-row-count dataset-table-info dataset-table-column)

(struct catalog (entries data-dirs))
(define-runtime-path core-data "core-data")
(define core-registry (build-path core-data "registry.rktd"))

(define (freeze v)
  (cond [(string? v) (string->immutable-string v)]
        [(pair? v) (cons (freeze (car v)) (freeze (cdr v)))]
        [(hash? v) (for/hash ([(k x) (in-hash v)]) (values (freeze k) (freeze x)))]
        [else v]))

(define (raise-registry-error who message . fields)
  (define details
    (let loop ([fields fields])
      (match fields
        ['() '()]
        [(list* label value more)
         (list* "\n  " label ": " (if (path? value) (path->string value) (~a value)) (loop more))])))
  (raise (exn:fail (apply string-append (~a who) ": " message details) (current-continuation-marks))))

(define (registry-path-string? v)
  (and (string? v) (relative-path? v) (andmap path? (explode-path v))))

(define (declared-registries dir)
  (define info-file (build-path dir "info.rkt"))
  (define info
    (with-handlers ([exn:fail? (lambda (e)
                                 (raise-registry-error 'datasets "cannot read info.rkt"
                                                       "info.rkt" info-file "error" (exn-message e)))])
      (get-info/full dir)))
  (define value (and info (info 'datasets-registries (lambda () #f))))
  (cond
    [(not info) '()]
    [(not (and (list? value) (andmap registry-path-string? value)))
     (raise-registry-error 'datasets "malformed datasets-registries" "info.rkt" info-file
                           "expected" "a list of relative path strings" "given" (~s value))]
    [else
     (for/list ([file (in-list value)])
       (define path (simplify-path (build-path dir file)))
       (cond [(file-exists? path) path]
             [else (raise-registry-error 'datasets "registry file not found" "info.rkt" info-file
                                         "datasets-registries" (~s value) "file" path)]))]))

(define (registry-files dirs)
  (remove-duplicates
   (sort (cons (simplify-path core-registry) (append-map declared-registries dirs))
         string<? #:key path->string)
   #:key file-or-directory-identity))

(define (sha256-string? v) (and (string? v) (regexp-match? #px"^[0-9a-f]{64}$" v)))
(define ((nonempty-list-of ok?) v)
  (and (pair? v) (list? v) (andmap ok? v)))
(define (column-hash? v) (and (hash? v) (string? (hash-ref v "name" #f))))
(define required-keys
  '("name" "rows" "column-count" "columns" "identifiers" "suggested-targets" "source-url"
    "source-version" "source-sha256" "sha256" "license" "citation" "notes" "time-series"
    "tier" "package" "fetch"))

(define (entry-problem name entry)
  (define missing (for/first ([key (in-list required-keys)] #:unless (hash-has-key? entry key)) key))
  (define fetch (hash-ref entry "fetch" #f))
  (define tier (hash-ref entry "tier" #f))
  (define (fetch-ref key) (and (hash? fetch) (hash-ref fetch key #f)))
  (cond
    [missing (list missing "present")]
    [(not (equal? (hash-ref entry "name") name)) (list "name" "the dataset's registry key")]
    [(not (regexp-match? #px"^[a-z0-9]+(-[a-z0-9]+)*$" name))
     (list "name" "lowercase letters and digits in hyphen-separated words")]
    [(not ((nonempty-list-of column-hash?) (hash-ref entry "columns")))
     (list "columns" "a nonempty list of hashes, each with a string \"name\"")]
    [(not (sha256-string? (hash-ref entry "sha256"))) (list "sha256" "64 lowercase hexadecimal digits")]
    [(not (member tier '("bundled" "fetch"))) (list "tier" "\"bundled\" or \"fetch\"")]
    [(not (and (string? (hash-ref entry "package"))
               (regexp-match? #px"^[A-Za-z0-9_+-]+$" (hash-ref entry "package"))))
     (list "package" "a package name")]
    [(and (equal? tier "bundled") fetch) (list "fetch" "#f for a bundled dataset")]
    [(and (equal? tier "fetch") (not (hash? fetch))) (list "fetch" "a hash for a fetch-tier dataset")]
    [(equal? tier "bundled") #f]
    [(not ((nonempty-list-of string?) (fetch-ref "urls"))) (list "urls" "a nonempty list of strings")]
    [(not (sha256-string? (fetch-ref "sha256"))) (list "sha256" "64 lowercase hexadecimal digits")]
    [(not (exact-positive-integer? (fetch-ref "size"))) (list "size" "a positive byte count")]
    [(not (string? (fetch-ref "format"))) (list "format" "a string")]
    [else #f]))

(define (read-registry file)
  (define registry
    (with-handlers ([exn:fail? (lambda (e)
                                 (raise-registry-error 'datasets "cannot read registry"
                                                       "registry" file "error" (exn-message e)))])
      (call-with-input-file file read)))
  (unless (and (hash? registry) (immutable? registry) (andmap string? (hash-keys registry)))
    (raise-registry-error 'datasets "malformed registry" "registry" file
                          "expected" "an immutable hash from dataset names to metadata hashes"
                          "given" (~e registry)))
  (for ([(name entry) (in-hash registry)])
    (match (if (and (hash? entry) (immutable? entry))
               (entry-problem name entry)
               (list #f "an immutable metadata hash"))
      [#f (void)]
      [(list key expected)
       (apply raise-registry-error 'datasets "malformed registry entry" "registry" file
                             "dataset" (~s name)
                             (append (if key (list "key" (~s key)) '()) (list "expected" expected)))]))
  (define packages
    (sort (remove-duplicates (for/list ([entry (in-hash-values registry)]) (hash-ref entry "package")))
          string<?))
  (when (> (length packages) 1)
    (raise-registry-error 'datasets "one registry declares several packages" "registry" file
                          "key" (~s "package") "packages" (~s packages)))
  (freeze registry))

(define (core-copy? file registry core-identity)
  (and (not (= (file-or-directory-identity file) core-identity))
       (for/and ([entry (in-hash-values registry)])
         (equal? (hash-ref entry "package") "datasets-core"))))

(define (build-catalog files)
  (define core-identity (file-or-directory-identity core-registry))
  (for*/fold ([entries (hash)] [data-dirs (hash)] [sources (hash)]
              #:result (catalog entries data-dirs))
             ([file (in-list files)]
              [registry (in-value (read-registry file))]
              #:unless (core-copy? file registry core-identity)
              [(name entry) (in-hash registry)])
    (define previous (hash-ref sources name #f))
    (when previous
      (raise-registry-error 'datasets "duplicate dataset" "name" (~s name)
                            "package" (~s (hash-ref (hash-ref entries name) "package"))
                            "registry" previous
                            "package" (~s (hash-ref entry "package")) "registry" file))
    (define-values (dir _name _dir?) (split-path file))
    (values (hash-set entries name entry)
            (hash-set data-dirs name (build-path dir "data"))
            (hash-set sources name file))))

(define cache (box #f))

(define (file-signature file)
  (define stat (file-or-directory-stat file))
  (list file (hash-ref stat 'modify-time-nanoseconds) (hash-ref stat 'size)))

(define (current-catalog)
  (define files (registry-files (find-relevant-directories '(datasets-registries) 'all-available)))
  (define key (map file-signature files))
  (match (unbox cache)
    [(cons (== key) cached) cached]
    [_ (define fresh (build-catalog files))
       (set-box! cache (cons key fresh))
       fresh]))

(define (dataset-names #:package [package #f])
  (define names
    (for/list ([(name entry) (in-hash (catalog-entries (current-catalog)))]
               #:when (or (not package) (equal? package (hash-ref entry "package"))))
      (string->symbol name)))
  (when (and package (null? names))
    (raise-arguments-error 'dataset-names "unknown package"
                           "package" package "available" (dataset-packages)))
  (sort names symbol<?))

(define (dataset-packages)
  (sort (remove-duplicates (for/list ([entry (in-hash-values (catalog-entries (current-catalog)))])
                             (hash-ref entry "package")))
        string<?))

(define (dataset-entry name)
  (define known (current-catalog))
  (unless (and (symbol? name) (hash-has-key? (catalog-entries known) (symbol->string name)))
    (raise-arguments-error 'dataset-info "unknown dataset" "name" name "available" (dataset-names)))
  (values (hash-ref (catalog-entries known) (symbol->string name))
          (hash-ref (catalog-data-dirs known) (symbol->string name))))

(define (dataset-info name)
  (define-values (info _data-dir) (dataset-entry name))
  info)

(define (dataset-table-column table name)
  (define i (index-of (dataset-table-names table) name))
  (unless i (raise-arguments-error 'dataset-table-column "unknown column" "column" name
                                   "available" (dataset-table-names table)))
  (vector-ref (dataset-table-columns table) i))

(define (read-rows name info data-dir)
  (define file (build-path data-dir (format "~a.rktd.gz" name)))
  (define text
    (with-handlers ([exn:fail? (lambda (e)
                                 (raise-registry-error 'load-dataset-table "cannot read dataset data"
                                                       "dataset" name "file" file
                                                       "error" (exn-message e)))])
      (gunzip-file file)))
  (define expected (hash-ref info "sha256"))
  (define actual (hex (sha256-bytes text)))
  (unless (equal? actual expected)
    (raise-registry-error 'load-dataset-table "data sha256 mismatch" "dataset" name "file" file
                          "expected" expected "actual" actual))
  (read (open-input-bytes text)))

(define (load-dataset-table name #:columns [selected #f])
  (define-values (info data-dir) (dataset-entry name))
  (define all-names (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")))
  (define names (or selected all-names))
  (unless (and (list? names) (pair? names) (andmap string? names))
    (raise-argument-error 'load-dataset-table "nonempty list of column-name strings or #f" selected))
  (when (check-duplicates names)
    (raise-arguments-error 'load-dataset-table "duplicate column" "columns" names))
  (define indices
    (for/list ([n (in-list names)])
      (or (index-of all-names n)
          (raise-arguments-error 'load-dataset-table "unknown column" "column" n "available" all-names))))
  (when (equal? (hash-ref info "tier") "fetch")
    (raise-registry-error 'load-dataset-table "dataset is fetch tier and cannot be downloaded yet"
                          "dataset" name))
  (define rows (map list->vector (read-rows name info data-dir)))
  (define columns
    (vector->immutable-vector
     (for/vector ([i (in-list indices)])
       (vector->immutable-vector
        (for/vector ([row (in-list rows)])
          (define v (vector-ref row i))
          (if (eq? v 'missing) dataset-missing (freeze v)))))))
  (dataset-table (map string->immutable-string names) columns (length rows) info))
