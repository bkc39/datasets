#lang racket/base
(require (for-syntax (only-in compiler/cm-accomplice register-external-file)
                     racket/base
                     (only-in setup/getinfo get-info/full)
                     (only-in syntax/parse syntax-parse))
         (only-in datasets/core-private/cache cache-file cache-root clear-cache-directory
                  clear-cache-root download-dataset)
         (only-in datasets/core-private/codec gunzip-file hex)
         (only-in datasets/core-private/error raise-registry-error)
         (only-in datasets/core-private/log log-datasets-warning)
         (only-in racket/contract/base -> ->* and/c contract-out listof non-empty-listof or/c)
         (only-in racket/format ~e ~s)
         (only-in racket/list append-map check-duplicates index-of remove-duplicates take)
         (only-in racket/match == match)
         (only-in racket/runtime-path define-runtime-path)
         (only-in setup/getinfo directory-record-path directory-record-spec find-relevant-directory-records
                  get-info/full reset-relevant-directories-state!))
(provide (contract-out
          [dataset-names (->* () (#:package (or/c #f string?)) (listof symbol?))]
          [dataset-packages (-> (listof string?))]
          [load-dataset-table (->* (symbol?)
                                   (#:columns (or/c #f (non-empty-listof string?))
                                    #:download? boolean?)
                                   dataset-table?)]
          [fetch-dataset (->* (symbol?) (#:idle-timeout (and/c real? positive?)) complete-path?)]
          [dataset-cached? (-> symbol? boolean?)]
          [dataset-cache-directory (-> complete-path?)]
          [clear-dataset-cache (->* () ((or/c #f symbol?)) void?)])
         dataset-info
         dataset-missing dataset-missing?
         dataset-table? dataset-table-names dataset-table-columns
         dataset-table-row-count dataset-table-info dataset-table-column)

;; Opaque, singleton sentinel: cannot be confused with any ordinary observation.
(struct missing-value ())
(define dataset-missing (missing-value))
(define (dataset-missing? v) (eq? v dataset-missing))
(struct dataset-table (names columns row-count info) #:transparent)
(struct catalog (entries data-dirs discovery-failure))
(struct source (file core? signature) #:transparent)
(define-runtime-path core-data "core-data")

(define-syntax (declared-core-registries stx)
  (syntax-parse stx
    [(_)
     #:do [(define-values (dir _name _dir?) (split-path (syntax-source stx)))
           (define info-dir (build-path dir "core-data"))
           (register-external-file (build-path info-dir "info.rkt"))]
     #:with names (datum->syntax stx ((get-info/full info-dir) 'datasets-registries))
     #'(quote names)]))

(define core-registry-names (declared-core-registries))

(define (freeze v)
  (cond [(string? v) (string->immutable-string v)]
        [(pair? v) (cons (freeze (car v)) (freeze (cdr v)))]
        [(hash? v) (for/hash ([(k x) (in-hash v)]) (values (freeze k) (freeze x)))]
        [else v]))

(define (registry-path-string? v)
  (and (string? v) (relative-path? v) (andmap path? (explode-path v))))

(define (info-registries dir)
  (define info-file (build-path dir "info.rkt"))
  (define info
    (with-handlers ([exn:fail? (lambda (e)
                                 (raise-registry-error 'datasets "cannot read info.rkt"
                                                       "info.rkt" info-file "error" (exn-message e)))])
      (get-info/full dir #:namespace (make-base-empty-namespace))))
  (check-registries dir (if info (info 'datasets-registries (lambda () '())) '())))

(define (check-registries dir value)
  (cond
    [(and (list? value) (andmap registry-path-string? value)) value]
    [else (raise-registry-error 'datasets "malformed datasets-registries"
                                "info.rkt" (build-path dir "info.rkt")
                                "expected" "a list of relative path strings" "given" (~s value))]))

(define (resolve-registries dir names)
  (for/list ([name (in-list names)])
    (define path (simplify-path (build-path dir name)))
    (cond [(file-exists? path) path]
          [else (raise-registry-error 'datasets "registry file not found"
                                      "info.rkt" (build-path dir "info.rkt")
                                      "datasets-registries" (~s names) "file" path)])))

(define (file-signature file)
  (define stat (file-or-directory-stat file))
  (for/list ([key (in-list '(device-id inode change-time-nanoseconds modify-time-nanoseconds size))])
    (hash-ref stat key)))

(define (info-signature dir)
  (for/list ([file (in-list (cons (build-path dir "info.rkt")
                                  (for/list ([compiled (in-list (use-compiled-file-paths))])
                                    (build-path dir compiled "info_rkt.zo"))))]
             #:when (file-exists? file))
    (cons file (file-signature file))))

(define info-cache (box (hash)))

(define (declared-registries dirs)
  (define previous (unbox info-cache))
  (define current
    (for/hash ([dir (in-list dirs)])
      (define signature (info-signature dir))
      (values dir (match (hash-ref previous dir #f)
                    [(and hit (cons (== signature) _names)) hit]
                    [_ (cons signature (info-registries dir))]))))
  (set-box! info-cache current)
  (append-map (lambda (dir) (resolve-registries dir (cdr (hash-ref current dir)))) dirs))

(define (registry-sources dirs)
  (define core-names (check-registries core-data core-registry-names))
  (define core (resolve-registries core-data core-names))
  (define discovered
    (sort (declared-registries dirs)
          string<? #:key path->string))
  (remove-duplicates
   (append (for/list ([file (in-list core)]) (source file #t (file-signature file)))
           (for/list ([file (in-list discovered)]) (source file #f (file-signature file))))
   #:key (lambda (s) (take (source-signature s) 2))))

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
    [(not (string? (fetch-ref "format"))) (list "format" "a string")]
    [else #f]))

(define (read-registry file core?)
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
  (when (and (not core?) (equal? packages '("datasets-core")))
    (raise-registry-error 'datasets "package datasets-core is reserved" "registry" file))
  (freeze registry))

(define (build-catalog sources)
  (for*/fold ([entries (hash)] [data-dirs (hash)] [registries (hash)]
              #:result (catalog entries data-dirs #f))
             ([src (in-list sources)]
              [file (in-value (source-file src))]
              [(name entry) (in-hash (read-registry file (source-core? src)))])
    (define previous (hash-ref registries name #f))
    (when previous
      (raise-registry-error 'datasets "duplicate dataset" "name" (~s name)
                            "package" (~s (hash-ref (hash-ref entries name) "package"))
                            "registry" previous
                            "package" (~s (hash-ref entry "package")) "registry" file))
    (define-values (dir _name _dir?) (split-path file))
    (values (hash-set entries name entry)
            (hash-set data-dirs name (build-path dir "data"))
            (hash-set registries name file))))

(define cache (box #f))

(define (access-denied? e)
  (or (and (exn:fail:filesystem:errno? e)
           (member (exn:fail:filesystem:errno-errno e) '((13 . posix) (1 . posix) (5 . windows))))
      (and (exn:fail? e) (regexp-match? #rx"^[^:\n]*: `[a-z+-]+' access denied for " (exn-message e)))))

(define (discovered-directories)
  (with-handlers ([access-denied?
                   (lambda (e)
                     (log-datasets-warning "sibling-package discovery failed; using datasets-core only: ~a"
                                           (exn-message e))
                     (values '() (exn-message e)))])
    (values (with-handlers ([(lambda (_) #t)
                             (lambda (e) (reset-relevant-directories-state!) (raise e))])
              (for/list ([record (in-list (find-relevant-directory-records '(datasets-registries) 'all-available))]
                         #:unless (equal? (directory-record-spec record) '(lib "datasets" "core-data")))
                (directory-record-path record)))
            #f)))

(define (current-catalog)
  (define-values (dirs failure) (discovered-directories))
  (define sources (registry-sources dirs))
  (define built
    (match (unbox cache)
      [(cons (== sources) cached) cached]
      [_ (define fresh (build-catalog sources))
         (set-box! cache (cons sources fresh))
         fresh]))
  (struct-copy catalog built [discovery-failure failure]))

(define (discovery-failure-fields known)
  (match (catalog-discovery-failure known)
    [#f '()]
    [reason (list "sibling-package discovery failed" reason)]))

(define (catalog-packages known)
  (sort (remove-duplicates (for/list ([entry (in-hash-values (catalog-entries known))])
                             (hash-ref entry "package")))
        string<?))

(define (dataset-names #:package [package #f])
  (define known (current-catalog))
  (define names
    (for/list ([(name entry) (in-hash (catalog-entries known))]
               #:when (or (not package) (equal? package (hash-ref entry "package"))))
      (string->symbol name)))
  (when (and package (null? names))
    (apply raise-arguments-error 'dataset-names "unknown package"
           "package" package "available" (catalog-packages known) (discovery-failure-fields known)))
  (sort names symbol<?))

(define (dataset-packages)
  (catalog-packages (current-catalog)))

(define (dataset-entry who name)
  (define known (current-catalog))
  (unless (and (symbol? name) (hash-has-key? (catalog-entries known) (symbol->string name)))
    (apply raise-arguments-error who "unknown dataset" "name" name "available" (dataset-names)
           (discovery-failure-fields known)))
  (values (hash-ref (catalog-entries known) (symbol->string name))
          (hash-ref (catalog-data-dirs known) (symbol->string name))))

(define (dataset-info name)
  (dataset-info* 'dataset-info name))

(define (dataset-table-column table name)
  (define i (index-of (dataset-table-names table) name))
  (unless i (raise-arguments-error 'dataset-table-column "unknown column" "column" name
                                   "available" (dataset-table-names table)))
  (vector-ref (dataset-table-columns table) i))

(define (read-rows name file expected remedy)
  (define (unreadable e)
    (apply raise-registry-error 'load-dataset-table "cannot read dataset data" "dataset" name
           "file" file "error" (exn-message e) (if remedy (list "remedy" remedy) '())))
  (define text (with-handlers ([exn:fail? unreadable]) (gunzip-file file)))
  (define actual (and expected (hex (sha256-bytes text))))
  (cond
    [(equal? actual expected) (with-handlers ([exn:fail? unreadable]) (read (open-input-bytes text)))]
    [else (raise-registry-error 'load-dataset-table "data sha256 mismatch" "dataset" name
                                "file" file "expected" expected "actual" actual)]))

(define (fetch-tier? info)
  (equal? (hash-ref info "tier") "fetch"))

(define (bundled-file name data-dir)
  (build-path data-dir (format "~a.rktd.gz" name)))

(define (load-dataset-table name #:columns [selected #f] #:download? [download? #t])
  (define-values (info data-dir) (dataset-entry 'load-dataset-table name))
  (define all-names (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")))
  (define names (or selected all-names))
  (when (check-duplicates names)
    (raise-arguments-error 'load-dataset-table "duplicate column" "columns" names))
  (define indices
    (for/list ([n (in-list names)])
      (or (index-of all-names n)
          (raise-arguments-error 'load-dataset-table "unknown column" "column" n "available" all-names))))
  (define final
    (and (fetch-tier? info)
         (cache-file (cache-root 'load-dataset-table) (symbol->string name) info)))
  (define file
    (cond [(not final) (bundled-file name data-dir)]
          [(file-exists? final) final]
          [else (download-dataset 'load-dataset-table (symbol->string name) info final
                                  #:download? download? #:idle-timeout 60)]))
  (define rows
    (map list->vector
         (read-rows name file (and (not final) (hash-ref info "sha256"))
                    (and final (format "(clear-dataset-cache '~a)" name)))))
  (define columns
    (vector->immutable-vector
     (for/vector ([i (in-list indices)])
       (vector->immutable-vector
        (for/vector ([row (in-list rows)])
          (define v (vector-ref row i))
          (if (eq? v 'missing) dataset-missing (freeze v)))))))
  (dataset-table (map string->immutable-string names) columns (length rows) info))

(define (fetch-dataset name #:idle-timeout [idle 60])
  (define-values (info data-dir) (dataset-entry 'fetch-dataset name))
  (define root (cache-root 'fetch-dataset))
  (define final (and (fetch-tier? info) (cache-file root (symbol->string name) info)))
  (cond
    [(not final) (bundled-file name data-dir)]
    [(file-exists? final) final]
    [else (download-dataset 'fetch-dataset (symbol->string name) info final
                            #:download? #t #:idle-timeout idle)]))

(define (dataset-cached? name)
  (define-values (info _data-dir) (dataset-entry 'dataset-cached? name))
  (define root (cache-root 'dataset-cached?))
  (or (not (fetch-tier? info))
      (file-exists? (cache-file root (symbol->string name) info))))

(define (dataset-cache-directory)
  (cache-root 'dataset-cache-directory))

(define (clear-dataset-cache [name #f])
  (define info (and name (dataset-info* 'clear-dataset-cache name)))
  (define root (cache-root 'clear-dataset-cache))
  (cond
    [(not name) (clear-cache-root 'clear-dataset-cache root)]
    [(fetch-tier? info)
     (clear-cache-directory 'clear-dataset-cache name (build-path root (symbol->string name)))]
    [else (void)]))

(define (dataset-info* who name)
  (define-values (info _data-dir) (dataset-entry who name))
  info)
