#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-missing dataset-missing? dataset-table dataset-table-columns
                  dataset-table-info dataset-table-names dataset-table-row-count dataset-table?)
         (only-in datasets/core-private/codec gunzip-file hex)
         (only-in file/gzip gzip-through-ports)
         (only-in net/url path->url url->string)
         (only-in racket/contract/base -> ->* and/c any any/c contract-out hash/c list/c listof or/c)
         (only-in racket/file delete-directory/files file->bytes make-directory*
                  make-temporary-directory)
         (only-in racket/match match-lambda)
         (only-in racket/path find-relative-path)
         (only-in racket/port call-with-output-bytes)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/string string-join)
         (only-in "http-fixture.rkt" call-with-http-fixture http-fixture-url))
(provide (contract-out
          [registry-fixture (-> string? complete-path?)]
          [call-with-registry-fixtures (->* ((listof complete-path?) (-> any)) (#:specs (listof any/c)) any)]
          [call-with-temporary-fixture (-> (-> complete-path? any) any)]
          [write-fixture-index! (-> complete-path? (listof complete-path?) (listof any/c) void?)]
          [write-registry-fixture! (-> path? #:info any/c #:registries (hash/c string? any/c)
                                       #:data (hash/c string? bytes?) void?)]
          [table-digest (-> dataset-table? string?)]
          [rebuild (-> dataset-table? dataset-table?)]
          [datum->cells (-> list? list?)]
          [column-types (-> dataset-table? (listof string?))]
          [toy-artifact bytes?]
          [toy-artifact-sha256 string?]
          [toy-table-digest string?]
          [fetch-entry (->* ((listof string?))
                            (#:data-sha256 string? #:format string? #:sha256 string?)
                            (and/c hash? immutable?))]
          [call-with-fetch-fixture (-> (listof hash?) (-> complete-path? complete-path? any) any)]
          [call-with-environment (-> (hash/c string? (or/c #f string?)) (-> any) any)]
          [call-with-artifact-url (-> bytes? (-> string? any) any)]
          [cache-files (-> path? (listof string?))]
          [wait-until (-> (and/c real? positive?) (-> any/c) boolean?)]
          [fetch-error? (-> (or/c string? regexp?) ... (-> any/c boolean?))]
          [with-server (-> hash? (-> (-> string? string?) (listof hash?))
                           (-> any/c complete-path? complete-path? any) any)]
          [toy-at (-> string? (-> (-> string? string?) (listof hash?)))]
          [toy-rows list?]
          [toy-content-sha256 string?]
          [raw-a bytes?]
          [raw-a-sha256 string?]
          [raw-b bytes?]
          [raw-b-sha256 string?]
          [set-entry (->* ((listof (list/c string? (listof string?) string?)))
                          (#:binding string? #:module string? #:data-sha256 string?)
                          (and/c hash? immutable?))]
          [convert-ok (and/c hash? immutable?)]
          [toy-set-entry (->* ((-> string? string?))
                              (#:binding string? #:module string? #:data-sha256 string?)
                              (and/c hash? immutable?))]
          [wide-entry (->* (exact-positive-integer? #:rows exact-nonnegative-integer?)
                           (#:data-sha256 string?)
                           (and/c hash? immutable?))])
         rows->rktd-bytes gzip-bytes gunzip-file hex gen-entry fetch-ok)

(define-runtime-path fixtures "fixtures")

(define (registry-fixture name)
  (simplify-path (build-path fixtures name)))

(define fixture-count (box 0))

(define (call-with-temporary-fixture proc)
  (set-box! fixture-count (add1 (unbox fixture-count)))
  (define dir (make-temporary-directory (format "datasets-fixture-~a-~~a" (unbox fixture-count))))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(define (call-with-registry-fixtures dirs thunk
                                     #:specs [specs (for/list ([i (in-range (length dirs))])
                                                      `(lib ,(format "datasets-fixture-~a" i)))])
  (call-with-temporary-fixture
   (lambda (root)
     (write-fixture-index! root dirs specs)
     (parameterize ([current-library-collection-paths
                     (cons root (current-library-collection-paths))])
       (thunk)))))

(define (write-fixture-index! root dirs specs)
  (define compiled (build-path root "info-domain" "compiled"))
  (make-directory* compiled)
  (call-with-output-file (build-path compiled "cache.rktd") #:exists 'truncate
    (lambda (out)
      (write (for/list ([dir (in-list dirs)] [spec (in-list specs)])
               `(,(path->bytes dir) (datasets-registries) ,spec 0 0))
             out))))

(define (write-registry-fixture! dir #:info info #:registries registries #:data data)
  (call-with-output-file (build-path dir "info.rkt") #:exists 'truncate
    (lambda (out)
      (write `(module info info (#%module-begin (define datasets-registries ',info))) out)))
  (for ([(file registry) (in-hash registries)])
    (call-with-output-file (build-path dir file) #:exists 'truncate
      (lambda (out) (write registry out))))
  (make-directory* (build-path dir "data"))
  (for ([(name bytes) (in-hash data)])
    (call-with-output-file (build-path dir "data" (format "~a.rktd.gz" name)) #:exists 'truncate
      (lambda (out) (write-bytes bytes out))))
  (void))

(define (rows->rktd-bytes rows)
  (string->bytes/utf-8
   (string-append "(\n" (apply string-append (map (lambda (row) (format "  ~s\n" row)) rows)) ")\n")))

(define (gzip-bytes bs [timestamp 0])
  (call-with-output-bytes
   (lambda (out) (gzip-through-ports (open-input-bytes bs) out #f timestamp))))

(define (rebuild t)
  (dataset-table (dataset-table-names t) (vector->list (dataset-table-columns t))
                 #:info (dataset-table-info t)))

(define (datum->cells datum)
  (map (lambda (c) (if (eq? c 'missing) dataset-missing c)) datum))

(define (column-types t)
  (map (lambda (c) (hash-ref c "type")) (hash-ref (dataset-table-info t) "columns")))

(define (table-digest table)
  (hex (sha256-bytes
        (string->bytes/utf-8
         (format "~s" (list (dataset-table-names table)
                            (dataset-table-row-count table)
                            (for/list ([c (in-vector (dataset-table-columns table))])
                              (for/list ([v (in-vector c)])
                                (if (dataset-missing? v) '#:missing v)))))))))

(define (gen-entry name #:package [package "datasets-gen"] #:tier [tier "bundled"]
                   #:fetch [fetch #f] #:sha256 [sha256 (make-string 64 #\0)])
  (hash "name" name "rows" 1 "column-count" 1
        "columns" (list (hash "name" "x" "original-name" "x" "type" "integer" "levels" '()
                              "unit" #f "description" "x" "missing-count" 0))
        "identifiers" '() "suggested-targets" '()
        "source-url" "file:///fixture/gen" "source-version" "fixture"
        "source-sha256" (make-string 64 #\0) "sha256" sha256
        "license" "CC0-1.0" "citation" "Fixture." "notes" "Fixture." "time-series" #f
        "tier" tier "package" package "fetch" fetch))

(define fetch-ok
  (hash "urls" '("file:///fixture/gen.rktd.gz") "sha256" (make-string 64 #\a) "format" "rktd.gz"))

(define toy-artifact (file->bytes (build-path fixtures "sibling" "data" "toy.rktd.gz")))
(define toy-artifact-sha256 "2a8b8c1a037d8c33432ee050c106b1840433f5f7138b0f326c6117719491ba85")
(define toy-table-digest "8351c6815d3e312013fd46602ec0d0b7eebed45c7187bbf24eb04c22566ff3ed")
(define toy-remote
  (hash-ref (call-with-input-file (build-path fixtures "sibling" "remote.rktd") read) "toy-remote"))

(define (fetch-entry urls #:sha256 [sha256 toy-artifact-sha256] #:format [format "rktd.gz"]
                     #:data-sha256 [data-sha256 (hash-ref toy-remote "sha256")])
  (hash-set* toy-remote
             "package" "datasets-fetch-fixture"
             "sha256" data-sha256
             "fetch" (hash "urls" urls "sha256" sha256 "format" format)))

(define (call-with-fetch-fixture entries proc)
  (call-with-temporary-fixture
   (lambda (registry)
     (write-registry-fixture!
      registry #:info '("registry.rktd") #:data (hash)
      #:registries (hash "registry.rktd"
                         (for/hash ([entry (in-list entries)])
                           (values (hash-ref entry "name") entry))))
     (call-with-temporary-fixture
      (lambda (root)
        (call-with-registry-fixtures (list registry)
          (lambda ()
            (call-with-environment (hash "DATASETS_HOME" (path->string root))
              (lambda () (proc root registry))))))))))

(define (call-with-environment changes thunk)
  (define env (environment-variables-copy (current-environment-variables)))
  (for ([(name value) (in-hash changes)])
    (environment-variables-set! env
                                (string->bytes/utf-8 name)
                                (and value (string->bytes/utf-8 value))))
  (parameterize ([current-environment-variables env])
    (thunk)))

(define (call-with-artifact-url bytes proc)
  (call-with-temporary-fixture
   (lambda (dir)
     (define file (build-path dir "artifact.rktd.gz"))
     (call-with-output-file file (lambda (out) (write-bytes bytes out)))
     (proc (url->string (path->url file))))))

(define (cache-files root)
  (cond
    [(directory-exists? root)
     (sort (for/list ([path (in-directory root)] #:when (file-exists? path))
             (string-join (map path->string (explode-path (find-relative-path root path))) "/"))
           string<?)]
    [else '()]))

(define (wait-until bound ready?)
  (define deadline (+ (current-inexact-milliseconds) (* 1000 bound)))
  (let loop ()
    (cond [(ready?) #t]
          [(> (current-inexact-milliseconds) deadline) #f]
          [else (sleep 0.01) (loop)])))

(define ((fetch-error? . rxs) e)
  (and (exn:fail? e) (not (exn:fail:contract? e))
       (for/and ([rx (in-list rxs)])
         (regexp-match? (if (string? rx) (regexp (regexp-quote rx)) rx) (exn-message e)))))

(define (with-server routes entries-of proc)
  (call-with-http-fixture routes
    (lambda (srv)
      (call-with-fetch-fixture (entries-of (lambda (p) (http-fixture-url srv p)))
        (lambda (root registry) (proc srv root registry))))))

(define ((toy-at path) u)
  (list (fetch-entry (list (u path)))))

(define toy-rows '((1 "a" 0.5) (2 missing 1.5) (3 "c" missing)))
(define toy-content-sha256 (hex (sha256-bytes (rows->rktd-bytes toy-rows))))
(define raw-a (rows->rktd-bytes '((1 "a" 0.5))))
(define raw-a-sha256 (hex (sha256-bytes raw-a)))
(define raw-b (rows->rktd-bytes '((2 missing 1.5) (3 "c" missing))))
(define raw-b-sha256 (hex (sha256-bytes raw-b)))

(define (converter-fetch files binding module)
  (hash "format" "convert"
        "files" (map (match-lambda [(list name urls sha256) (hash "name" name "urls" urls "sha256" sha256)])
                     files)
        "converter" (hash "module" module "binding" binding)))

(define (set-entry files #:binding [binding "concat-rows"]
                   #:module [module "datasets/core-tests/convert-fixture"]
                   #:data-sha256 [data-sha256 toy-content-sha256])
  (hash-set* toy-remote
             "name" "toy-set"
             "package" "datasets-fetch-fixture"
             "sha256" data-sha256
             "fetch" (converter-fetch files binding module)))

(define (toy-set-entry u #:binding [binding "concat-rows"]
                       #:module [module "datasets/core-tests/convert-fixture"]
                       #:data-sha256 [data-sha256 toy-content-sha256])
  (set-entry (list (list "a.rktd" (list (u "/a")) raw-a-sha256)
                   (list "b.rktd" (list (u "/b")) raw-b-sha256))
             #:binding binding #:module module #:data-sha256 data-sha256))

(define convert-ok
  (converter-fetch (list (list "gen.csv" '("file:///fixture/gen.csv") (make-string 64 #\a)))
                   "concat-rows" "datasets/core-tests/convert-fixture"))

(define (wide-entry m #:rows rows #:data-sha256 [data-sha256 (make-string 64 #\0)])
  (hash-set* (gen-entry "wide" #:package "datasets-fetch-fixture" #:tier "fetch" #:fetch fetch-ok
                        #:sha256 data-sha256)
             "rows" rows
             "column-count" m
             "columns" (for/list ([j (in-range m)])
                         (hash "name" (format "c~a" j) "original-name" (format "c~a" j) "type" "integer"
                               "levels" '() "unit" #f "description" "c" "missing-count" 0))))
