#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-missing? dataset-table-columns dataset-table-names
                  dataset-table-row-count dataset-table?)
         (only-in datasets/core-private/codec gunzip-file hex)
         (only-in file/gunzip gunzip-through-ports)
         (only-in file/gzip gzip-through-ports)
         (only-in net/url path->url url->string)
         (only-in racket/contract/base -> ->* and/c any any/c contract-out hash/c listof or/c)
         (only-in racket/file delete-directory/files file->bytes make-directory*
                  make-temporary-directory)
         (only-in racket/match match)
         (only-in racket/path find-relative-path)
         (only-in racket/port call-with-output-bytes)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/string string-join))
(provide (contract-out
          [registry-fixture (-> string? complete-path?)]
          [call-with-registry-fixtures (-> (listof complete-path?) (-> any) any)]
          [call-with-temporary-fixture (-> (-> complete-path? any) any)]
          [write-registry-fixture! (-> path? #:info any/c #:registries (hash/c string? any/c)
                                       #:data (hash/c string? bytes?) void?)]
          [table-digest (-> dataset-table? string?)]
          [toy-artifact bytes?]
          [toy-artifact-sha256 string?]
          [toy-table-digest string?]
          [fetch-entry (->* ((listof string?))
                            (#:sha256 string? #:size exact-positive-integer? #:format string?
                             #:content-sha256 string?)
                            (and/c hash? immutable?))]
          [call-with-fetch-fixture (-> (listof hash?) (-> complete-path? complete-path? any) any)]
          [call-with-environment (-> (hash/c string? (or/c #f string?)) (-> any) any)]
          [call-with-artifact-url (-> bytes? (-> string? any) any)]
          [cache-files (-> path? (listof string?))]
          [wait-until (-> (and/c real? positive?) (-> any/c) boolean?)])
         rows->rktd-bytes gzip-bytes gunzip-bytes gunzip-file hex gen-entry fetch-ok path-only*)

(define-runtime-path fixtures "fixtures")

(define (registry-fixture name)
  (simplify-path (build-path fixtures name)))

(define (call-with-temporary-fixture proc)
  (define dir (make-temporary-directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(define (call-with-registry-fixtures dirs thunk)
  (call-with-temporary-fixture
   (lambda (root)
     (define compiled (build-path root "info-domain" "compiled"))
     (make-directory* compiled)
     (call-with-output-file (build-path compiled "cache.rktd")
       (lambda (out)
         (write (for/list ([dir (in-list dirs)] [i (in-naturals)])
                  `(,(path->bytes dir) (datasets-registries)
                    (lib ,(format "datasets-fixture-~a" i)) 0 0))
                out)))
     (parameterize ([current-library-collection-paths
                     (cons root (current-library-collection-paths))])
       (thunk)))))

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

(define (gzip-bytes bs)
  (call-with-output-bytes
   (lambda (out) (gzip-through-ports (open-input-bytes bs) out #f 0))))

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
  (hash "urls" '("file:///fixture/gen.rktd.gz") "sha256" (make-string 64 #\a) "size" 10
        "format" "rktd.gz"))

(define toy-artifact (file->bytes (build-path fixtures "sibling" "data" "toy.rktd.gz")))
(define toy-artifact-sha256 "2a8b8c1a037d8c33432ee050c106b1840433f5f7138b0f326c6117719491ba85")
(define toy-table-digest "8351c6815d3e312013fd46602ec0d0b7eebed45c7187bbf24eb04c22566ff3ed")
(define toy-remote
  (hash-ref (call-with-input-file (build-path fixtures "sibling" "remote.rktd") read) "toy-remote"))

(define (fetch-entry urls #:sha256 [sha256 toy-artifact-sha256] #:size [size 60]
                     #:format [format "rktd.gz"]
                     #:content-sha256 [content-sha256 (hash-ref toy-remote "sha256")])
  (hash-set* toy-remote
             "package" "datasets-fetch-fixture"
             "sha256" content-sha256
             "fetch" (hash "urls" urls "sha256" sha256 "size" size "format" format)))

(define (call-with-fetch-fixture entries proc)
  (call-with-temporary-fixture
   (lambda (registry)
     (write-registry-fixture!
      registry #:info '("registry.rktd") #:data (hash)
      #:registries (hash "registry.rktd"
                         (for/hash ([entry (in-list entries)]) (values (hash-ref entry "name") entry))))
     (call-with-temporary-fixture
      (lambda (root)
        (call-with-registry-fixtures (list registry)
          (lambda ()
            (call-with-environment (hash "DATASETS_HOME" (path->string root) "DATASETS_OFFLINE" #f)
              (lambda () (proc root registry))))))))))

(define (call-with-environment changes thunk)
  (define env (environment-variables-copy (current-environment-variables)))
  (for ([(name value) (in-hash changes)])
    (environment-variables-set! env (string->bytes/utf-8 name) (and value (string->bytes/utf-8 value))))
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

(define (path-only* relative)
  (match (regexp-match #rx"^(.*)/[^/]*$" relative)
    [(list _ dir) dir]
    [#f "."]))

(define (gunzip-bytes bs)
  (call-with-output-bytes
   (lambda (out) (gunzip-through-ports (open-input-bytes bs) out))))
