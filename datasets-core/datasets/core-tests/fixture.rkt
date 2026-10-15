#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-missing? dataset-table-columns dataset-table-names
                  dataset-table-row-count dataset-table?)
         (only-in datasets/core-private/codec gunzip-file hex)
         (only-in file/gzip gzip-through-ports)
         (only-in racket/contract/base -> any any/c contract-out hash/c listof)
         (only-in racket/file delete-directory/files make-directory* make-temporary-directory)
         (only-in racket/port call-with-output-bytes)
         (only-in racket/runtime-path define-runtime-path))
(provide (contract-out
          [registry-fixture (-> string? complete-path?)]
          [call-with-registry-fixtures (-> (listof complete-path?) (-> any) any)]
          [call-with-temporary-fixture (-> (-> complete-path? any) any)]
          [write-registry-fixture! (-> path? #:info any/c #:registries (hash/c string? any/c)
                                       #:data (hash/c string? bytes?) void?)]
          [table-digest (-> dataset-table? string?)])
         rows->rktd-bytes gzip-bytes gunzip-file hex gen-entry fetch-ok)

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
