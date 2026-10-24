#lang racket/base
(require (for-syntax racket/base (only-in syntax/parse ...+ expr id))
         (only-in datasets/core-private/codec file-sha256 hex)
         (only-in datasets/core-private/convert convert-fetch? converter/c row-value?)
         (only-in datasets/core-private/error raise-registry-error)
         (only-in datasets/core-private/log log-datasets-info)
         (only-in datasets/core-private/resource with-custodian with-resource)
         (only-in file/gzip gzip-through-ports)
         (only-in net/url get-pure-port/headers string->url url->path url-scheme)
         (only-in racket/contract/base contract)
         (only-in racket/contract/combinator blame-positive exn:fail:contract:blame-object
                  exn:fail:contract:blame?)
         (only-in racket/file make-directory* make-temporary-file)
         (only-in racket/format ~e ~s)
         (only-in racket/list append*)
         (only-in racket/match match)
         (only-in racket/string string-join)
         (only-in syntax/parse/define define-syntax-parse-rule))
(provide cache-file cache-root clear-cache-directory clear-cache-root download-dataset)

(define name-rx #px"^[a-z0-9]+(-[a-z0-9]+)*$")
(define cache-file-rx
  #px"^[0-9a-f]{64}(\\.rktd\\.gz(\\.[A-Za-z0-9]+\\.part)?|\\.raw\\.[A-Za-z0-9]+\\.part)$")
(define supported-formats '("convert" "rktd.gz"))
(define-namespace-anchor anchor)

(define (cache-root who)
  (match (getenv "DATASETS_HOME")
    [(or #f "") (build-path (find-system-path 'cache-dir) "racket-datasets")]
    [(? complete-path? home) (string->path home)]
    [home (raise-registry-error who "DATASETS_HOME is not an absolute path" "value" home)]))

(define (cache-file root name info)
  (define fetch (hash-ref info "fetch"))
  (define key (hash-ref (if (convert-fetch? fetch) info fetch) "sha256"))
  (build-path root name (format "~a.rktd.gz" key)))

(define (download-dataset who name info final #:download? download? #:idle-timeout idle)
  (define fetch (hash-ref info "fetch"))
  (define fetch-format (hash-ref fetch "format"))
  (cond
    [(not download?)
     (raise-registry-error who "dataset is not cached and downloads are disabled" "dataset" name
                           "reason" "#:download? #f" "cache file" final
                           "to download" (format "(fetch-dataset '~a)" name))]
    [(not (member fetch-format supported-formats))
     (raise-registry-error who "unsupported fetch format" "dataset" name "format" fetch-format
                           "supported" (format "~s" supported-formats)
                           "hint" "a newer datasets-core may support this format")]
    [(convert-fetch? fetch)
     (define converter (load-converter who name info))
     (convert-set who name info final converter idle)]
    [else
     (with-part-file [part who name final]
       (try-urls who (list "dataset" name) name (hash-ref fetch "urls") (hash-ref fetch "sha256") part idle)
       (with-cache-failure [who "cannot write the dataset cache" "dataset" name "file" final]
         (rename-file-or-directory part final #t))
       final)]))

(define-syntax-parse-rule (with-cache-failure [who:expr message:expr field:expr ...] body:expr ...+)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-registry-error who message field ... "error" (exn-message e)))])
    body ...))

(define-syntax-parse-rule (with-part-file [part:id who:expr name:expr final:expr] body:expr ...+)
  (with-part [part who name final (string-append (path->string (file-name-from final)) ".~a.part")]
    body ...))

(define-syntax-parse-rule (with-part [part:id who:expr name:expr beside:expr template:expr]
                            body:expr ...+)
  (with-resource [part (make-part-file who name beside template) (delete-if-present part)]
    body ...))

(define (file-name-from path)
  (define-values (_dir file _dir?) (split-path path))
  file)

(define (make-part-file who name beside template)
  (define-values (dir _file _dir?) (split-path beside))
  (with-cache-failure [who "cannot write the dataset cache" "dataset" name "directory" dir]
    (make-directory* dir)
    (make-temporary-file template #:base-dir dir)))

(define (try-urls who fields label urls sha256 part idle)
  (let loop ([urls urls] [failures '()])
    (match urls
      ['() (apply raise-registry-error who "cannot download dataset"
                  (append fields (append* (reverse failures))))]
      [(cons url more)
       (log-datasets-info "downloading ~a from ~a" label url)
       (match (with-handlers ([exn:fail? exn-message])
                (download-url url part idle)
                (hash-problem part sha256))
         [#f (void)]
         [problem (loop more (cons (list "url" url "error" problem) failures))])])))

(define (converter-ref info key)
  (hash-ref (hash-ref (hash-ref info "fetch") "converter") key))

(define (converter-fields name info)
  (list "dataset" name "package" (hash-ref info "package")
        "module" (converter-ref info "module") "binding" (converter-ref info "binding")))

(define (converter-module info)
  (string->symbol (converter-ref info "module")))

(define (load-converter who name info)
  (define binding (string->symbol (converter-ref info "binding")))
  (define value
    (with-handlers ([exn:fail? (lambda (e)
                                 (apply raise-registry-error who "cannot load converter"
                                        (append (converter-fields name info)
                                                (list "error" (exn-message e)))))])
      (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
        (dynamic-require (converter-module info) binding))))
  (contract converter/c value (converter-module info) 'datasets/core binding #f))

(define (convert-set who name info final converter idle)
  (let loop ([files (hash-ref (hash-ref info "fetch") "files")] [paths (hash)])
    (match files
      ['() (write-converted who name info final (converted-text who name info converter paths))]
      [(cons file more)
       (define file-name (hash-ref file "name"))
       (with-part [part who name final (format "~a.raw.~~a.part" (hash-ref file "sha256"))]
         (try-urls who (list "dataset" name "file" file-name) (format "~a file ~a" name file-name)
                   (hash-ref file "urls") (hash-ref file "sha256") part idle)
         (loop more (hash-set paths file-name part)))])))

(define (converted-text who name info converter files)
  (define fields (converter-fields name info))
  (define module (converter-module info))
  (define (converter-blame? e)
    (and (exn:fail:contract:blame? e)
         (equal? (blame-positive (exn:fail:contract:blame-object e)) module)))
  (define (guarded thunk)
    (with-handlers ([(lambda (e) (and (exn:fail? e) (not (converter-blame? e))))
                     (lambda (e)
                       (apply raise-registry-error who "converter failed"
                              (append fields (list "error" (exn-message e)))))])
      (thunk)))
  (define width (hash-ref info "column-count"))
  (define (well-formed? row)
    (and (list? row) (= (length row) width) (andmap row-value? row)))
  (log-datasets-info "converting ~a with ~a from ~a" name (converter-ref info "binding") module)
  (define-values (more? next) (guarded (lambda () (sequence-generate (converter files info)))))
  (define out (open-output-bytes))
  (write-string "(\n" out)
  (let loop ([index 0])
    (match (guarded (lambda () (and (more?) (list (next)))))
      [#f (void)]
      [(list (? well-formed? row))
       (write-row row out)
       (loop (add1 index))]
      [(list row)
       (apply raise-registry-error who "converter produced a malformed row"
              (append fields
                      (list "row" index
                            "expected" (format "a list of ~a values, each an exact integer, flonum, string or 'missing"
                                               width)
                            "given" (~e row))))]))
  (write-string ")\n" out)
  (get-output-bytes out #t))

(define (write-row row out)
  (write-string (string-join (map value->string row) " " #:before-first "  (" #:after-last ")\n") out))

(define (value->string value)
  (match value
    [(? string?) (~s value)]
    ['missing "missing"]
    [_ (number->string value)]))

(define (write-converted who name info final text)
  (define expected (hash-ref info "sha256"))
  (define actual (hex (sha256-bytes text)))
  (cond
    [(equal? actual expected)
     (with-part-file [part who name final]
       (with-cache-failure [who "cannot write the dataset cache" "dataset" name "file" final]
         (call-with-output-file part #:exists 'truncate
           (lambda (out) (gzip-through-ports (open-input-bytes text) out #f 0)))
         (rename-file-or-directory part final #t))
       final)]
    [else (apply raise-registry-error who "converted data sha256 mismatch"
                 (append (converter-fields name info) (list "expected" expected "actual" actual)))]))

(define (download-url url part idle)
  (with-custodian
    (define in (open-url (string->url url)))
    (call-with-output-file part #:exists 'truncate
      (lambda (out) (copy-until-idle in out idle)))))

(define (copy-until-idle in out idle)
  (define buffer (make-bytes 65536))
  (let loop ()
    (match (and (sync/timeout idle in) (read-bytes-avail! buffer in))
      [#f (fail (format "no data for ~a seconds" idle))]
      [(? eof-object?) (void)]
      [n (write-bytes buffer out 0 n) (loop)])))

(define (open-url u)
  (match (url-scheme u)
    [#f (fail "URL has no scheme")]
    [(app string-downcase "file") (open-input-file (url->path u))]
    [(app string-downcase (or "http" "https"))
     (define-values (in headers) (get-pure-port/headers u #:redirections 10 #:status? #t))
     (match headers
       [(regexp #px"^HTTP/\\S+ 2[0-9]{2}") in]
       [(regexp #px"^HTTP/\\S+ ([0-9]{3})" (list _ status))
        (close-input-port in)
        (fail (string-append "HTTP status " status))]
       [_ (close-input-port in) (fail "malformed HTTP status line")])]
    [scheme (fail (format "unsupported URL scheme ~a" scheme))]))

(define (fail message)
  (raise (exn:fail message (current-continuation-marks))))

(define (hash-problem part expected)
  (define actual (file-sha256 part))
  (cond
    [(equal? actual expected) #f]
    [else (format "sha256 mismatch: expected ~a, actual ~a" expected actual)]))

(define (delete-if-present file)
  (with-handlers ([exn:fail:filesystem? (lambda (e) (when (file-exists? file) (raise e)))])
    (delete-file file)))

(define (clear-cache-directory who name dir)
  (unless (link-exists? dir)
    (with-cache-failure [who "cannot clear the dataset cache" "dataset" name "directory" dir]
      (for ([file (in-list (if (directory-exists? dir) (directory-list dir) '()))]
            #:when (regexp-match? cache-file-rx (path->string file))
            #:when (file-exists? (build-path dir file)))
        (delete-if-present (build-path dir file)))
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory dir)))))

(define (clear-cache-root who root)
  (define subs
    (with-cache-failure [who "cannot clear the dataset cache" "directory" root]
      (if (directory-exists? root) (directory-list root) '())))
  (for ([sub (in-list subs)]
        #:when (regexp-match? name-rx (path->string sub))
        #:when (directory-exists? (build-path root sub)))
    (clear-cache-directory who (path->string sub) (build-path root sub))))
