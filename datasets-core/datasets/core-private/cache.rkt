#lang racket/base
(require (for-syntax racket/base (only-in syntax/parse ...+ expr id))
         (only-in datasets/core-private/codec file-sha256)
         (only-in datasets/core-private/error raise-registry-error)
         (only-in datasets/core-private/log log-datasets-info)
         (only-in datasets/core-private/resource with-custodian with-resource)
         (only-in net/url get-pure-port/headers string->url url->path url-scheme)
         (only-in racket/file make-directory* make-temporary-file)
         (only-in racket/list append*)
         (only-in racket/match match)
         (only-in syntax/parse/define define-syntax-parse-rule))
(provide cache-file cache-root clear-cache-directory clear-cache-root download-dataset)

(define name-rx #px"^[a-z0-9]+(-[a-z0-9]+)*$")
(define cache-file-rx #px"^[0-9a-f]{64}\\.rktd\\.gz(\\.[A-Za-z0-9]+\\.part)?$")
(define supported-formats '("rktd.gz"))

(define (cache-root who)
  (match (getenv "DATASETS_HOME")
    [(or #f "") (build-path (find-system-path 'cache-dir) "racket-datasets")]
    [(? complete-path? home) (string->path home)]
    [home (raise-registry-error who "DATASETS_HOME is not an absolute path" "value" home)]))

(define (cache-file root name info)
  (build-path root name (format "~a.rktd.gz" (hash-ref (hash-ref info "fetch") "sha256"))))

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
    [else
     (with-part-file [part who name final]
       (try-urls who name fetch part final idle))]))

(define-syntax-parse-rule (with-cache-failure [who:expr message:expr field:expr ...] body:expr ...+)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-registry-error who message field ... "error" (exn-message e)))])
    body ...))

(define-syntax-parse-rule (with-part-file [part:id who:expr name:expr final:expr] body:expr ...+)
  (with-resource [part (make-part-file who name final) (delete-if-present part)]
    body ...))

(define (make-part-file who name final)
  (define-values (dir file _dir?) (split-path final))
  (with-cache-failure [who "cannot write the dataset cache" "dataset" name "directory" dir]
    (make-directory* dir)
    (make-temporary-file (string-append (path->string file) ".~a.part") #:base-dir dir)))

(define (try-urls who name fetch part final idle)
  (let loop ([urls (hash-ref fetch "urls")] [failures '()])
    (match urls
      ['() (apply raise-registry-error who "cannot download dataset" "dataset" name
                  (append* (reverse failures)))]
      [(cons url more)
       (log-datasets-info "downloading ~a from ~a" name url)
       (match (with-handlers ([exn:fail? exn-message])
                (download-url url part idle)
                (hash-problem part (hash-ref fetch "sha256")))
         [#f (rename-file-or-directory part final #t) final]
         [problem (loop more (cons (list "url" url "error" problem) failures))])])))

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
