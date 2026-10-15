#lang racket/base
(require (for-syntax racket/base (only-in syntax/parse ...+ expr id))
         (only-in datasets/core-private/codec file-sha256 gunzip-sha256)
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

(define (offline-value)
  (match (getenv "DATASETS_OFFLINE")
    [(or #f "") #f]
    [value (and (not (member (string-downcase value) '("0" "false" "no" "off"))) value)]))

(define (download-dataset who name info final #:download? download? #:idle-timeout idle)
  (define fetch (hash-ref info "fetch"))
  (define offline (offline-value))
  (define fetch-format (hash-ref fetch "format"))
  (cond
    [(or offline (not download?))
     (raise-registry-error
      who "dataset is not cached and downloads are disabled" "dataset" name
      "reason" (if offline (string-append "DATASETS_OFFLINE=" offline) "#:download? #f")
      "cache file" final
      "to download" (string-append "(fetch-dataset '" name ")"
                                   (if offline " with DATASETS_OFFLINE unset" "")))]
    [(not (member fetch-format supported-formats))
     (raise-registry-error who "unsupported fetch format" "dataset" name "format" fetch-format
                           "supported" (format "~s" supported-formats)
                           "hint" "a newer datasets-core may support this format")]
    [else
     (define-values (dir _file _dir?) (split-path final))
     (make-directory* dir)
     (with-part-file [part final]
       (try-urls who name info part final idle))]))

(define-syntax-parse-rule (with-part-file [part:id final:expr] body:expr ...+)
  (with-resource [part (make-part-file final) (delete-if-present part)]
    body ...))

(define (make-part-file final)
  (define-values (dir file _dir?) (split-path final))
  (make-temporary-file (string-append (path->string file) ".~a.part") #:base-dir dir))

(define (try-urls who name info part final idle)
  (define fetch (hash-ref info "fetch"))
  (let loop ([urls (hash-ref fetch "urls")] [failures '()])
    (match urls
      ['() (apply raise-registry-error who "cannot download dataset" "dataset" name
                  (append* (reverse failures)))]
      [(cons url more)
       (log-datasets-info "downloading ~a from ~a (~a bytes)" name url (hash-ref fetch "size"))
       (match (or (download-url url part (hash-ref fetch "size") idle)
                  (with-handlers ([exn:fail? exn-message])
                    (verification-problem part fetch (hash-ref info "sha256"))))
         [#f (install! part final)]
         [problem (loop more (cons (list "url" url "error" problem) failures))])])))

(define (download-url url part size idle)
  (define progress (make-semaphore 0))
  (define outcome (box #f))
  (with-custodian
    (define worker
      (thread (lambda ()
                (set-box! outcome (with-handlers ([exn:fail? exn-message])
                                    (copy-url url part size progress)
                                    'done)))))
    (let loop ()
      (match (sync/timeout idle (wrap-evt progress (lambda (_) 'progress)) (thread-dead-evt worker))
        ['progress (loop)]
        [#f (format "no data for ~a seconds" idle)]
        [_ (match (unbox outcome)
             ['done #f]
             [#f "download stopped"]
             [problem problem])]))))

(define (copy-url url part size progress)
  (define in (open-url (string->url url)))
  (define buffer (make-bytes 65536))
  (call-with-output-file part #:exists 'truncate
    (lambda (out)
      (let loop ([received 0])
        (define n (read-bytes-avail! buffer in))
        (cond
          [(and (eof-object? n) (= received size)) (void)]
          [(eof-object? n)
           (fail (format "size mismatch: expected ~a bytes, received ~a" size received))]
          [(> (+ received n) size)
           (fail (format "size mismatch: expected ~a bytes, received more than ~a" size size))]
          [else
           (write-bytes buffer out 0 n)
           (semaphore-post progress)
           (loop (+ received n))])))))

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

(define (verification-problem part fetch content-sha256)
  (define expected (hash-ref fetch "sha256"))
  (define actual (file-sha256 part))
  (cond
    [(equal? actual expected) (content-problem part content-sha256)]
    [else (format "sha256 mismatch: expected ~a, actual ~a" expected actual)]))

(define (content-problem part expected)
  (define actual (gunzip-sha256 part))
  (cond
    [(equal? actual expected) #f]
    [else (format "content sha256 mismatch: expected ~a, actual ~a" expected actual)]))

(define (install! part final)
  (with-handlers ([exn:fail:filesystem? (lambda (e)
                                          (cond [(file-exists? final) (delete-if-present part)]
                                                [else (raise e)]))])
    (rename-file-or-directory part final #t))
  final)

(define (delete-if-present file)
  (with-handlers ([exn:fail:filesystem? (lambda (e) (when (file-exists? file) (raise e)))])
    (delete-file file)))

(define (clear-cache-directory dir)
  (for ([file (in-list (if (directory-exists? dir) (directory-list dir) '()))]
        #:when (regexp-match? cache-file-rx (path->string file))
        #:when (file-exists? (build-path dir file)))
    (delete-if-present (build-path dir file)))
  (with-handlers ([exn:fail:filesystem? void])
    (delete-directory dir)))

(define (clear-cache-root root)
  (for ([sub (in-list (if (directory-exists? root) (directory-list root) '()))]
        #:when (regexp-match? name-rx (path->string sub))
        #:unless (link-exists? (build-path root sub))
        #:when (directory-exists? (build-path root sub)))
    (clear-cache-directory (build-path root sub))))
