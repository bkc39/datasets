#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-cache-directory dataset-info dataset-missing? dataset-names
                  dataset-table-column dataset-table-names dataset-table-row-count fetch-dataset
                  load-dataset-table)
         (only-in datasets/fetch-tests/digest column-digest)
         (only-in file/sha1 bytes->hex-string)
         (only-in net/url call/input-url get-pure-port string->url)
         (only-in racket/file file->value)
         (only-in racket/list append* append-map)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/string string-join)
         (only-in racket/vector vector-count))
(define-runtime-path parity-file "fixtures/parity.rktd")
(module+ network
  (define parity (file->value parity-file))
  (define (seconds thunk)
    (define start (current-inexact-milliseconds))
    (define result (thunk))
    (values result (/ (round (- (current-inexact-milliseconds) start)) 1000.0)))
  (define (expect law what actual expected)
    (if (equal? actual expected) '() (list (format "~a ~a: expected ~s, actual ~s" law what expected actual))))
  (define (url-sha256 url)
    (call/input-url (string->url url) (lambda (u) (get-pure-port u #:redirections 10))
                    (lambda (in) (bytes->hex-string (sha256-bytes in)))))

  (define (pinned-bytes-problems name)
    (append*
     (for*/list ([file (in-list (hash-ref (hash-ref (dataset-info name) "fetch") "files"))]
                 [url (in-list (hash-ref file "urls"))])
       (define-values (sha elapsed) (seconds (lambda () (url-sha256 url))))
       (printf "N1 ~a ~a ~as\n" name url elapsed)
       (expect 'N1 url sha (hash-ref file "sha256")))))

  (define (value-ok? c v)
    (or (dataset-missing? v)
        (case (hash-ref c "type")
          [("integer") (exact-integer? v)]
          [("real") (flonum? v)]
          [("categorical") (and (member v (hash-ref c "levels")) #t)]
          [else (string? v)])))

  (define (column-problems c column reference)
    (define name (hash-ref c "name"))
    (append (expect 'N2 (list name "missing") (vector-count dataset-missing? column) (hash-ref c "missing-count"))
            (expect 'N2 (list name "types") (for/and ([v (in-vector column)]) (value-ok? c v)) #t)
            (expect 'N3 (list name "digest") (column-digest column) (hash-ref (hash-ref reference "columns") name))))

  (define (table-problems name)
    (define info (dataset-info name))
    (define-values (path fetch-time) (seconds (lambda () (fetch-dataset name))))
    (define-values (table load-time) (seconds (lambda () (load-dataset-table name #:download? #f))))
    (printf "N2 ~a fetch+convert ~as, load ~as, cached ~a bytes, peak ~a MB\n" name fetch-time load-time
            (file-size path) (quotient (current-memory-use 'peak) 1000000))
    (define reference (hash-ref parity (symbol->string name)))
    (append (expect 'N2 (list name "rows") (dataset-table-row-count table) (hash-ref info "rows"))
            (expect 'N3 (list name "rows") (dataset-table-row-count table) (hash-ref reference "rows"))
            (expect 'N2 (list name "names") (dataset-table-names table)
                    (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")))
            (append* (for/list ([c (in-list (hash-ref info "columns"))])
                       (column-problems c (dataset-table-column table (hash-ref c "name")) reference)))))

  (printf "DATASETS_HOME ~a\n" (dataset-cache-directory))
  (define names (dataset-names #:package "datasets-fetch"))
  (define problems
    (append (append-map pinned-bytes-problems names) (append-map table-problems names)))
  (if (null? problems)
      (printf "N1-N3 hold for ~a\n" names)
      (raise (exn:fail (string-join problems "\n" #:before-first "network laws failed:\n")
                       (current-continuation-marks)))))
