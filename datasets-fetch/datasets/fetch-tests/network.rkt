#lang racket/base
(require (only-in datasets/core dataset-cache-directory dataset-info dataset-missing? dataset-names
                  dataset-table-column dataset-table-names dataset-table-row-count fetch-dataset
                  load-dataset-table)
         (only-in datasets/fetch-tests/digest expect parity-problems)
         (only-in file/sha1 bytes->hex-string)
         (only-in net/url call/input-url get-pure-port string->url)
         (only-in racket/list append* append-map)
         (only-in racket/string string-join)
         (only-in racket/vector vector-count)
         (only-in threading ~>))
(module+ network
  (define (seconds thunk)
    (define start (current-inexact-milliseconds))
    (define result (thunk))
    (values result (/ (round (- (current-inexact-milliseconds) start)) 1000.0)))
  (define (url-sha256 url)
    (call/input-url (string->url url) (lambda (u) (get-pure-port u #:redirections 10))
                    (lambda (in) (bytes->hex-string (sha256-bytes in)))))

  (define (pinned-bytes-problems name)
    (append*
     (for*/list ([file (in-list (~> name dataset-info (hash-ref "fetch") (hash-ref "files")))]
                 [url (in-list (hash-ref file "urls"))])
       (define-values (sha elapsed) (seconds (lambda () (url-sha256 url))))
       (printf "pinned bytes ~a ~a ~as\n" name url elapsed)
       (expect (list url "sha256") sha (hash-ref file "sha256")))))

  (define (value-ok? c v)
    (or (dataset-missing? v)
        (case (hash-ref c "type")
          [("integer") (exact-integer? v)]
          [("real") (flonum? v)]
          [("categorical") (and (member v (hash-ref c "levels")) #t)]
          [else (string? v)])))

  (define (column-problems c column)
    (define name (hash-ref c "name"))
    (append (expect (list name "missing") (vector-count dataset-missing? column) (hash-ref c "missing-count"))
            (expect (list name "types") (for/and ([v (in-vector column)]) (value-ok? c v)) #t)))

  (define (table-problems name)
    (define info (dataset-info name))
    (define-values (path fetch-time) (seconds (lambda () (fetch-dataset name))))
    (define-values (table load-time) (seconds (lambda () (load-dataset-table name #:download? #f))))
    (printf "~a fetch+convert ~as, load ~as, cached ~a bytes, peak ~a MB\n" name fetch-time load-time
            (file-size path) (quotient (current-memory-use 'peak) 1000000))
    (append (expect (list name "rows") (dataset-table-row-count table) (hash-ref info "rows"))
            (parity-problems name table)
            (expect (list name "names") (dataset-table-names table)
                    (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")))
            (append* (for/list ([c (in-list (hash-ref info "columns"))])
                       (column-problems c (dataset-table-column table (hash-ref c "name")))))))

  (printf "DATASETS_HOME ~a\n" (dataset-cache-directory))
  (define names (dataset-names #:package "datasets-fetch"))
  (define problems
    (append (append-map pinned-bytes-problems names) (append-map table-problems names)))
  (if (null? problems)
      (printf "network checks hold for ~a\n" names)
      (error (string-join problems "\n" #:before-first "network checks failed:\n"))))
