#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-cache-directory dataset-info dataset-missing? dataset-names
                  dataset-table-column dataset-table-names dataset-table-row-count fetch-dataset
                  load-dataset-table)
         (only-in datasets/fetch-tests/digest column-digest)
         (only-in file/sha1 bytes->hex-string)
         (only-in json read-json)
         (only-in net/url call/input-url get-pure-port string->url)
         (only-in racket/list append* append-map)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/string string-join)
         (only-in racket/vector vector-count)
         (only-in threading ~>))
(define-runtime-path parity-file "fixtures/parity.json")

(define gzip-isize-bytes 4)

(define (gzip-isize path)
  (call-with-input-file path
    (lambda (in)
      (file-position in (- (file-size path) gzip-isize-bytes))
      (integer-bytes->integer (read-bytes gzip-isize-bytes in) #f #f))))

(define (raised->string v)
  (if (exn? v) (exn-message v) (format "raised ~s" v)))

(define (call-with-memory-limit limit thunk)
  (collect-garbage)
  (define custodian (make-custodian))
  (custodian-limit-memory custodian limit custodian)
  (define result (box 'killed-at-the-memory-limit))
  (thread-wait (parameterize ([current-custodian custodian])
                 (thread (lambda () (set-box! result (with-handlers ([(lambda (_) #t) raised->string]) (thunk)))))))
  (custodian-shutdown-all custodian)
  (unbox result))

(module+ test
  (require (only-in datasets/fetch-tests/fixture call-with-upstream-files)
           (only-in rackunit check-equal? test-case))
  (define small-limit (* 16 1000 1000))
  (test-case "gzip-isize reads the uncompressed size from the gzip trailer"
    (call-with-upstream-files '(("a.gz" (gz "plain text\n")))
      (lambda (files) (check-equal? (gzip-isize (hash-ref files "a.gz")) 11))))
  (test-case "call-with-memory-limit returns an error's message, and its sentinel only when the limit kills"
    (check-equal? (call-with-memory-limit small-limit (lambda () (error 'x "boom"))) "x: boom")
    (check-equal? (call-with-memory-limit small-limit (lambda () (raise 'oops))) "raised oops")
    (check-equal? (call-with-memory-limit small-limit (lambda () 'done)) 'done))
  (test-case "call-with-memory-limit kills an allocating thunk however large the caller's heap"
    (define ballast (for/list ([_ (in-range 150)]) (make-bytes 1000000)))
    (check-equal? (call-with-memory-limit small-limit
                                          (lambda ()
                                            (define kept (for/list ([_ (in-range 320)]) (make-bytes 100000)))
                                            (collect-garbage)
                                            (length kept)))
                  'killed-at-the-memory-limit)
    (check-equal? (length ballast) 150)))

(module+ network
  (define parity (call-with-input-file parity-file read-json))
  (define (seconds thunk)
    (define start (current-inexact-milliseconds))
    (define result (thunk))
    (values result (/ (round (- (current-inexact-milliseconds) start)) 1000.0)))
  (define (expect what actual expected)
    (if (equal? actual expected) '() (list (format "~a: expected ~s, actual ~s" what expected actual))))
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

  (define (column-problems c column reference)
    (define name (hash-ref c "name"))
    (append (expect (list name "missing") (vector-count dataset-missing? column) (hash-ref c "missing-count"))
            (expect (list name "types") (for/and ([v (in-vector column)]) (value-ok? c v)) #t)
            (expect (list name "parity digest") (column-digest column)
                    (~> reference (hash-ref 'columns) (hash-ref (string->symbol name))))))

  (define (table-problems name)
    (define info (dataset-info name))
    (define-values (path fetch-time) (seconds (lambda () (fetch-dataset name))))
    (define-values (table load-time) (seconds (lambda () (load-dataset-table name #:download? #f))))
    (printf "~a fetch+convert ~as, load ~as, cached ~a bytes, peak ~a MB\n" name fetch-time load-time
            (file-size path) (quotient (current-memory-use 'peak) 1000000))
    (define reference (hash-ref parity name))
    (append (expect (list name "rows") (dataset-table-row-count table) (hash-ref info "rows"))
            (expect (list name "parity rows") (dataset-table-row-count table) (hash-ref reference 'rows))
            (expect (list name "names") (dataset-table-names table)
                    (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")))
            (append* (for/list ([c (in-list (hash-ref info "columns"))])
                       (column-problems c (dataset-table-column table (hash-ref c "name")) reference)))))

  (define bytes-per-value 8)
  (define value-copies 2)
  (define baseline-bytes (* 64 1000 1000))

  (define (bounded-load-problems name)
    (define info (dataset-info name))
    (define text-size (gzip-isize (fetch-dataset name)))
    (define (load-rows limit)
      (define-values (rows elapsed)
        (seconds (lambda ()
                   (call-with-memory-limit
                    limit (lambda () (dataset-table-row-count (load-dataset-table name #:download? #f)))))))
      (printf "~a load under a ~a MB limit ~as: ~a\n" name (quotient limit 1000000) elapsed rows)
      rows)
    (define limit
      (+ text-size (* value-copies (hash-ref info "rows") (hash-ref info "column-count") bytes-per-value)
         baseline-bytes))
    (append (expect (list name "rows loaded under a memory limit" limit) (load-rows limit)
                    (hash-ref info "rows"))
            (expect (list name "a limit below the text size stops the load")
                    (load-rows (quotient text-size 2))
                    'killed-at-the-memory-limit)))

  (printf "DATASETS_HOME ~a\n" (dataset-cache-directory))
  (define names (dataset-names #:package "datasets-fetch"))
  (define problems
    (append (append-map pinned-bytes-problems names) (append-map table-problems names)
            (bounded-load-problems 'fashion-mnist)))
  (if (null? problems)
      (printf "network checks hold for ~a\n" names)
      (error (string-join problems "\n" #:before-first "network checks failed:\n"))))
