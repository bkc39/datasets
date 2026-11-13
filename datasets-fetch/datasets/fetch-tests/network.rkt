#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core clear-dataset-cache dataset-cache-directory dataset-info dataset-missing?
                  dataset-names dataset-table-column dataset-table-names dataset-table-row-count fetch-dataset
                  load-dataset-table)
         (only-in datasets/fetch-tests/digest expect expect-= parity-problems)
         (only-in file/sha1 bytes->hex-string)
         (only-in net/url call/input-url get-pure-port string->url)
         (only-in racket/list append* append-map make-list remove-duplicates)
         (only-in racket/match match-define)
         (only-in racket/string string-join)
         (only-in racket/vector vector-count)
         (only-in threading ~>))
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
  (define (seconds thunk)
    (define start (current-inexact-milliseconds))
    (define result (thunk))
    (values result (/ (round (- (current-inexact-milliseconds) start)) 1000.0)))
  (define (url-sha256 url)
    (call/input-url (string->url url) (lambda (u) (get-pure-port u #:redirections 10))
                    (lambda (in) (bytes->hex-string (sha256-bytes in)))))

  (define (pinned-bytes-problems names)
    (define pins
      (remove-duplicates
       (for*/list ([name (in-list names)]
                   [file (in-list (~> name dataset-info (hash-ref "fetch") (hash-ref "files")))]
                   [url (in-list (hash-ref file "urls"))])
         (list name url (hash-ref file "sha256")))
       #:key cdr))
    (append*
     (for/list ([pin (in-list pins)])
       (match-define (list name url pinned) pin)
       (define-values (sha elapsed) (seconds (lambda () (url-sha256 url))))
       (printf "pinned bytes ~a ~a ~as\n" name url elapsed)
       (expect (list url "sha256") sha pinned))))

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

  (define (zip-problems)
    (define table (load-dataset-table 'zip #:download? #f))
    (define (column name) (dataset-table-column table name))
    (define (cells i names) (for/list ([name (in-list names)]) (vector-ref (column name) i)))
    (define digits (column "digit"))
    (define split (column "split"))
    (define negative-zero-rows
      (for*/list ([name (in-list (dataset-table-names table))]
                  [(v i) (in-parallel (column name) (in-naturals))]
                  #:when (eqv? v -0.0))
        i))
    (append (expect '(zip "row 0") (cells 0 '("digit" "pixel8" "pixel9" "pixel10")) '(6 -0.631 0.862 -0.167))
            (expect '(zip "row 9297") (cells 9297 '("digit" "split")) '(1 "test"))
            (expect '(zip "digit counts")
                    (for/list ([d (in-range 10)]) (vector-count (lambda (v) (= v d)) digits))
                    '(1553 1269 929 824 852 716 834 792 708 821))
            (expect '(zip "digit sum") (for/sum ([v (in-vector digits)]) v) 36188)
            (expect-= '(zip "pixel1 sum") (for/sum ([v (in-vector (column "pixel1"))]) v) -9265.046 1e-6)
            (expect '(zip "negative zeros") (length negative-zero-rows) 170)
            (expect '(zip "splits of the negative zeros")
                    (remove-duplicates (for/list ([i (in-list negative-zero-rows)]) (vector-ref split i)))
                    '("train"))))

  (define (counts column values)
    (for/list ([v (in-list values)]) (vector-count (lambda (x) (equal? x v)) column)))
  (define (range-list from to) (for/list ([k (in-range from (add1 to))]) k))

  (define (image-problems name row-checks label-counts)
    (define table (load-dataset-table name #:download? #f))
    (define (column c) (dataset-table-column table c))
    (append (append* (for/list ([check (in-list row-checks)])
                       (match-define (list row names expected) check)
                       (expect (list name "row" row) (for/list ([c (in-list names)]) (vector-ref (column c) row))
                               expected)))
            (append* (for/list ([c (in-list label-counts)])
                       (match-define (list label values expected) c)
                       (expect (list name label "counts") (counts (column label) values) expected)))))

  (define (cifar-10-train-problems)
    (define table (load-dataset-table 'cifar-10 #:download? #f))
    (define labels (dataset-table-column table "label"))
    (define split (dataset-table-column table "split"))
    (expect '(cifar-10 "train label counts")
            (for/list ([v (in-range 10)])
              (for/sum ([l (in-vector labels)] [s (in-vector split)]) (if (and (= l v) (equal? s "train")) 1 0)))
            (make-list 10 5000)))

  (define (convert-memory-problems name)
    (define info (dataset-info name))
    (define limit (+ (* 2 (hash-ref info "rows") (sub1 (hash-ref info "column-count"))) (* 256 1000 1000)))
    (clear-dataset-cache name)
    (define-values (result elapsed)
      (seconds (lambda () (call-with-memory-limit limit (lambda () (fetch-dataset name))))))
    (printf "~a convert under a ~a MB limit ~as: ~a\n" name (quotient limit 1000000) elapsed result)
    (expect (list name "converted under a memory limit" limit) (path? result) #t))

  (define red-8 (for/list ([k (in-range 1 9)]) (format "red~a" k)))
  (define (image-set-problems)
    (append (image-problems 'cifar-10
                            `((0 ("label" ,@red-8) (6 59 43 50 68 98 119 139 145))
                              (50000 ("label" ,@red-8) (3 158 159 165 166 160 156 162 159)))
                            `(("label" ,(range-list 0 9) ,(make-list 10 6000))))
            (cifar-10-train-problems)
            (image-problems 'cifar-100 '()
                            `(("label" ,(range-list 0 99) ,(make-list 100 600))
                              ("coarse-label" ,(range-list 0 19) ,(make-list 20 3000))))
            (image-problems 'kmnist '() `(("label" ,(range-list 0 9) ,(make-list 10 7000))))
            (image-problems 'emnist-balanced '() `(("label" ,(range-list 0 46) ,(make-list 47 2800))))
            (image-problems 'emnist-letters '() `(("label" ,(range-list 1 26) ,(make-list 26 5600))))
            (image-problems 'emnist-digits '() `(("label" ,(range-list 0 9) ,(make-list 10 28000))))))

  (define (linnerud-problems)
    (define table (load-dataset-table 'linnerud #:download? #f))
    (define names (dataset-table-names table))
    (define (row i) (for/list ([name (in-list names)]) (vector-ref (dataset-table-column table name) i)))
    (append (expect '(linnerud "row 0") (row 0) '(5 162 60 191 36 50))
            (expect '(linnerud "row 19") (row 19) '(2 110 43 138 33 68))
            (expect '(linnerud "column sums")
                    (for/list ([name (in-list names)])
                      (for/sum ([v (in-vector (dataset-table-column table name))]) v))
                    '(189 2911 1406 3572 708 1122))))

  (printf "DATASETS_HOME ~a\n" (dataset-cache-directory))
  (define names (dataset-names #:package "datasets-fetch"))
  (define problems
    (append (pinned-bytes-problems names) (append-map table-problems names)
            (bounded-load-problems 'fashion-mnist) (bounded-load-problems 'cifar-10) (zip-problems)
            (linnerud-problems) (image-set-problems) (convert-memory-problems 'cifar-10)))
  (if (null? problems)
      (printf "network checks hold for ~a\n" names)
      (error (string-join problems "\n" #:before-first "network checks failed:\n"))))
