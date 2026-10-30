#lang racket/base
(require (only-in compiler/find-exe find-exe)
         (only-in datasets/core dataset-info dataset-missing dataset-names dataset-table-columns
                  dataset-table-row-count fetch-dataset load-dataset-table)
         (only-in datasets/core-private/canonical read-canonical-columns write-canonical-rows)
         (only-in datasets/core-tests/convert-fixture long-row-count long-row-text)
         (only-in racket/list append-map first index-of last remove-duplicates)
         (only-in racket/match match match-define)
         (only-in racket/port call-with-output-bytes)
         (only-in racket/system system*/exit-code)
         (only-in racket/vector vector-copy vector-extend)
         (only-in "fixture.rkt" call-with-deep-fixture-directory call-with-memory-limit
                  call-with-registry-fixtures call-with-temporary-fixture call-with-timeout error-detail
                  fetch-entry fetch-error? gen-entry gunzip-file gzip-bytes hex raw-a raw-b rows->rktd-bytes
                  toy-artifact toy-at toy-rows toy-set-entry wait-until with-server write-registry-fixture!))
(module+ test
  (require (only-in rackunit check-equal? check-false check-true test-case))

  (define (read-reference text indices rows)
    (define in (open-input-bytes text))
    (regexp-try-match #px"^\\s*[(]" in)
    (let loop ([columns (for/vector ([_ (in-list indices)]) (make-vector rows))] [capacity rows] [count 0])
      (cond
        [(regexp-try-match #px"^\\s*[)]" in)
         (for/vector ([c (in-vector columns)])
           (vector->immutable-vector (vector-copy c 0 count)))]
        [(= count capacity)
         (define larger (max 16 (* 2 count)))
         (loop (for/vector ([c (in-vector columns)]) (vector-extend c larger)) larger count)]
        [else
         (define cells (list->vector (read in)))
         (for ([column (in-vector columns)] [i (in-list indices)])
           (vector-set! column count (match (vector-ref cells i)
                                       ['missing dataset-missing]
                                       [(? string? s) (string->immutable-string s)]
                                       [v v])))
         (loop columns capacity (add1 count))])))

  (define (bundled? name)
    (equal? (hash-ref (dataset-info name) "tier") "bundled"))

  (test-case "every installed bundled dataset loads as the reader read it"
    (for ([name (in-list (dataset-names))] #:when (bundled? name))
      (define label (symbol->string name))
      (define info (dataset-info name))
      (define names (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")))
      (define text (gunzip-file (fetch-dataset name)))
      (define (reference selected)
        (call-with-default-reading-parameterization
         (lambda () (read-reference text (map (lambda (n) (index-of names n)) selected) (hash-ref info "rows")))))
      (define subset (remove-duplicates (list (last names) (first names))))
      (define table (load-dataset-table name))
      (check-equal? (dataset-table-columns table) (reference names) label)
      (check-equal? (dataset-table-row-count table) (hash-ref info "rows") label)
      (check-equal? (dataset-table-columns (load-dataset-table name #:columns subset)) (reference subset) label)))

  (define toy-text (rows->rktd-bytes toy-rows))
  (define corruptions
    `((truncated-text ,(gzip-bytes #"(\n  (1 \"a\" 0.5)\n  (2") mismatch)
      (not-canonical ,(gzip-bytes #"garbage") mismatch)
      (trailing-text ,(gzip-bytes (bytes-append toy-text #"  (4 \"d\" 2.5)\n")) mismatch)
      (truncated-gzip ,(subbytes toy-artifact 0 30) mismatch)
      (huge-exponent ,(gzip-bytes #"(\n  (#e1e100000000 #e1e100000000 #e1e100000000)\n)\n") mismatch)
      (long-decimal ,(gzip-bytes (bytes-append #"(\n  (0." (make-bytes 6400000 49) #" 1 2)\n)\n")) mismatch)
      (cut-header ,(subbytes toy-artifact 0 10) unreadable)))
  (define tier-errors
    '((bundled "data sha256 mismatch" "cannot read dataset data" #f)
      (cached "data sha256 mismatch" "cannot read dataset data" #t)
      (pinned "registry sha256 does not match the pinned download" "cannot read the pinned download" #f)))
  (define load-seconds 10)

  (define (load-corrupt tier data)
    (match tier
      ['bundled
       (call-with-temporary-fixture
        (lambda (dir)
          (write-registry-fixture!
           dir #:info '("registry.rktd") #:data (hash "gen" data)
           #:registries (hash "registry.rktd"
                              (hash "gen" (gen-entry "gen" #:sha256 (hex (sha256-bytes toy-text))))))
          (call-with-registry-fixtures (list dir) (lambda () (load-dataset-table 'gen)))))]
      ['cached
       (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
         (lambda (srv root registry)
           (call-with-output-file (fetch-dataset 'toy-remote) #:exists 'truncate
             (lambda (o) (write-bytes data o)))
           (load-dataset-table 'toy-remote)))]
      ['pinned
       (with-server (hash "/toy" `(ok ,data))
                    (lambda (u) (list (fetch-entry (list (u "/toy")) #:sha256 (hex (sha256-bytes data)))))
         (lambda (srv root registry) (load-dataset-table 'toy-remote)))]))

  (test-case "corrupt data that also does not parse reports its sha256 or gzip error, never a parse error"
    (for* ([corruption (in-list corruptions)] [tier-error (in-list tier-errors)])
      (match-define (list label data kind) corruption)
      (match-define (list tier mismatch unreadable remedy?) tier-error)
      (define message (if (eq? kind 'mismatch) mismatch unreadable))
      (define raised
        (call-with-timeout load-seconds (lambda () (with-handlers ([exn:fail? values]) (load-corrupt tier data) #f))))
      (define where (format "~a ~a ~a" tier label raised))
      (check-true (and (exn? raised) ((fetch-error? (string-append "load-dataset-table: " message)) raised)) where)
      (check-equal? (and (exn? raised) (regexp-match? #rx"clear-dataset-cache" (exn-message raised))) remedy? where)
      (check-false (and (exn? raised) (regexp-match? #rx"cannot parse" (exn-message raised))) where)))

  (define conversion-seconds 300)
  (test-case "a conversion streams its text: it runs under a memory limit far below the text's size"
    (define limit (* 16 1024 1024))
    (with-server (hash "/a" `(ok ,raw-a) "/b" `(ok ,raw-b))
                 (lambda (u) (list (toy-set-entry u #:binding "long-rows")))
      (lambda (srv root registry)
        (dataset-info 'toy-set)
        (define result
          (call-with-memory-limit limit conversion-seconds
                                  (lambda () (with-handlers ([exn:fail? exn-message]) (fetch-dataset 'toy-set)))))
        (check-true (> (* (string-length long-row-text) long-row-count) (* 2 limit)))
        (check-true (and (string? result) (regexp-match? #rx"converted data sha256 mismatch" result))
                    (format "~a" result)))))

  (define (call-with-gen-fixture text entry proc)
    (call-with-temporary-fixture
     (lambda (dir)
       (write-registry-fixture!
        dir #:info '("registry.rktd") #:data (hash "gen" (gzip-bytes text))
        #:registries (hash "registry.rktd" (hash "gen" (hash-set entry "sha256" (hex (sha256-bytes text))))))
       (proc dir))))

  (test-case "rows beyond the cell budget grow the columns, whatever the registry row count"
    (define budget 16)
    (for* ([n (in-list '(15 16 17 40))]
           [stated (in-list (list 0 (sub1 n) n (add1 n) (* 4 n)))]
           [indices (in-list '((1) (0 1)))])
      (define text (rows->rktd-bytes (for/list ([i (in-range n)]) (list i (- i)))))
      (check-equal? (read-canonical-columns (open-input-bytes text) indices 2 stated budget)
                    (cons (read-reference text indices stated) n)
                    (format "~a rows, ~a stated, columns ~a" n stated indices))))

  (define (flonum-bits x) (integer-bytes->integer (real->floating-point-bytes x 8) #t))
  (define (bits-flonum n) (floating-point-bytes->real (integer->integer-bytes n 8 #t)))
  (define (flonum-step x d) (bits-flonum (+ (flonum-bits x) d)))
  (define boundary-flonums
    (append '(0.0 5e-324 1e-320 1e21 1e+20 +inf.0 +nan.0)
            (for*/list ([k (in-range -330 310)] [d (in-list '(-1 0 1))])
              (flonum-step (string->number (format "1e~a" k)) d))))
  (define random-flonums
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 42)
      (for/list ([_ (in-range 100000)])
        (floating-point-bytes->real (apply bytes (for/list ([_ (in-range 8)]) (random 256)))))))

  (test-case "every flonum loads back as written"
    (define flonums (append boundary-flonums (map - boundary-flonums) random-flonums))
    (define text (call-with-output-bytes (lambda (out) (write-canonical-rows (map list flonums) 1 out))))
    (match-define (cons (vector column) count)
      (read-canonical-columns (open-input-bytes text) '(0) 1 (length flonums)))
    (check-equal? count (length flonums))
    (check-equal? (for/list ([v (in-list flonums)] [r (in-vector column)] #:unless (eqv? v r)) (list v r)) '()))

  (test-case "bundled data with its sha256 and a 1001-digit integer cannot be parsed, under any fixture path"
    (call-with-deep-fixture-directory 150
     (lambda ()
       (call-with-gen-fixture (bytes-append #"(\n  (" (make-bytes 1001 49) #")\n)\n") (gen-entry "gen")
         (lambda (dir)
           (define message
             (call-with-registry-fixtures (list dir)
               (lambda () (with-handlers ([exn:fail? exn-message]) (load-dataset-table 'gen) "loaded"))))
           (check-true (regexp-match? #rx"^load-dataset-table: cannot parse dataset data\n" message) message)
           (check-equal? (error-detail message)
                         (string-append "row 0: unexpected value " (make-string 40 #\1) "…")))))))

  (define linux? (eq? (system-type 'os*) 'linux))
  (define (open-fds) (directory-list "/proc/self/fd" #:build? #t))
  (define (data-fds)
    (filter (lambda (fd) (regexp-match? #rx"[.]rktd[.]gz$" (path->string (resolve-path fd)))) (open-fds)))
  (define (live-threads custodian)
    (append-map (lambda (v)
                  (cond
                    [(custodian? v) (live-threads v)]
                    [(and (thread? v) (not (thread-dead? v))) (list v)]
                    [else '()]))
                (custodian-managed-list custodian (current-custodian))))

  (test-case "killing a load closes its file and stops its workers"
    (define n 1000000)
    (when linux?
      (call-with-gen-fixture (rows->rktd-bytes (for/list ([i (in-range n)]) (list i)))
                             (hash-set (gen-entry "gen") "rows" n)
        (lambda (dir)
          (call-with-registry-fixtures (list dir)
            (lambda ()
              (dataset-info 'gen)
              (define before (length (open-fds)))
              (define loads (make-custodian))
              (define finished? (box #f))
              (define t (parameterize ([current-custodian loads])
                          (thread (lambda () (load-dataset-table 'gen) (set-box! finished? #t)))))
              (check-true (wait-until load-seconds (lambda () (pair? (data-fds)))))
              (kill-thread t)
              (check-false (unbox finished?))
              (check-true (wait-until load-seconds
                                      (lambda () (and (null? (data-fds)) (null? (live-threads loads))))))
              (check-equal? (length (open-fds)) before)))))))

  (define subprocess-memory-kb (* 3 1024 1024))
  (test-case "a registry row count far beyond the data neither aborts nor preallocates it"
    (when linux?
      (call-with-gen-fixture (rows->rktd-bytes '((1) (2) (3))) (hash-set (gen-entry "gen") "rows" (expt 10 11))
        (lambda (dir)
          (define program
            `(write (call-with-registry-fixtures
                     (list (string->path ,(path->string dir)))
                     (lambda () (dataset-table-row-count (load-dataset-table 'gen))))))
          (define out (open-output-string))
          (define status
            (parameterize ([current-output-port out])
              (system*/exit-code "/bin/sh" "-c" (format "ulimit -v ~a && exec \"$@\"" subprocess-memory-kb)
                                 "sh" (find-exe) "-l" "racket/base" "-l" "datasets/core"
                                 "-l" "datasets/core-tests/fixture" "-e" (format "~s" program))))
          (check-equal? (list status (get-output-string out)) '(0 "3")))))))
