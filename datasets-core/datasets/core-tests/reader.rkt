#lang racket/base
(require (only-in datasets/core dataset-info dataset-missing dataset-names dataset-table-columns
                  dataset-table-row-count fetch-dataset load-dataset-table)
         (only-in datasets/core-tests/convert-fixture long-row-count)
         (only-in racket/list range remove-duplicates)
         (only-in racket/match match match-define)
         (only-in racket/vector vector-copy vector-extend)
         (only-in "fixture.rkt" call-with-registry-fixtures call-with-temporary-fixture fetch-entry
                  fetch-error? gen-entry gunzip-file gzip-bytes hex raw-a raw-b rows->rktd-bytes
                  toy-artifact toy-at toy-rows toy-set-entry with-server write-registry-fixture!))
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
      (define info (dataset-info name))
      (define names (for/list ([c (in-list (hash-ref info "columns"))]) (hash-ref c "name")))
      (define text (gunzip-file (fetch-dataset name)))
      (define (reference indices)
        (call-with-default-reading-parameterization
         (lambda () (read-reference text indices (hash-ref info "rows")))))
      (define subset (remove-duplicates (list (sub1 (length names)) 0)))
      (define table (load-dataset-table name))
      (check-equal? (dataset-table-columns table) (reference (range (length names))) (symbol->string name))
      (check-equal? (dataset-table-row-count table) (hash-ref info "rows") (symbol->string name))
      (check-equal? (dataset-table-columns
                     (load-dataset-table name #:columns (map (lambda (i) (list-ref names i)) subset)))
                    (reference subset)
                    (symbol->string name))))

  (define toy-text (rows->rktd-bytes toy-rows))
  (define corruptions
    `((truncated-text ,(gzip-bytes #"(\n  (1 \"a\" 0.5)\n  (2") mismatch)
      (not-canonical ,(gzip-bytes #"garbage") mismatch)
      (trailing-text ,(gzip-bytes (bytes-append toy-text #"  (4 \"d\" 2.5)\n")) mismatch)
      (truncated-gzip ,(subbytes toy-artifact 0 30) mismatch)
      (cut-header ,(subbytes toy-artifact 0 10) unreadable)))
  (define tier-errors
    `((bundled (mismatch "data sha256 mismatch") (unreadable "cannot read dataset data") #f)
      (cached (mismatch "data sha256 mismatch") (unreadable "cannot read dataset data") #t)
      (pinned (mismatch "registry sha256 does not match the pinned download")
              (unreadable "cannot read the pinned download") #f)))

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
      (define message (cadr (assq kind (list mismatch unreadable))))
      (define raised (with-handlers ([exn:fail? values]) (load-corrupt tier data) #f))
      (define where (format "~a ~a" tier label))
      (check-true (and raised ((fetch-error? (string-append "load-dataset-table: " message)) raised)) where)
      (check-equal? (and raised (regexp-match? #rx"clear-dataset-cache" (exn-message raised))) remedy? where)
      (check-false (and raised (regexp-match? #rx"cannot parse" (exn-message raised))) where)))

  (test-case "a conversion streams its text: it runs under a memory limit far below the text's size"
    (define limit (* 16 1024 1024))
    (with-server (hash "/a" `(ok ,raw-a) "/b" `(ok ,raw-b))
                 (lambda (u) (list (toy-set-entry u #:binding "long-rows")))
      (lambda (srv root registry)
        (dataset-info 'toy-set)
        (collect-garbage)
        (define c (make-custodian))
        (custodian-limit-memory c limit c)
        (define ch (make-channel))
        (define collector (thread (lambda () (let loop () (collect-garbage) (sleep 0.05) (loop)))))
        (define t (parameterize ([current-custodian c])
                    (thread (lambda () (channel-put ch (with-handlers ([exn:fail? exn-message])
                                                         (fetch-dataset 'toy-set)))))))
        (define result (or (sync/timeout 300 ch (wrap-evt (thread-dead-evt t) (lambda (_) 'killed)))
                           'timeout))
        (kill-thread collector)
        (check-true (> (* 200 long-row-count) (* 2 limit)))
        (check-true (and (string? result)
                         (regexp-match? #rx"converted data sha256 mismatch" result))
                    (format "~a" result))))))
