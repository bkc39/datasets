#lang racket/base
(require (only-in datasets/core clear-dataset-cache dataset-cache-directory dataset-cached? dataset-info
                  dataset-missing dataset-names dataset-table-column dataset-table-row-count fetch-dataset
                  load-dataset-table)
         (only-in datasets/core-tests/convert-fixture converter-calls converter-started-evt
                  edge-table-rows reset-converter! set-malformed-row!)
         (only-in racket/contract/combinator exn:fail:contract:blame?)
         (only-in racket/file make-directory*)
         (only-in racket/list drop take)
         (only-in racket/match match match-define)
         (only-in racket/path path-only)
         (only-in "fixture.rkt" cache-files call-with-artifact-url call-with-environment
                  call-with-fetch-fixture fetch-error? gunzip-file gzip-bytes hex raw-a raw-a-sha256 raw-b
                  raw-b-sha256 rows->rktd-bytes set-entry table-digest toy-content-sha256 toy-rows
                  toy-table-digest wide-entry with-server)
         (only-in "http-fixture.rkt" http-fixture-closed-evt http-fixture-requests
                  http-fixture-stalled-evt http-fixture-url))
(module+ test
  (require (only-in rackunit check-eq? check-equal? check-exn check-false check-not-false check-pred
                    check-true test-case))
  (define final-rel (format "toy-set/~a.rktd.gz" toy-content-sha256))
  (define raw-part-rx #px"^toy-set/[0-9a-f]{64}\\.raw\\.[A-Za-z0-9]+\\.part$")
  (define routes (hash "/a" `(ok ,raw-a) "/b" `(ok ,raw-b)))
  (define (raw-parts root) (filter (lambda (f) (regexp-match? raw-part-rx f)) (cache-files root)))
  (define ((converter-error? . rxs) e)
    (and ((apply fetch-error? rxs) e) (not (regexp-match? #rx"clear-dataset-cache" (exn-message e)))))
  (define ((blames-converter?) e)
    (and (exn:fail:contract:blame? e) (regexp-match? #rx"blaming: [^\n]*convert-fixture" (exn-message e))))
  (define ((set-with binding #:module [module "datasets/core-tests/convert-fixture"]
                     #:data-sha256 [sha toy-content-sha256]) u)
    (list (set-entry (list (list "a.rktd" (list (u "/a")) raw-a-sha256)
                           (list "b.rktd" (list (u "/b")) raw-b-sha256))
                     #:binding binding #:module module #:data-sha256 sha)))
  (define (fetch-thread)
    (define result (make-channel))
    (values result (thread (lambda () (channel-put result (with-handlers ([exn:break? values])
                                                            (fetch-dataset 'toy-set)))))))

  (test-case "S1 miss: each file once in order, only the converted file remains, then hits"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (reset-converter!)
        (check-false (dataset-cached? 'toy-set))
        (define p (fetch-dataset 'toy-set))
        (check-equal? p (build-path root final-rel))
        (check-equal? (cache-files root) (list final-rel))
        (check-equal? (gunzip-file p) (rows->rktd-bytes toy-rows))
        (check-equal? (http-fixture-requests srv) '("/a" "/b"))
        (check-true (dataset-cached? 'toy-set))
        (check-equal? (table-digest (load-dataset-table 'toy-set #:download? #f)) toy-table-digest)
        (check-equal? (fetch-dataset 'toy-set) p)
        (check-equal? (http-fixture-requests srv) '("/a" "/b"))
        (check-equal? (length (converter-calls)) 1))))

  (test-case "S2 the converter gets each verified file by name beside the cache file, and the info"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (reset-converter!)
        (fetch-dataset 'toy-set)
        (match-define (list (list immutable-files? seen info)) (converter-calls))
        (check-true immutable-files?)
        (check-equal? (for/hash ([(n s) (in-hash seen)]) (values n (cadr s)))
                      (hash "a.rktd" raw-a-sha256 "b.rktd" raw-b-sha256))
        (for ([s (in-hash-values seen)])
          (check-true (complete-path? (car s)))
          (check-equal? (path-only (car s)) (path-only (build-path root final-rel)))
          (check-false (file-exists? (car s))))
        (check-equal? info (dataset-info 'toy-set)))))

  (test-case "S3 each file falls through its own mirrors in order"
    (with-server (hash "/a404" '(status 404) "/a" `(ok ,raw-a) "/bbad" `(ok ,raw-a)
                       "/rb" '(redirect "/b") "/b" `(ok ,raw-b))
                 (lambda (u) (list (set-entry (list (list "a.rktd" (list (u "/a404") (u "/a")) raw-a-sha256)
                                                    (list "b.rktd" (list (u "/bbad") (u "/rb")) raw-b-sha256)))))
      (lambda (srv root registry)
        (check-equal? (fetch-dataset 'toy-set) (build-path root final-rel))
        (check-equal? (http-fixture-requests srv) '("/a404" "/a" "/bbad" "/rb" "/b"))
        (check-equal? (cache-files root) (list final-rel)))))

  (test-case "S4 a file whose every URL fails: the error names it, no conversion, nothing cached"
    (with-server (hash "/a" `(ok ,raw-a) "/bbad" `(ok ,raw-a))
                 (lambda (u) (list (set-entry (list (list "a.rktd" (list (u "/a")) raw-a-sha256)
                                                    (list "b.rktd" (list (u "/bbad") (u "/missing"))
                                                          raw-b-sha256)))))
      (lambda (srv root registry)
        (reset-converter!)
        (for ([who (in-list (list fetch-dataset load-dataset-table))]
              [rx (in-list (list #rx"^fetch-dataset: cannot download dataset\n"
                                 #rx"^load-dataset-table: cannot download dataset\n"))])
          (check-exn (fetch-error? rx "\n  dataset: toy-set\n  file: b.rktd\n"
                                   (http-fixture-url srv "/bbad") "sha256 mismatch" raw-b-sha256 raw-a-sha256
                                   (http-fixture-url srv "/missing") "HTTP status 404")
                     (lambda () (who 'toy-set))))
        (check-equal? (converter-calls) '())
        (check-equal? (cache-files root) '())
        (check-false (dataset-cached? 'toy-set))
        (check-equal? (http-fixture-requests srv) '("/a" "/bbad" "/missing" "/a" "/bbad" "/missing")))))

  (test-case "S5 a converter that raises: E3 without remedy; a non-exn raise propagates; nothing cached"
    (with-server routes (set-with "failing-rows")
      (lambda (srv root registry)
        (check-exn (converter-error? #rx"^fetch-dataset: converter failed\n" "dataset: toy-set"
                                     "package: datasets-fetch-fixture"
                                     "module: datasets/core-tests/convert-fixture" "binding: failing-rows"
                                     "upstream format changed")
                   (lambda () (fetch-dataset 'toy-set)))
        (check-equal? (cache-files root) '())))
    (with-server routes (set-with "raising-rows")
      (lambda (srv root registry)
        (check-eq? (with-handlers ([symbol? values]) (fetch-dataset 'toy-set)) 'upstream)
        (check-equal? (cache-files root) '()))))

  (test-case "S6 content that differs from the pin: E5 with expected and actual, no remedy"
    (with-server routes (set-with "concat-rows" #:data-sha256 (make-string 64 #\0))
      (lambda (srv root registry)
        (check-exn (converter-error? #rx"^fetch-dataset: converted data sha256 mismatch\n"
                                     "dataset: toy-set" "binding: concat-rows"
                                     (string-append "expected: " (make-string 64 #\0))
                                     (string-append "actual: " toy-content-sha256))
                   (lambda () (fetch-dataset 'toy-set)))
        (check-equal? (cache-files root) '())
        (check-false (dataset-cached? 'toy-set)))))

  (test-case "S7 a malformed row names its index; nothing cached"
    (for ([bad (in-list '((2 "c") (2 "c" 1.5 4) (2 "c" 1/2) (2 c 1.5) (2 "c" #t) #(2 "c" 1.5)
                          (2 "c" (1.5)) 2))])
      (with-server routes (set-with "malformed-rows")
        (lambda (srv root registry)
          (set-malformed-row! bad)
          (check-exn (converter-error? #rx"^fetch-dataset: converter produced a malformed row\n"
                                       "dataset: toy-set" "binding: malformed-rows" "row: 1")
                     (lambda () (fetch-dataset 'toy-set)))
          (check-equal? (cache-files root) '())))))

  (test-case "S8 breaking the protocol is a contract violation blaming the converter module"
    (for ([binding (in-list '("wrong-arity" "not-a-procedure"))])
      (with-server routes (set-with binding)
        (lambda (srv root registry)
          (check-exn (blames-converter?) (lambda () (fetch-dataset 'toy-set)))
          (check-equal? (http-fixture-requests srv) '())
          (check-equal? (cache-files root) '()))))
    (with-server routes (set-with "non-sequence-rows")
      (lambda (srv root registry)
        (check-exn (blames-converter?) (lambda () (fetch-dataset 'toy-set)))
        (check-equal? (cache-files root) '()))))

  (test-case "S9 a converter that cannot be loaded: E1 before any request; the registry stays valid"
    (for ([c (in-list '(("datasets/core-tests/no-such-converter" "concat-rows")
                        ("datasets/core-tests/convert-fixture" "no-such-binding")))])
      (match-define (list module binding) c)
      (with-server routes (set-with binding #:module module)
        (lambda (srv root registry)
          (check-not-false (memq 'toy-set (dataset-names)))
          (check-equal? (hash-ref (hash-ref (hash-ref (dataset-info 'toy-set) "fetch") "converter") "module")
                        module)
          (check-exn (converter-error? #rx"^fetch-dataset: cannot load converter\n" "dataset: toy-set"
                                       (string-append "module: " module) (string-append "binding: " binding))
                     (lambda () (fetch-dataset 'toy-set)))
          (check-equal? (http-fixture-requests srv) '())
          (check-false (directory-exists? (build-path root "toy-set")))))))

  (test-case "S10 #:download? #f refuses a set before any request or conversion"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (reset-converter!)
        (check-exn (fetch-error? #rx"^load-dataset-table: dataset is not cached" "toy-set" "#:download? #f"
                                 "(fetch-dataset 'toy-set)")
                   (lambda () (load-dataset-table 'toy-set #:download? #f)))
        (check-equal? (http-fixture-requests srv) '())
        (check-equal? (converter-calls) '())
        (check-equal? (cache-files root) '()))))

  (test-case "S11 a break during conversion removes every temporary file"
    (with-server routes (set-with "stalling-rows")
      (lambda (srv root registry)
        (reset-converter!)
        (define-values (result t) (fetch-thread))
        (check-not-false (sync/timeout 10 (converter-started-evt)))
        (check-equal? (length (raw-parts root)) 2)
        (break-thread t)
        (check-pred exn:break? (sync/timeout 10 result))
        (check-equal? (cache-files root) '()))))

  (test-case "S12 a break during the second download removes the first file and the partial second"
    (with-server (hash "/a" `(ok ,raw-a) "/stall" `(stall ,raw-b 10))
                 (lambda (u) (list (set-entry (list (list "a.rktd" (list (u "/a")) raw-a-sha256)
                                                    (list "b.rktd" (list (u "/stall")) raw-b-sha256)))))
      (lambda (srv root registry)
        (reset-converter!)
        (define-values (result t) (fetch-thread))
        (check-not-false (sync/timeout 10 (http-fixture-stalled-evt srv)))
        (check-equal? (length (raw-parts root)) 2)
        (break-thread t)
        (check-pred exn:break? (sync/timeout 10 result))
        (check-not-false (sync/timeout 10 (http-fixture-closed-evt srv)))
        (check-equal? (cache-files root) '())
        (check-equal? (converter-calls) '()))))

  (test-case "S13 the key is the content sha256: equal content under new raw pins hits, new content misses"
    (define a2 (rows->rktd-bytes (take toy-rows 2)))
    (define b2 (rows->rktd-bytes (drop toy-rows 2)))
    (define reversed-text (rows->rktd-bytes (reverse toy-rows)))
    (define reversed-sha (hex (sha256-bytes reversed-text)))
    (call-with-fetch-fixture '()
      (lambda (root _)
        (define (with-home thunk) (call-with-environment (hash "DATASETS_HOME" (path->string root)) thunk))
        (with-server routes (set-with "concat-rows")
          (lambda (_s _r _g) (with-home (lambda () (fetch-dataset 'toy-set)))))
        (with-server (hash "/a" `(ok ,a2) "/b" `(ok ,b2))
                     (lambda (u) (list (set-entry (list (list "a.rktd" (list (u "/a")) (hex (sha256-bytes a2)))
                                                        (list "b.rktd" (list (u "/b")) (hex (sha256-bytes b2)))))))
          (lambda (srv _r _g)
            (with-home
             (lambda ()
               (reset-converter!)
               (check-true (dataset-cached? 'toy-set))
               (check-equal? (table-digest (load-dataset-table 'toy-set)) toy-table-digest)
               (check-equal? (http-fixture-requests srv) '())
               (check-equal? (converter-calls) '())))))
        (with-server routes (set-with "reversed-rows" #:data-sha256 reversed-sha)
          (lambda (_s _r _g)
            (with-home
             (lambda ()
               (check-false (dataset-cached? 'toy-set))
               (check-equal? (vector->list (dataset-table-column (load-dataset-table 'toy-set) "id"))
                             '(3 2 1))))))
        (check-equal? (cache-files root)
                      (sort (list final-rel (format "toy-set/~a.rktd.gz" reversed-sha)) string<?)))))

  (test-case "S14 a corrupt converted file names the remedy, and the remedy works"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (define final (fetch-dataset 'toy-set))
        (for ([bytes (in-list (list (gzip-bytes (rows->rktd-bytes '((9 "a" 0.5)))) #"garbage"))]
              [rx (in-list (list #rx"data sha256 mismatch" #rx"cannot read dataset data"))])
          (call-with-output-file final #:exists 'truncate (lambda (o) (write-bytes bytes o)))
          (check-exn (fetch-error? rx "toy-set" "(clear-dataset-cache 'toy-set)")
                     (lambda () (load-dataset-table 'toy-set))))
        (clear-dataset-cache 'toy-set)
        (check-equal? (table-digest (load-dataset-table 'toy-set)) toy-table-digest)
        (check-equal? (http-fixture-requests srv) '("/a" "/b" "/a" "/b")))))

  (test-case "S15 stale raw temporaries: not cached, untouched by a fetch, removed by clear"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (define stale (format "toy-set/~a.raw.4242.part" (make-string 64 #\a)))
        (define foreign (list (format "toy-set/~a.raw" (make-string 64 #\b)) "toy-set/notes.txt"))
        (for ([f (in-list (cons stale foreign))])
          (make-directory* (path-only (build-path root f)))
          (call-with-output-file (build-path root f) (lambda (o) (write-bytes #"x" o))))
        (check-false (dataset-cached? 'toy-set))
        (fetch-dataset 'toy-set)
        (check-equal? (cache-files root) (sort (list* final-rel stale foreign) string<?))
        (clear-dataset-cache 'toy-set)
        (check-equal? (cache-files root) (sort foreign string<?)))))

  (test-case "S16 the converted text is the canonical write of the rows and loads back equal"
    (define text (rows->rktd-bytes edge-table-rows))
    (with-server routes (set-with "edge-rows" #:data-sha256 (hex (sha256-bytes text)))
      (lambda (srv root registry)
        (check-equal? (gunzip-file (fetch-dataset 'toy-set)) text)
        (define t (load-dataset-table 'toy-set))
        (check-equal? (for/list ([c (in-list '("id" "group" "score"))])
                        (vector->list (dataset-table-column t c)))
                      (list '(1 2 3) (list "x\ny \"q\" é" dataset-missing "c") (list -0.0 1e21 +nan.0))))))

  (test-case "S17 each file's download and the conversion are logged on topic datasets"
    (define receiver (make-log-receiver (current-logger) 'info 'datasets))
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (fetch-dataset 'toy-set)
        (define msgs (let loop () (match (sync/timeout 0 receiver)
                                    [#f '()] [(vector _ m _ _) (cons m (loop))])))
        (for ([needles (in-list (list (list "toy-set" "a.rktd" (http-fixture-url srv "/a"))
                                      (list "toy-set" "b.rktd" (http-fixture-url srv "/b"))
                                      (list "toy-set" "concat-rows")))])
          (check-true (for/or ([m (in-list msgs)])
                        (for/and ([n (in-list needles)]) (regexp-match? (regexp-quote n) m))))))))

  (test-case "S18 queries on a set create nothing and convert nothing"
    (call-with-fetch-fixture (list (car ((set-with "stalling-rows")
                                         (lambda (p) (string-append "file:///nonexistent" p)))))
      (lambda (root registry)
        (reset-converter!)
        (define sub (build-path root "sub"))
        (call-with-environment (hash "DATASETS_HOME" (path->string sub))
          (lambda ()
            (dataset-cache-directory) (dataset-cached? 'toy-set) (dataset-info 'toy-set) (dataset-names)
            (check-false (directory-exists? sub))
            (check-equal? (converter-calls) '()))))))

  (test-case "R1 a load peaks at the text plus one copy of the columns"
    (define n 50000) (define m 40)
    (define rows (for/list ([i (in-range n)]) (for/list ([j (in-range m)]) (modulo (+ i j) 2))))
    (define text (rows->rktd-bytes rows))
    (define entry (wide-entry m #:rows n #:data-sha256 (hex (sha256-bytes text))))
    (call-with-artifact-url (gzip-bytes text)
      (lambda (url)
        (call-with-fetch-fixture (list (hash-set entry "fetch" (hash "format" "rktd.gz" "urls" (list url)
                                                                     "sha256" (hex (sha256-bytes (gzip-bytes text))))))
          (lambda (root registry)
            (fetch-dataset 'wide)
            (collect-garbage) (collect-garbage)
            (define c (make-custodian))
            (custodian-limit-memory c (+ (bytes-length text) (* n m 8) (* 8 1024 1024)) c)
            (define ch (make-channel))
            (define collector (thread (lambda () (let loop () (collect-garbage) (sleep 0) (loop)))))
            (define t (parameterize ([current-custodian c])
                        (thread (lambda () (channel-put ch (dataset-table-row-count
                                                            (load-dataset-table 'wide #:download? #f)))))))
            (define result (sync/timeout 120 ch (wrap-evt (thread-dead-evt t) (lambda (_) 'killed))))
            (kill-thread collector)
            (check-equal? result n)))))))
