#lang racket/base
(require (only-in datasets/core clear-dataset-cache dataset-cache-directory dataset-cached? dataset-info
                  dataset-missing dataset-names dataset-table-column dataset-table-row-count fetch-dataset
                  load-dataset-table)
         (only-in datasets/core-tests/convert-fixture converter-recorder edge-table-rows malformed-row)
         (only-in racket/contract/combinator blame-positive exn:fail:contract:blame-object
                  exn:fail:contract:blame?)
         (only-in racket/file make-directory*)
         (only-in racket/list drop take)
         (only-in racket/match match match-define)
         (only-in racket/path path-only)
         (only-in "fixture.rkt" cache-files call-with-artifact-url call-with-environment
                  call-with-fetch-fixture call-with-memory-limit fetch-error? gunzip-file gzip-bytes hex raw-a raw-a-sha256 raw-b
                  raw-b-sha256 rows->rktd-bytes set-entry table-digest toy-content-sha256 toy-rows
                  toy-set-entry toy-table-digest wide-entry with-server)
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
  (define ((blames? module-rx detail) e)
    (and (exn:fail:contract:blame? e)
         (regexp-match? module-rx (format "~a" (blame-positive (exn:fail:contract:blame-object e))))
         (regexp-match? (regexp-quote detail) (exn-message e))))
  (define ((set-with binding #:module [module "datasets/core-tests/convert-fixture"]
                     #:data-sha256 [sha toy-content-sha256]) u)
    (list (toy-set-entry u #:binding binding #:module module #:data-sha256 sha)))
  (define (call-with-converter-calls proc)
    (define calls '())
    (parameterize ([converter-recorder (lambda (call) (set! calls (cons call calls)))])
      (proc (lambda () (reverse calls)))))
  (define (call-with-wide-text text width rows proc)
    (define gz (gzip-bytes text))
    (call-with-artifact-url gz
      (lambda (url)
        (define entry (wide-entry width #:rows rows #:data-sha256 (hex (sha256-bytes text))))
        (call-with-fetch-fixture (list (hash-set entry "fetch" (hash "format" "rktd.gz" "urls" (list url)
                                                                     "sha256" (hex (sha256-bytes gz)))))
          (lambda (root registry) (proc))))))
  (define (load-wide-text text rows)
    (call-with-wide-text text 2 rows
      (lambda ()
        (with-handlers ([exn:fail? exn-message])
          (define t (load-dataset-table 'wide))
          (list (dataset-table-row-count t)
                (vector->list (dataset-table-column t "c0"))
                (vector->list (dataset-table-column t "c1")))))))
  (define (fetch-thread)
    (define result (make-channel))
    (values result (thread (lambda () (channel-put result (with-handlers ([exn:break? values])
                                                            (fetch-dataset 'toy-set)))))))

  (test-case "a miss downloads each file once in order, caches only the converted file, then hits"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (call-with-converter-calls
         (lambda (converter-calls)
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
           (check-equal? (length (converter-calls)) 1))))))

  (test-case "the converter gets each verified file by name beside the cache file, and the info"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (call-with-converter-calls
         (lambda (converter-calls)
           (fetch-dataset 'toy-set)
           (match-define (list (list immutable-files? seen info)) (converter-calls))
           (check-true immutable-files?)
           (check-equal? (for/hash ([(n s) (in-hash seen)])
                           (match-define (list _path sha256) s)
                           (values n sha256))
                         (hash "a.rktd" raw-a-sha256 "b.rktd" raw-b-sha256))
           (for ([s (in-hash-values seen)])
             (match-define (list path _sha256) s)
             (check-true (complete-path? path))
             (check-equal? (path-only path) (path-only (build-path root final-rel)))
             (check-false (file-exists? path)))
           (check-equal? info (dataset-info 'toy-set)))))))

  (test-case "each file falls through its own mirrors in order"
    (with-server (hash "/a404" '(status 404) "/a" `(ok ,raw-a) "/bbad" `(ok ,raw-a)
                       "/rb" '(redirect "/b") "/b" `(ok ,raw-b))
                 (lambda (u) (list (set-entry (list (list "a.rktd" (list (u "/a404") (u "/a")) raw-a-sha256)
                                                    (list "b.rktd" (list (u "/bbad") (u "/rb")) raw-b-sha256)))))
      (lambda (srv root registry)
        (check-equal? (fetch-dataset 'toy-set) (build-path root final-rel))
        (check-equal? (http-fixture-requests srv) '("/a404" "/a" "/bbad" "/rb" "/b"))
        (check-equal? (cache-files root) (list final-rel)))))

  (test-case "a file whose every URL fails is named in the error; nothing is converted or cached"
    (with-server (hash "/a" `(ok ,raw-a) "/bbad" `(ok ,raw-a))
                 (lambda (u) (list (set-entry (list (list "a.rktd" (list (u "/a")) raw-a-sha256)
                                                    (list "b.rktd" (list (u "/bbad") (u "/missing"))
                                                          raw-b-sha256)))))
      (lambda (srv root registry)
        (call-with-converter-calls
         (lambda (converter-calls)
           (for ([who (in-list (list fetch-dataset load-dataset-table))]
                 [rx (in-list (list #rx"^fetch-dataset: cannot download dataset\n"
                                    #rx"^load-dataset-table: cannot download dataset\n"))])
             (check-exn (fetch-error? rx "\n  dataset: toy-set\n  file: b.rktd\n"
                                      (http-fixture-url srv "/bbad") "sha256 mismatch" raw-b-sha256
                                      raw-a-sha256 (http-fixture-url srv "/missing") "HTTP status 404")
                        (lambda () (who 'toy-set))))
           (check-equal? (converter-calls) '())
           (check-equal? (cache-files root) '())
           (check-false (dataset-cached? 'toy-set))
           (check-equal? (http-fixture-requests srv)
                         '("/a" "/bbad" "/missing" "/a" "/bbad" "/missing")))))))

  (test-case "a converter that raises fails without a remedy; a non-exn raise propagates; nothing cached"
    (for ([binding (in-list '("failing-rows" "lazy-failing-rows"))]
          [message (in-list '("upstream format changed" "line 1 malformed"))])
      (with-server routes (set-with binding)
        (lambda (srv root registry)
          (check-exn (converter-error? #rx"^fetch-dataset: converter failed\n" "dataset: toy-set"
                                       "package: datasets-fetch-fixture"
                                       "module: datasets/core-tests/convert-fixture"
                                       (string-append "binding: " binding) message)
                     (lambda () (fetch-dataset 'toy-set)))
          (check-equal? (cache-files root) '()))))
    (with-server routes (set-with "raising-rows")
      (lambda (srv root registry)
        (check-eq? (with-handlers ([symbol? values]) (fetch-dataset 'toy-set)) 'upstream)
        (check-equal? (cache-files root) '()))))

  (test-case "content that differs from the pin names expected and actual, without a remedy"
    (with-server routes (set-with "concat-rows" #:data-sha256 (make-string 64 #\0))
      (lambda (srv root registry)
        (check-exn (converter-error? #rx"^fetch-dataset: converted data sha256 mismatch\n"
                                     "dataset: toy-set" "binding: concat-rows"
                                     (string-append "expected: " (make-string 64 #\0))
                                     (string-append "actual: " toy-content-sha256))
                   (lambda () (fetch-dataset 'toy-set)))
        (check-equal? (cache-files root) '())
        (check-false (dataset-cached? 'toy-set)))))

  (test-case "a malformed row names its index; nothing cached"
    (for ([bad (in-list '((2 "c") (2 "c" 1.5 4) (2 "c" 1/2) (2 c 1.5) (2 "c" #t) #(2 "c" 1.5)
                          (2 "c" (1.5)) 2))])
      (with-server routes (set-with "malformed-rows")
        (lambda (srv root registry)
          (check-exn (converter-error? #rx"^fetch-dataset: converter produced a malformed row\n"
                                       "dataset: toy-set" "binding: malformed-rows" "row: 1")
                     (lambda () (parameterize ([malformed-row bad]) (fetch-dataset 'toy-set))))
          (check-equal? (cache-files root) '())))))

  (test-case "breaking the protocol is a contract violation blaming the converter module"
    (for ([c (in-list '(("convert-fixture" "wrong-arity" "accepts: 1 argument" #f)
                        ("convert-fixture" "not-a-procedure" "produced: 'rows" #f)
                        ("convert-fixture" "non-sequence-rows" "promised: sequence?" #t)
                        ("arity-convert-fixture" "one-argument-rows" "accepts: 1 argument" #f)
                        ("contract-convert-fixture" "non-sequence-rows" "promised: sequence?" #t)))])
      (match-define (list file binding detail downloads?) c)
      (with-server routes (set-with binding #:module (string-append "datasets/core-tests/" file))
        (lambda (srv root registry)
          (check-exn (blames? (regexp (string-append "core-tests/" (regexp-quote file) "($|[.]rkt$)")) detail)
                     (lambda () (fetch-dataset 'toy-set)))
          (check-equal? (http-fixture-requests srv) (if downloads? '("/a" "/b") '()))
          (check-equal? (cache-files root) '())))))

  (test-case "a converter that cannot be loaded fails before any request; the registry stays valid"
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

  (test-case "#:download? #f refuses a set before any request or conversion"
    (with-server routes (set-with "concat-rows")
      (lambda (srv root registry)
        (call-with-converter-calls
         (lambda (converter-calls)
           (check-exn (fetch-error? #rx"^load-dataset-table: dataset is not cached" "toy-set"
                                    "#:download? #f" "(fetch-dataset 'toy-set)")
                      (lambda () (load-dataset-table 'toy-set #:download? #f)))
           (check-equal? (http-fixture-requests srv) '())
           (check-equal? (converter-calls) '())
           (check-equal? (cache-files root) '()))))))

  (test-case "a break during conversion removes every temporary file"
    (with-server routes (set-with "stalling-rows")
      (lambda (srv root registry)
        (define started (make-semaphore 0))
        (define-values (result t)
          (parameterize ([converter-recorder (lambda (_) (semaphore-post started))])
            (fetch-thread)))
        (check-not-false (sync/timeout 10 started))
        (check-equal? (length (raw-parts root)) 2)
        (break-thread t)
        (check-pred exn:break? (sync/timeout 10 result))
        (check-equal? (cache-files root) '()))))

  (test-case "a break during the second download removes the first file and the partial second"
    (with-server (hash "/a" `(ok ,raw-a) "/stall" `(stall ,raw-b 10))
                 (lambda (u) (list (set-entry (list (list "a.rktd" (list (u "/a")) raw-a-sha256)
                                                    (list "b.rktd" (list (u "/stall")) raw-b-sha256)))))
      (lambda (srv root registry)
        (call-with-converter-calls
         (lambda (converter-calls)
           (define-values (result t) (fetch-thread))
           (check-not-false (sync/timeout 10 (http-fixture-stalled-evt srv)))
           (check-equal? (length (raw-parts root)) 2)
           (break-thread t)
           (check-pred exn:break? (sync/timeout 10 result))
           (check-not-false (sync/timeout 10 (http-fixture-closed-evt srv)))
           (check-equal? (cache-files root) '())
           (check-equal? (converter-calls) '()))))))

  (test-case "the key is the content sha256: equal content under new raw pins hits, new content misses"
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
               (call-with-converter-calls
                (lambda (converter-calls)
                  (check-true (dataset-cached? 'toy-set))
                  (check-equal? (table-digest (load-dataset-table 'toy-set)) toy-table-digest)
                  (check-equal? (http-fixture-requests srv) '())
                  (check-equal? (converter-calls) '())))))))
        (with-server routes (set-with "reversed-rows" #:data-sha256 reversed-sha)
          (lambda (_s _r _g)
            (with-home
             (lambda ()
               (check-false (dataset-cached? 'toy-set))
               (check-equal? (vector->list (dataset-table-column (load-dataset-table 'toy-set) "id"))
                             '(3 2 1))))))
        (check-equal? (cache-files root)
                      (sort (list final-rel (format "toy-set/~a.rktd.gz" reversed-sha)) string<?)))))

  (test-case "a corrupt converted file names the remedy, and the remedy works"
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

  (test-case "stale raw temporaries are not cached, untouched by a fetch, and removed by clear"
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

  (test-case "the converted text is the canonical write of the rows and loads back equal"
    (define text (rows->rktd-bytes edge-table-rows))
    (with-server routes (set-with "edge-rows" #:data-sha256 (hex (sha256-bytes text)))
      (lambda (srv root registry)
        (check-equal? (gunzip-file (fetch-dataset 'toy-set)) text)
        (define t (load-dataset-table 'toy-set))
        (check-equal? (for/list ([c (in-list '("id" "group" "score"))])
                        (vector->list (dataset-table-column t c)))
                      (list '(1 2 3) (list "x\ny \"q\" é" dataset-missing "c") (list -0.0 1e21 +nan.0))))))

  (test-case "the converted text does not follow the caller's print parameters"
    (define text (rows->rktd-bytes edge-table-rows))
    (with-server routes (set-with "edge-rows" #:data-sha256 (hex (sha256-bytes text)))
      (lambda (srv root registry)
        (parameterize ([print-pair-curly-braces #t] [print-graph #t])
          (check-equal? (gunzip-file (fetch-dataset 'toy-set)) text)))))

  (test-case "each file's download and the conversion are logged on topic datasets"
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

  (test-case "queries on a set create nothing and convert nothing"
    (call-with-fetch-fixture (list (toy-set-entry (lambda (p) (string-append "file:///nonexistent" p))
                                                  #:binding "stalling-rows"))
      (lambda (root registry)
        (call-with-converter-calls
         (lambda (converter-calls)
           (define sub (build-path root "sub"))
           (call-with-environment (hash "DATASETS_HOME" (path->string sub))
             (lambda ()
               (dataset-cache-directory) (dataset-cached? 'toy-set) (dataset-info 'toy-set) (dataset-names)
               (check-false (directory-exists? sub))
               (check-equal? (converter-calls) '()))))))))

  (test-case "the loader fills, grows or trims its columns to the rows actually present"
    (define text (rows->rktd-bytes '((1 2) (3 4) (5 6))))
    (for ([rows (in-list '(3 1 100 0))])
      (check-equal? (load-wide-text text rows) '(3 (1 3 5) (2 4 6)))))

  (test-case "canonical cells and framing load as written"
    (for ([c (in-list '((#"(\n  (1 2)\n)" (1 (1) (2)))
                        (#"(\n  (+inf.0 -inf.0)\n  (+nan.0 -0.0)\n)\n" (2 (+inf.0 +nan.0) (-inf.0 -0.0)))
                        (#"(\n  (123456789012345678901 -1234567890123456789)\n)\n"
                         (1 (123456789012345678901) (-1234567890123456789)))
                        (#"(\n  (2.9e-05 1e+21)\n  (0.5 -1e-7)\n)\n" (2 (2.9e-05 0.5) (1e+21 -1e-7)))))])
      (match-define (list text expected) c)
      (check-equal? (load-wide-text text (car expected)) expected (format "~s" text))))

  (test-case "data that is not the canonical written form cannot be parsed"
    (for ([c (in-list '((#"; written by hand\n(\n  (1 2)\n)\n" "expected an opening parenthesis")
                        (#"(\n  (1 2)\n  (3)\n)\n" "row 1 has 1 values")
                        (#"(\n  (1 2)\n  3\n)\n" "row 1 is not a list")
                        (#"(\n  (1 2)\n" "unexpected end of data after row 1")
                        (#"(\n  (1 2)\n  (3 \"4)\n" "row 1: unterminated string")
                        (#"(\n  (1 2)\n)\n  (3 4)\n" "unexpected data after the closing parenthesis")
                        (#"(\n  (1 2)\n)\n)\n" "unexpected data after the closing parenthesis")
                        (#"(\n  (1 2 3)\n)\n" "row 0 has more than 2 values")
                        (#"(\n  (1 2)\n  (3 4) \n)\n" "row 1 is not a list")
                        (#"(\n (1 2)\n)\n" "row 0 is not a list")
                        (#"(\n  [1 2]\n)\n" "row 0 is not a list")
                        (#"(\n  ( 1 2)\n)\n" "row 0: expected a value")
                        (#"(\n  (1  2)\n)\n" "row 0: expected a value")
                        (#"(\n  (1 )\n)\n" "row 0: expected a value")
                        (#"(\n  (\"a\"b 2)\n)\n" "row 0: expected a space after value 0")
                        (#"(\n  (1\t2)\n)\n" "row 0: unexpected value 1\t2")
                        (#"(\n  (1 1/2)\n)\n" "row 0: unexpected value 1/2")
                        (#"(\n  (1 #t)\n)\n" "row 0: unexpected value #t")
                        (#"(\n  (1 nan)\n)\n" "row 0: unexpected value nan")
                        (#"(\n  (1 (2))\n)\n" "row 0: unexpected value (2)")
                        (#"(\n  (1 \"\\q\")\n)\n" "row 0: string::1: read: unknown escape sequence `\\q` in string")
                        (#"(\n  (1 \"\377\")\n)\n" "row 0: invalid UTF-8 in a string")))])
      (match-define (list text detail) c)
      (define message (load-wide-text text 1))
      (check-pred string? message)
      (for ([needle (in-list (list "load-dataset-table: cannot parse dataset data\n" "dataset: wide"
                                   "package: datasets-fetch-fixture" (string-append "error: " detail)))])
        (check-true (regexp-match? (regexp-quote needle) message) needle))))

  (test-case "a load peaks at the text plus one copy of the columns"
    (define n 50000) (define m 40)
    (define rows (for/list ([i (in-range n)]) (for/list ([j (in-range m)]) (modulo (+ i j) 2))))
    (define text (rows->rktd-bytes rows))
    (call-with-wide-text text m n
      (lambda ()
        (fetch-dataset 'wide)
        (collect-garbage)
        (check-equal? (call-with-memory-limit
                       (+ (bytes-length text) (* n m 8) (* 8 1024 1024)) 120
                       (lambda () (dataset-table-row-count (load-dataset-table 'wide #:download? #f))))
                      n)))))
