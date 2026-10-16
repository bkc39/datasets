#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-missing? dataset-names dataset-packages
                  dataset-table-column dataset-table-names dataset-table-row-count load-dataset-table)
         (only-in racket/file make-directory*)
         (only-in racket/list append*)
         (only-in racket/match match-define)
         (only-in racket/runtime-path define-runtime-path)
         (only-in "fixture.rkt" call-with-environment call-with-registry-fixtures
                  call-with-temporary-fixture fetch-ok gen-entry registry-fixture
                  write-registry-fixture!))
(define-runtime-path core-module "../core.rkt")
(define-runtime-path core-registry "../core-data/registry.rktd")
(module+ test
  (require (only-in rackunit check-equal? check-exn check-false check-not-exn check-not-false
                    check-regexp-match check-true test-case)
           (only-in racket/sandbox kill-evaluator make-evaluator))
  (define core-13 '(air-passengers airquality anscombe breast-cancer diabetes faithful
                    iris mtcars plant-growth titanic tooth-growth us-arrests wine))
  (define ((registry-error? . rxs) e)
    (and (exn:fail? e) (not (exn:fail:contract? e))
         (for/and ([rx (in-list rxs)]) (regexp-match? rx (exn-message e)))))
  (define registry-calls
    (list dataset-names dataset-packages
          (lambda () (dataset-info 'mtcars)) (lambda () (load-dataset-table 'mtcars))))
  (define (snapshot) (for/list ([n (in-list (dataset-names))]) (cons n (dataset-info n))))
  (define installed (dataset-names))
  (define installed-packages (dataset-packages))
  (define installed-core (dataset-names #:package "datasets-core"))
  (define both (list (registry-fixture "sibling") (registry-fixture "other")))
  (define (check-malformed info registries rxs)
    (call-with-temporary-fixture
     (lambda (dir)
       (write-registry-fixture! dir #:info info #:registries registries #:data (hash))
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (for ([call (in-list registry-calls)])
             (check-exn (apply registry-error? (regexp (regexp-quote (path->string dir))) rxs) call)))))))
  (define (one entry) (hash "registry.rktd" (hash (hash-ref entry "name" "gen") entry)))
  (define (listed? name) (and (memq name (dataset-names)) #t))
  (define ((denial message) who path) (error who "~a~a" message path))
  (define ((errno-denial reason errno) who path)
    (raise (exn:fail:filesystem:errno
            (format "~a: cannot open input file\n  path: ~a\n  system error: ~a" who path reason)
            (current-continuation-marks) errno)))
  (define (call-with-unreadable file thunk #:raise [raise-denial (denial "denied: ")])
    (parameterize ([current-security-guard
                    (make-security-guard (current-security-guard)
                                         (lambda (who path modes)
                                           (when (and path (memq 'read modes) (equal? (simplify-path path) file))
                                             (raise-denial who path)))
                                         void)])
      (thunk)))
  (define (fixture-index)
    (build-path (car (current-library-collection-paths)) "info-domain" "compiled" "cache.rktd"))
  (define (call-with-core-copy registries thunk)
    (call-with-temporary-fixture
     (lambda (root)
       (define dir (build-path root "datasets" "core-data"))
       (make-directory* dir)
       (write-registry-fixture! dir #:info (sort (hash-keys registries) string<?) #:registries registries
                                #:data (hash))
       (call-with-registry-fixtures (list dir) thunk #:specs '((lib "datasets" "core-data"))))))
  (define (call-with-gen thunk)
    (call-with-temporary-fixture
     (lambda (dir)
       (rewrite-gen! dir "gen")
       (call-with-registry-fixtures (list dir) thunk))))
  (define (rewrite-gen! dir name #:modify-seconds [seconds #f])
    (write-registry-fixture! dir #:info '("registry.rktd") #:registries (one (gen-entry name)) #:data (hash))
    (when seconds (file-or-directory-modify-seconds (build-path dir "registry.rktd") seconds)))

  (test-case "R0 dedupe works; fixtures do not leak into installed discovery"
    (check-not-false (member "datasets-core" installed-packages))
    (check-false (for/or ([p (in-list installed-packages)])
                   (member p '("datasets-duplicate" "datasets-fixture" "datasets-other"))))
    (check-equal? (dataset-names #:package "datasets-core") core-13))

  (test-case "R1 R8 R12 sibling registry discovered and loads; uncached fetch tier refused offline"
    (call-with-registry-fixtures (list (registry-fixture "sibling"))
      (lambda ()
        (check-equal? (dataset-names) (sort (list* 'toy 'toy-remote installed) symbol<?))
        (check-equal? (dataset-packages) (sort (cons "datasets-fixture" installed-packages) string<?))
        (check-equal? (dataset-names #:package "datasets-fixture") '(toy toy-remote))
        (define t (load-dataset-table 'toy))
        (check-equal? (dataset-table-names t) '("id" "group" "score"))
        (check-equal? (vector->list (dataset-table-column t "id")) '(1 2 3))
        (check-true (dataset-missing? (vector-ref (dataset-table-column t "group") 1)))
        (check-true (dataset-missing? (vector-ref (dataset-table-column t "score") 2)))
        (check-equal? (hash-ref (dataset-info 'toy) "package") "datasets-fixture")
        (check-equal? (hash-ref (dataset-info 'toy-remote) "tier") "fetch")
        (check-true (immutable? (hash-ref (dataset-info 'toy-remote) "fetch")))
        (call-with-temporary-fixture
         (lambda (root)
           (call-with-environment (hash "DATASETS_HOME" (path->string root) "DATASETS_OFFLINE" "1")
             (lambda ()
               (check-exn (registry-error? #rx"not cached" #rx"toy-remote")
                          (lambda () (load-dataset-table 'toy-remote))))))))))

  (test-case "R13 registry cache follows discovery"
    (check-equal? (dataset-names) installed))

  (test-case "R10 merge independent of discovery order"
    (check-equal? (call-with-registry-fixtures both snapshot)
                  (call-with-registry-fixtures (reverse both) snapshot)))

  (test-case "R11 #:package partitions dataset-names"
    (call-with-registry-fixtures both
      (lambda ()
        (define parts (map (lambda (p) (dataset-names #:package p)) (dataset-packages)))
        (check-true (andmap pair? parts))
        (check-equal? (sort (append* parts) symbol<?) (dataset-names)))))

  (test-case "R2 duplicates rejected naming both packages"
    (call-with-registry-fixtures (list (registry-fixture "duplicate"))
      (lambda ()
        (for ([call (in-list registry-calls)])
          (check-exn (registry-error? #rx"duplicate dataset" #rx"iris" #rx"datasets-core" #rx"datasets-duplicate")
                     call))))
    (check-equal? (dataset-names) installed))

  (test-case "R5 discovery lazy: require never raises"
    (call-with-registry-fixtures (list (registry-fixture "duplicate"))
      (lambda ()
        (parameterize ([current-namespace (make-base-namespace)])
          (check-not-exn (lambda () (dynamic-require 'datasets/core #f)))
          (check-exn (registry-error? #rx"duplicate dataset")
                     (dynamic-require 'datasets/core 'dataset-names))))))

  (test-case "R9 contract blame names caller; unknown package reported"
    (check-exn #rx"blaming: [^\n]*registries\\.rkt"
               (lambda () (dataset-names #:package 'datasets-core)))
    (check-exn (lambda (e) (and (exn:fail:contract? e)
                                (regexp-match? #rx"unknown package" (exn-message e))
                                (regexp-match? #rx"datasets-core" (exn-message e))))
               (lambda () (dataset-names #:package "datasets-absent"))))

  (test-case "R3 R6 R7 malformed info or registry names file and key"
    (for ([c (in-list
              (list (list (hash-remove (gen-entry "gen") "tier") #rx"\"tier\"")
                    (list (hash-set (gen-entry "gen") "tier" "cloud") #rx"\"tier\"")
                    (list (gen-entry "gen" #:fetch fetch-ok) #rx"\"fetch\"")
                    (list (gen-entry "gen" #:tier "fetch") #rx"\"fetch\"")
                    (list (gen-entry "gen" #:tier "fetch" #:fetch (hash-set fetch-ok "urls" '())) #rx"\"urls\"")
                    (list (gen-entry "gen" #:tier "fetch" #:fetch (hash-set fetch-ok "sha256" "xyz")) #rx"\"sha256\"")
                    (list (gen-entry "gen" #:tier "fetch" #:fetch (hash-remove fetch-ok "format")) #rx"\"format\"")
                    (list (hash-remove (gen-entry "gen") "package") #rx"\"package\"")
                    (list (gen-entry "gen" #:package "datasets core") #rx"\"package\"")
                    (list (gen-entry "gen" #:sha256 "ABC") #rx"\"sha256\"")
                    (list (hash-remove (gen-entry "gen") "columns") #rx"\"columns\"")))])
      (match-define (list entry rx) c)
      (check-malformed '("registry.rktd") (one entry) (list rx #rx"gen")))
    (check-malformed '("registry.rktd") (hash "registry.rktd" (hash "gen" (gen-entry "other"))) (list #rx"\"name\""))
    (check-malformed '("registry.rktd") (hash "registry.rktd" (hash "Gen" (gen-entry "Gen"))) (list #rx"\"name\""))
    (check-malformed '("registry.rktd")
                     (hash "registry.rktd" (hash "a" (gen-entry "a" #:package "p-one") "b" (gen-entry "b" #:package "p-two")))
                     (list #rx"\"package\""))
    (check-malformed '("registry.rktd") (hash "registry.rktd" '(1 2)) (list #rx"registry"))
    (check-malformed '("one.rktd" "two.rktd")
                     (hash "one.rktd" (hash "gen" (gen-entry "gen")) "two.rktd" (hash "gen" (gen-entry "gen")))
                     (list #rx"duplicate dataset" #rx"one\\.rktd" #rx"two\\.rktd"))
    (for ([info (in-list (list '("../registry.rktd") '("/tmp/registry.rktd") "registry.rktd" '(registry) '("absent.rktd")))])
      (check-malformed info (hash) (list #rx"datasets-registries|absent\\.rktd"))))

  (test-case "stale index: a missing directory, one without info.rkt, or one without the key is skipped"
    (call-with-temporary-fixture
     (lambda (dir)
       (define moved (build-path dir "moved"))
       (make-directory* moved)
       (call-with-output-file (build-path moved "info.rkt")
         (lambda (out) (write '(module info info (#%module-begin (define name "moved"))) out)))
       (call-with-registry-fixtures (list (build-path dir "absent") dir moved)
         (lambda ()
           (check-equal? (dataset-names) installed)
           (check-equal? (dataset-table-row-count (load-dataset-table 'iris)) 150))))))

  (test-case "cache follows the registry files an edited info.rkt declares"
    (call-with-temporary-fixture
     (lambda (dir)
       (write-registry-fixture! dir #:info '("one.rktd")
                                #:registries (hash "one.rktd" (hash "gen" (gen-entry "gen"))
                                                   "two.rktd" (hash "gen-two" (gen-entry "gen-two")))
                                #:data (hash))
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-false (memq 'gen-two (dataset-names)))
           (write-registry-fixture! dir #:info '("one.rktd" "two.rktd") #:registries (hash) #:data (hash))
           (check-not-false (memq 'gen-two (dataset-names))))))))

  (test-case "a registry error is not cached: fixing the file in place recovers"
    (call-with-temporary-fixture
     (lambda (dir)
       (write-registry-fixture! dir #:info '("registry.rktd")
                                #:registries (one (hash-remove (gen-entry "gen") "tier"))
                                #:data (hash))
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-exn (registry-error? #rx"\"tier\"") dataset-names)
           (write-registry-fixture! dir #:info '("registry.rktd") #:registries (one (gen-entry "gen"))
                                    #:data (hash))
           (check-equal? (dataset-names) (sort (cons 'gen installed) symbol<?)))))))

  (test-case "an unreadable registry is not cached: once readable it is listed"
    (call-with-temporary-fixture
     (lambda (dir)
       (rewrite-gen! dir "gen")
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (call-with-unreadable (build-path dir "registry.rktd")
             (lambda () (check-exn (registry-error? #rx"cannot read registry" #rx"registry\\.rktd") dataset-names)))
           (check-true (listed? 'gen)))))))

  (test-case "the cache key covers an in-place rewrite"
    (call-with-temporary-fixture
     (lambda (dir)
       (rewrite-gen! dir "gen")
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-true (listed? 'gen))
           (rewrite-gen! dir "gen-x")
           (check-equal? (map listed? '(gen gen-x)) '(#f #t)))))))

  (test-case "the cache key covers a same-size rewrite with the modify time pinned"
    (call-with-temporary-fixture
     (lambda (dir)
       (rewrite-gen! dir "gen" #:modify-seconds 1000000)
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-true (listed? 'gen))
           (rewrite-gen! dir "gem" #:modify-seconds 1000000)
           (check-equal? (map listed? '(gen gem)) '(#f #t)))))))

  (test-case "declared registries are cached until the info.rkt changes"
    (call-with-temporary-fixture
     (lambda (dir)
       (rewrite-gen! dir "gen")
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-true (listed? 'gen))
           (call-with-unreadable (build-path dir "info.rkt") (lambda () (check-true (listed? 'gen)))))))))

  (test-case "a discovered copy of the core registry is not a duplicate"
    (call-with-core-copy (hash "registry.rktd" (call-with-input-file core-registry read))
      (lambda ()
        (check-equal? (dataset-names) installed)
        (check-equal? (dataset-table-row-count (load-dataset-table 'iris)) 150))))

  (test-case "a discovered copy of the core registry is skipped unread"
    (call-with-core-copy (hash "registry.rktd"
                               (for/hash ([(name entry) (in-hash (call-with-input-file core-registry read))])
                                 (values name (hash-remove entry "source-version"))))
      (lambda () (check-equal? (dataset-names #:package "datasets-core") core-13))))

  (test-case "a registry outside datasets/core-data cannot declare datasets-core"
    (check-malformed '("registry.rktd")
                     (hash "registry.rktd" (hash "toy" (gen-entry "toy" #:package "datasets-core")
                                                 "iris" (gen-entry "iris" #:package "datasets-core")))
                     (list #rx"package datasets-core is reserved" #rx"registry\\.rktd")))

  (test-case "every registry core's info.rkt declares is used"
    (call-with-temporary-fixture
     (lambda (root)
       (define dir (build-path root "datasets" "core-data"))
       (make-directory* dir)
       (copy-file core-module (build-path root "datasets" "core.rkt"))
       (write-registry-fixture! dir #:info '("registry.rktd" "extra.rktd")
                                #:registries (hash "registry.rktd" (call-with-input-file core-registry read)
                                                   "extra.rktd" (hash "gen" (gen-entry "gen" #:package "datasets-core")))
                                #:data (hash))
       (parameterize ([current-library-collection-paths (cons root (current-library-collection-paths))]
                      [current-namespace (make-base-namespace)])
         (check-equal? ((dynamic-require 'datasets/core 'dataset-names) #:package "datasets-core")
                       (sort (cons 'gen core-13) symbol<?))))))

  (test-case "a discovered core copy listing other registry files is skipped"
    (call-with-core-copy (hash "registry.rktd" (call-with-input-file core-registry read)
                               "fetch.rktd" (hash "gen" (gen-entry "gen" #:package "datasets-core")))
      (lambda () (check-equal? (dataset-names) installed))))

  (test-case "a discovery failure is not cached"
    (call-with-gen
     (lambda ()
       (call-with-unreadable (fixture-index) (lambda () (check-exn #rx"denied" dataset-names)))
       (check-true (listed? 'gen)))))

  (test-case "access denied to the index falls back to datasets-core, warns, and is retried"
    (call-with-gen
     (lambda ()
       (define receiver (make-log-receiver (current-logger) 'warning 'datasets))
       (call-with-unreadable (fixture-index) #:raise (denial "`read' access denied for ")
         (lambda ()
           (check-equal? (dataset-names) installed-core)
           (check-exn #rx"unknown dataset.*sibling-package discovery failed.*access denied"
                      (lambda () (dataset-info 'gen)))))
       (check-true (listed? 'gen))
       (check-regexp-match #rx"sibling-package discovery failed.*access denied"
                           (vector-ref (sync/timeout 0 receiver) 1)))))

  (test-case "a default sandbox evaluator loads the core datasets"
    (define ev (make-evaluator 'racket/base))
    (ev '(require datasets/core))
    (check-equal? (ev '(dataset-table-row-count (load-dataset-table 'iris))) 150)
    (check-exn #rx"unknown dataset.*sibling-package discovery failed"
               (lambda () (ev '(dataset-info 'absent))))
    (kill-evaluator ev))

  (test-case "a computed datasets-registries value is re-read when info.rkt changes"
    (call-with-temporary-fixture
     (lambda (dir)
       (define (write-info! names)
         (call-with-output-file (build-path dir "info.rkt") #:exists 'truncate
           (lambda (out)
             (write `(module info info (#%module-begin (define datasets-registries (list ,@names)))) out))))
       (write-registry-fixture! dir #:info '()
                                #:registries (hash "registry.rktd" (hash "gen" (gen-entry "gen"))
                                                   "two.rktd" (hash "second" (gen-entry "second")))
                                #:data (hash))
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-equal? (for/list ([round (in-range 10)])
                           (write-info! (if (even? round) '("registry.rktd") '("registry.rktd" "two.rktd")))
                           (listed? 'second))
                         (for/list ([round (in-range 10)]) (odd? round))))))))

  (test-case "only a permission error or an access-denied guard falls back to datasets-core"
    (call-with-gen
     (lambda ()
       (call-with-unreadable (fixture-index)
         #:raise (errno-denial "Too many open files; errno=24" '(24 . posix))
         (lambda () (check-exn #rx"Too many open files" dataset-names)))
       (call-with-unreadable (fixture-index) #:raise (errno-denial "Permission denied; errno=13" '(13 . posix))
         (lambda () (check-equal? (dataset-names) installed-core)))
       (check-true (listed? 'gen)))))

  (test-case "a malformed index raises, even one mentioning access denied, and is retried once restored"
    (for ([bad (in-list '(bogus ("access denied")))])
      (call-with-gen
       (lambda ()
         (define index (fixture-index))
         (define saved (call-with-input-file index read))
         (call-with-output-file index #:exists 'truncate (lambda (out) (write bad out)))
         (check-exn #rx"bad info-domain cache" dataset-names)
         (call-with-output-file index #:exists 'truncate (lambda (out) (write saved out)))
         (check-true (listed? 'gen))))))

  (test-case "a registry under another path ending in datasets/core-data is discovered"
    (call-with-temporary-fixture
     (lambda (root)
       (define dir (build-path root "mylab" "datasets" "core-data"))
       (make-directory* dir)
       (write-registry-fixture! dir #:info '("registry.rktd")
                                #:registries (one (gen-entry "lab" #:package "mylab-data")) #:data (hash))
       (call-with-registry-fixtures (list dir) (lambda () (check-true (listed? 'lab)))))))

  (test-case "an unknown package in the fallback gives the discovery failure"
    (call-with-gen
     (lambda ()
       (call-with-unreadable (fixture-index) #:raise (denial "`read' access denied for ")
         (lambda ()
           (check-exn #rx"unknown package.*sibling-package discovery failed"
                      (lambda () (dataset-names #:package "datasets-gen")))))))))
