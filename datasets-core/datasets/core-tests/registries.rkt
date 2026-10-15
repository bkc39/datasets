#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-missing? dataset-names dataset-packages
                  dataset-table-column dataset-table-names dataset-table-row-count load-dataset-table)
         (only-in racket/list append*)
         (only-in racket/match match-define)
         (only-in racket/runtime-path define-runtime-path)
         (only-in "fixture.rkt" call-with-environment call-with-registry-fixtures
                  call-with-temporary-fixture fetch-ok gen-entry registry-fixture
                  write-registry-fixture!))
(define-runtime-path core-registry "../core-data/registry.rktd")
(module+ test
  (require (only-in rackunit check-equal? check-exn check-false check-not-exn check-not-false
                    check-true test-case))
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

  (test-case "stale index: a missing directory or one without info.rkt is skipped"
    (call-with-temporary-fixture
     (lambda (dir)
       (call-with-registry-fixtures (list (build-path dir "absent") dir)
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

  (test-case "a discovered copy of the core registry is not a duplicate"
    (call-with-temporary-fixture
     (lambda (dir)
       (write-registry-fixture! dir #:info '("registry.rktd") #:registries (hash) #:data (hash))
       (copy-file core-registry (build-path dir "registry.rktd"))
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (check-equal? (dataset-names) installed)
           (check-equal? (dataset-table-row-count (load-dataset-table 'iris)) 150)))))))
