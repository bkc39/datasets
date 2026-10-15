#lang racket/base
(require racket/list racket/match datasets/core "fixture.rkt")
(module+ test
  (require rackunit)
  (define core-13 '(air-passengers airquality anscombe breast-cancer diabetes faithful
                    iris mtcars plant-growth titanic tooth-growth us-arrests wine))
  (define ((registry-error? . rxs) e)
    (and (exn:fail? e) (not (exn:fail:contract? e))
         (for/and ([rx (in-list rxs)]) (regexp-match? rx (exn-message e)))))
  (define registry-calls
    (list dataset-names dataset-packages
          (lambda () (dataset-info 'mtcars)) (lambda () (load-dataset-table 'mtcars))))
  (define (snapshot) (for/list ([n (in-list (dataset-names))]) (cons n (dataset-info n))))

  (check-equal? (dataset-packages) '("datasets-core"))                 ; R0
  (check-equal? (dataset-names) core-13)
  (check-equal? (dataset-names #:package "datasets-core") core-13)

  (call-with-registry-fixtures (list (registry-fixture "sibling"))     ; R1, R8, R12
    (lambda ()
      (check-equal? (dataset-names) (sort (list* 'toy 'toy-remote core-13) symbol<?))
      (check-equal? (dataset-packages) '("datasets-core" "datasets-fixture"))
      (check-equal? (dataset-names #:package "datasets-fixture") '(toy toy-remote))
      (define t (load-dataset-table 'toy))
      (check-equal? (dataset-table-names t) '("id" "group" "score"))
      (check-equal? (vector->list (dataset-table-column t "id")) '(1 2 3))
      (check-true (dataset-missing? (vector-ref (dataset-table-column t "group") 1)))
      (check-true (dataset-missing? (vector-ref (dataset-table-column t "score") 2)))
      (check-equal? (hash-ref (dataset-info 'toy) "package") "datasets-fixture")
      (check-equal? (hash-ref (dataset-info 'toy-remote) "tier") "fetch")
      (check-true (immutable? (hash-ref (dataset-info 'toy-remote) "fetch")))
      (check-exn (registry-error? #rx"fetch tier" #rx"toy-remote")
                 (lambda () (load-dataset-table 'toy-remote)))))
  (check-equal? (dataset-names) core-13)                               ; R13

  (define both (list (registry-fixture "sibling") (registry-fixture "other")))
  (check-equal? (call-with-registry-fixtures both snapshot)            ; R10
                (call-with-registry-fixtures (reverse both) snapshot))
  (call-with-registry-fixtures both                                    ; R11
    (lambda ()
      (define parts (map (lambda (p) (dataset-names #:package p)) (dataset-packages)))
      (check-true (andmap pair? parts))
      (check-equal? (sort (append* parts) symbol<?) (dataset-names))))

  (call-with-registry-fixtures (list (registry-fixture "duplicate"))   ; R2
    (lambda ()
      (for ([call (in-list registry-calls)])
        (check-exn (registry-error? #rx"duplicate dataset" #rx"iris" #rx"datasets-core" #rx"datasets-duplicate")
                   call))))
  (check-equal? (dataset-names) core-13)

  (call-with-registry-fixtures (list (registry-fixture "duplicate"))   ; R5
    (lambda ()
      (parameterize ([current-namespace (make-base-namespace)])
        (check-not-exn (lambda () (dynamic-require 'datasets/core #f)))
        (check-exn (registry-error? #rx"duplicate dataset")
                   (dynamic-require 'datasets/core 'dataset-names)))))

  (check-exn #rx"blaming: [^\n]*registries\\.rkt"                      ; R9
             (lambda () (dataset-names #:package 'datasets-core)))
  (check-exn (lambda (e) (and (exn:fail:contract? e)
                              (regexp-match? #rx"unknown package" (exn-message e))
                              (regexp-match? #rx"datasets-core" (exn-message e))))
             (lambda () (dataset-names #:package "datasets-absent")))

  (define (check-malformed info registries rxs)                        ; R3, R6, R7
    (call-with-temporary-fixture
     (lambda (dir)
       (write-registry-fixture! dir #:info info #:registries registries #:data (hash))
       (call-with-registry-fixtures (list dir)
         (lambda ()
           (for ([call (in-list registry-calls)])
             (check-exn (apply registry-error? (regexp (regexp-quote (path->string dir))) rxs) call)))))))
  (define (one entry) (hash "registry.rktd" (hash (hash-ref entry "name" "gen") entry)))
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
