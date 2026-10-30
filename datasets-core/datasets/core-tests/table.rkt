#lang racket/base
(require (only-in racket/list append-map)
         (only-in racket/match match match-define)
         (only-in datasets/core
                  dataset-missing dataset-names dataset-table dataset-table-column
                  dataset-table-columns dataset-table-info dataset-table-names
                  dataset-table-row-count load-dataset-table)
         (only-in datasets/core-tests/fixture column-types datum->cells rebuild)
         (only-in datasets/synthetic make-blobs make-circles make-regression))
(module+ test
  (require (only-in rackunit check-equal? check-exn check-regexp-match check-true test-case)
           (only-in racket/contract/combinator exn:fail:contract:blame?))
  (define (outcome form)
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-attach-module (variable-reference->empty-namespace (#%variable-reference))
                               'datasets/core)
      (eval `(module client racket/base (require datasets/core) (void ,form)))
      (with-handlers ([exn:fail:contract:blame? exn-message])
        (eval '(require 'client))
        'accepted)))
  (define (bounded-outcome form)
    (define custodian (make-custodian))
    (custodian-limit-memory custodian (* 256 1024 1024) custodian)
    (define result (box 'unfinished))
    (define worker
      (parameterize ([current-custodian custodian])
        (thread (λ () (set-box! result (outcome form))))))
    (sync/timeout 10 worker)
    (custodian-shutdown-all custodian)
    (unbox result))
  (define (deeply-immutable? v)
    (match v
      [(? hash?)
       (and (immutable? v)
            (for/and ([(k x) (in-hash v)]) (and (deeply-immutable? k) (deeply-immutable? x))))]
      [(cons a d) (and (deeply-immutable? a) (deeply-immutable? d))]
      [(? string?) (immutable? v)]
      [(? vector?) (and (immutable? v) (for/and ([x (in-vector v)]) (deeply-immutable? x)))]
      [_ #t]))
  (define (default-info rows names columns)
    (hash "name" #f "rows" rows "column-count" (length names) "columns" columns
          "identifiers" '() "suggested-targets" '()
          "source-url" #f "source-version" #f "source-sha256" #f "sha256" #f
          "license" #f "citation" #f "notes" #f "time-series" #f))
  (define (described name type missing-count)
    (hash "name" name "original-name" name "type" type "levels" '() "unit" #f
          "missing-count" missing-count "description" ""))
  (define t2-info
    (default-info 2 '("x" "s") (list (described "x" "integer" 1) (described "s" "string" 0))))
  (define (t2 #:info [info (hash)])
    (dataset-table '("x" "s") (list (list 1 dataset-missing) '("a" "b")) #:info info))
  (define core-names (dataset-names #:package "datasets-core"))
  (define generator-tables
    (list (make-circles #:n-samples 0 #:seed 0) (make-blobs #:n-samples 5 #:seed 0)
          (make-regression #:n-samples 4 #:n-features 2 #:n-targets 3 #:seed 0)))

  (test-case "T1 exact: accessors return the inputs, for list and vector columns"
    (for* ([example (in-list '((("a") ((1 2 3)))
                            (("a" "b") (() ()))
                            (("x" "i") ((-0.0 +nan.0 +inf.0 -inf.0 5e-324 0 1.7976931348623157e308)
                                        (0 1 2 3 4 5 6)))
                            (("s" "m") (("p" "q") (missing 2.5)))))]
           [shape (in-list (list values list->vector))])
      (match-define (list names raw) example)
      (define columns (map datum->cells raw))
      (define u (dataset-table names (map shape columns)))
      (check-equal? (dataset-table-names u) names)
      (check-equal? (dataset-table-columns u) (list->vector (map list->vector columns)))
      (check-equal? (dataset-table-row-count u) (length (car columns)))
      (for ([n (in-list names)] [c (in-list columns)])
        (check-equal? (dataset-table-column u n) (list->vector c)))
      (check-exn #rx"unknown column" (λ () (dataset-table-column u "absent")))))

  (test-case "T2 exact: the default metadata"
    (check-equal? (dataset-table-info (t2)) t2-info))

  (test-case "T3 exact: column types derive from the cells"
    (for ([row (in-list `((() "integer") ((missing missing) "integer") ((1 2) "integer")
                          ((1 2.5) "real") ((+nan.0) "real") ((missing 1.0) "real")
                          (("a" missing) "string") ((,(expt 2 62)) "integer")))])
      (match-define (list column type) row)
      (check-equal? (column-types (dataset-table '("c") (list (datum->cells column)))) (list type)
                    (format "~s" column))))

  (test-case "T4 exact: #:info keys replace their defaults whole"
    (check-equal? (dataset-table-info (t2 #:info (hash "name" "toy" "extra" 1)))
                  (hash-set (hash-set t2-info "name" "toy") "extra" 1))
    (check-equal? (hash-ref (dataset-table-info (t2 #:info (hash "columns" (list (hash "name" "x")))))
                            "columns")
                  (list (hash "name" "x")))
    (define from-mutable (dataset-table-info (t2 #:info (make-hash '(("name" . "toy"))))))
    (check-true (immutable? from-mutable))
    (check-equal? from-mutable (hash-set t2-info "name" "toy")))

  (define (mutable-inputs)
    (values (string-copy "a")
            (vector (string-copy "a") (string-copy "b"))
            (make-hash (list (cons "notes" (string-copy "n"))
                             (list "levels" (string-copy "l"))))))
  (define (snapshot)
    (dataset-table '("a") (list (vector "a" "b")) #:info (hash "notes" "n" "levels" '("l"))))

  (test-case "T5 exact: mutable inputs give a deeply immutable table"
    (define-values (name column info) (mutable-inputs))
    (define u (dataset-table (list name) (list column) #:info info))
    (check-true (deeply-immutable? (dataset-table-names u)))
    (check-true (deeply-immutable? (dataset-table-columns u)))
    (check-true (deeply-immutable? (dataset-table-info u))))

  (test-case "T6 exact: later changes to the arguments do not reach the table"
    (define-values (name column info) (mutable-inputs))
    (define u (dataset-table (list name) (list column) #:info info))
    (check-equal? u (snapshot))
    (vector-set! column 0 "z")
    (string-set! (vector-ref column 1) 0 #\z)
    (string-set! name 0 #\z)
    (hash-set! info "notes" "changed")
    (string-set! (car (hash-ref info "levels")) 0 #\z)
    (check-equal? u (snapshot)))

  (test-case "T6 exact: a chaperone armed after construction does not reach the table"
    (define armed? (box #f))
    (define column
      (chaperone-vector (vector-immutable 1 2 3)
                        (λ (_ i x)
                          (when (unbox armed?) (error 'chaperone "read ~a" i))
                          x)
                        (λ (_ _i x) x)))
    (define u (dataset-table '("a") (list column)))
    (set-box! armed? #t)
    (check-equal? (for/list ([x (in-vector (dataset-table-column u "a"))]) x) '(1 2 3)))

  (test-case "T7 exact: rebuilding a loaded or generated table gives an equal table"
    (define loaded
      (append-map (λ (name)
                    (define t (load-dataset-table name))
                    (list t (load-dataset-table name #:columns (reverse (dataset-table-names t)))))
                  core-names))
    (for ([t (in-list (append loaded generator-tables))])
      (check-equal? (rebuild t) t)
      (check-equal? (rebuild (rebuild t)) t)))

  (for ([row (in-list
              `(((dataset-table '("a" "a") '((1) (2))) "contract violation;\n duplicate column\n")
                ((dataset-table '("a" "b") '((1))) "contract violation;\n one column per name\n")
                ((dataset-table '("a") '((1) (2))) "contract violation;\n one column per name\n")
                ((dataset-table '("a" "b") '((1 2) (3)))
                 "contract violation;\n every column the same length\n")
                ((dataset-table '() '()) "the names argument")
                ((dataset-table '(a) '((1))) "expected: string[?]")
                ((dataset-table '("a") '(5)) "expected: dataset-column/c")
                ((dataset-table '("a") '((1/3))) "expected: dataset-column/c")
                ((dataset-table '("a") (list (list (expt 2 63)))) "expected: dataset-column/c")
                ((dataset-table '("a") (list (list (- -1 (expt 2 63))))) "expected: dataset-column/c")
                ((dataset-table '("a") '((#t))) "expected: dataset-column/c")
                ((dataset-table '("a") '((missing))) "expected: dataset-column/c")
                ((dataset-table '("a") '((1 "x"))) "expected: dataset-column/c")
                ((dataset-table '("a") '((1.5 "x"))) "expected: dataset-column/c")
                ((dataset-table '("a") '((1)) #:info 5) "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hash 'name "x")) "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hash "columns" 5)) "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hash "columns" (list (hash "type" "real"))))
                 "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hash "notes" (vector 1))) "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hash "notes" (box 1))) "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hash "notes" (bytes 1))) "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hash "notes" (list (hash 'k 1))))
                 "expected: dataset-info/c")
                ((dataset-table '("a") '((1)) #:info (hasheq (string-copy "columns") 5))
                 "expected: dataset-info/c")
                ((dataset-table '("a") '((1))
                                #:info (hasheq (string-copy "notes") 1 (string-copy "notes") 2))
                 "expected: dataset-info/c")
                ((dataset-table '("a") '((1))
                                #:info (let ([h (make-hash)]) (hash-set! h "notes" h) h))
                 "expected: dataset-info/c")
                ((dataset-table '("a") '((1))
                                #:info (let ([h (make-hash)]) (hash-set! h "notes" (list 1 h)) h))
                 "expected: dataset-info/c")
                ((dataset-table '("a" "b") '(() ())) accepted)
                ((dataset-table '("a") (list (list dataset-missing))) accepted)
                ((dataset-table '("a") (list (vector 1 2))) accepted)
                ((dataset-table '("a") (list (list (- (expt 2 63)) (sub1 (expt 2 63)))))
                 accepted)
                ((dataset-table '("a") (list (list 1 2.5 dataset-missing))) accepted)
                ((dataset-table '("s") (list (list (string #\x) dataset-missing))) accepted)
                ((dataset-table '("a") '((1)) #:info (make-hash (list (cons "name" "toy"))))
                 accepted)
                ((dataset-table '("a") '((1))
                                #:info (hash "notes" (list 'k 1 2.5 #t '() (hash "k" (list "v")))))
                 accepted)
                ((dataset-table '("a") '((1))
                                #:info (let ([shared (list "v")]) (hash "notes" (list shared shared))))
                 accepted)
                ((dataset-table '("a") '((1))
                                #:info (hash "notes" (for/fold ([v "leaf"]) ([_ (in-range 40)])
                                                       (list v v))))
                 accepted)
                ((dataset-table '("a") '((1))
                                #:info (hash "notes" (for/fold ([v "leaf"]) ([_ (in-range 40)])
                                                       (hash "l" v "r" v))))
                 accepted)))])
    (match-define (list form expected) row)
    (test-case (format "T9 exact: ~s gives ~s" form expected)
      (define result (bounded-outcome form))
      (match expected
        ['accepted (check-equal? result 'accepted (format "~s" form))]
        [pattern
         (define message (format "~a" result))
         (check-regexp-match (regexp pattern) message)
         (check-regexp-match #rx"blaming: [^\n]*client" message)])))

  (test-case "T10 bounded: five million cells are checked and copied in under a second"
    (define names (for/list ([j (in-range 10)]) (format "c~a" j)))
    (define columns
      (for/list ([j (in-range 10)])
        (for/vector #:length 500000 ([i (in-range 500000)]) (exact->inexact i))))
    (collect-garbage)
    (define start (current-inexact-monotonic-milliseconds))
    (dataset-table names columns)
    (define elapsed (- (current-inexact-monotonic-milliseconds) start))
    (check-true (< elapsed 1000.0) (format "~a ms" (round elapsed)))))
