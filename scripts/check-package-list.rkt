#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/cmdline command-line)
         (only-in racket/file file->lines)
         (only-in racket/list append-map check-duplicates drop-right filter-map last take)
         (only-in racket/match == match)
         (only-in racket/runtime-path define-runtime-path)
         (only-in setup/getinfo get-info/full))

(define-runtime-path repo-root "..")

(define (read-package-list root)
  (file->lines (build-path root "scripts" "packages.txt") #:line-mode 'linefeed))

(define (info-field root name key)
  (match (get-info/full (build-path root name))
    [#f '()]
    [info (info key (λ () '()))]))

(define (dep-options? options)
  (match options
    ['() #t]
    [(list* '#:version (? string?) more) (dep-options? more)]
    [(list* '#:platform _ more) (dep-options? more)]
    [_ #f]))

(define (dep-name form)
  (match form
    [(or (? string? name) (list (? string? name) (? string?))) name]
    [(list* (? string? name) (? dep-options?)) name]
    [_ #f]))

(define (dep-problem name form)
  (match (dep-name form)
    [#f `(dep ,name ,form)]
    [(== name) `(self ,name)]
    [_ #f]))

(define (field-problems name field)
  (match field
    [(? list? forms) (filter-map (λ (form) (dep-problem name form)) forms)]
    [form `((dep ,name ,form))]))

(define (form-problems root name)
  (match (with-handlers ([exn:fail? exn-message])
           (map (λ (key) (info-field root name key)) '(deps build-deps)))
    [(? string? message) `((info ,name ,message))]
    [fields (append-map (λ (field) (field-problems name field)) fields)]))

(define (listed-deps root name keys names)
  (for*/list ([key (in-list keys)]
              [dep (in-list (map dep-name (info-field root name key)))]
              #:when (member dep names))
    dep))

(define (package-dirs root)
  (filter-map (λ (entry)
                (and (file-exists? (build-path root entry "info.rkt")) (path->string entry)))
              (directory-list root)))

(define (mismatch check)
  (match check
    [(list _ same same) #f]
    [_ check]))

(define (order-problems root names packages)
  (for*/list ([i (in-range (length packages))]
              [dep (in-list (listed-deps root (list-ref packages i) '(deps build-deps) names))]
              #:unless (member dep (take packages i)))
    `(order ,(list-ref packages i) ,dep)))

(define (package-list-problems root)
  (define names (read-package-list root))
  (define malformed
    (filter-map (λ (name) (and (not (regexp-match? #px"^[a-z][a-z0-9-]*$" name)) `(line ,name)))
                names))
  (define duplicate (check-duplicates names))
  (define forms (append-map (λ (name) (form-problems root name)) names))
  (cond
    [(pair? malformed) malformed]
    [duplicate `((duplicate ,duplicate))]
    [(< (length names) 2) `((count ,(length names)))]
    [(pair? forms) forms]
    [else
     (define packages (drop-right names 1))
     (append (order-problems root names packages)
             (filter-map mismatch
                         `((deps ,(sort (listed-deps root (last names) '(deps) names) string<?)
                                 ,(sort packages string<?))
                           (dirs ,(package-dirs root) ,(sort names string<?)))))]))

(define (describe problem)
  (match problem
    [(list 'line name) (format "scripts/packages.txt: ~s is not a package name" name)]
    [(list 'duplicate name) (format "scripts/packages.txt: ~a is listed twice" name)]
    [(list 'count n)
     (format "scripts/packages.txt: ~a packages listed; a core and an umbrella are needed" n)]
    [(list 'info name message) (format "~a/info.rkt: cannot be read: ~a" name message)]
    [(list 'dep name form) (format "~a/info.rkt: ~s is not a dependency form" name form)]
    [(list 'self name) (format "~a/info.rkt: the package depends on itself" name)]
    [(list 'order name dep)
     (format "scripts/packages.txt: ~a depends on ~a, which is not listed before it" name dep)]
    [(list 'deps found expected)
     (format "scripts/packages.txt: the umbrella's info.rkt deps on listed packages are ~s, not ~s"
             found
             expected)]
    [(list 'dirs found expected)
     (format "scripts/packages.txt: the package directories are ~s, not ~s" found expected)]))

(module+ main
  (define root (command-line #:args ([root repo-root]) (path->complete-path root)))
  (define problems (package-list-problems root))
  (for ([problem (in-list problems)])
    (eprintf "check-package-list: ~a\n" (describe problem)))
  (exit (if (null? problems) 0 1)))

(module+ test
  (require (only-in racket/file delete-directory/files make-directory* make-temporary-directory)
           (only-in racket/list drop remove-duplicates)
           (only-in racket/match match-define)
           (only-in racket/port with-output-to-string)
           (only-in racket/string string-prefix?)
           (only-in racket/system system*)
           rackunit)

  (struct tree (lines eol infos dirs))

  (define fixture-lines '("datasets-core" "datasets-r" "datasets-fetch" "datasets"))
  (define umbrella (last fixture-lines))
  (define fixture
    (tree fixture-lines
          "\n"
          (hash "datasets-core" (hash 'deps '(("base" #:version "9.3")) 'build-deps '("rackunit-lib"))
                "datasets-r" (hash 'deps '(("base" #:version "9.3") "datasets-core"))
                "datasets-fetch" (hash 'deps '(("datasets-core" #:version "0.2") "threading-lib")
                                       'build-deps '("rackunit-lib"))
                "datasets" (hash 'deps '(("datasets-core" #:version "0.2")
                                         ("datasets-r" #:version "0.1")
                                         ("datasets-fetch" #:version "0.1")
                                         "polars")
                                 'build-deps '("scribble-lib")))
          fixture-lines))
  (define (nth i) (list-ref fixture-lines i))

  (define (swap xs a b [key values])
    (define (pick k) (findf (λ (x) (equal? (key x) k)) xs))
    (for/list ([x (in-list xs)])
      (cond
        [(equal? (key x) a) (pick b)]
        [(equal? (key x) b) (pick a)]
        [else x])))

  (define (insert xs i x)
    (append (take xs i) (list x) (drop xs i)))

  (define (edit-info t name proc)
    (match-define (tree lines eol infos dirs) t)
    (tree lines eol (hash-update infos name proc (hash)) dirs))

  (define (edit-field t key name proc)
    (edit-info t name (λ (fields) (hash-update fields key proc '()))))

  (define (edit t change)
    (match-define (tree lines eol infos dirs) t)
    (match change
      [`(swap-deps ,i ,j) (edit-field t 'deps umbrella (λ (ds) (swap ds (nth i) (nth j) dep-name)))]
      [`(remove-dep ,i)
       (edit-field t
                   'deps
                   umbrella
                   (λ (ds) (remove (nth i) ds (λ (name d) (equal? name (dep-name d))))))]
      ['(sort-deps) (edit-field t 'deps umbrella (λ (ds) (sort ds string<? #:key dep-name)))]
      [`(add ,key ,name ,dep) (edit-field t key name (λ (ds) (append ds (list dep))))]
      [`(set ,key ,name ,field) (edit-field t key name (λ (_) field))]
      [`(info-text ,name ,text) (edit-info t name (λ (_) text))]
      [`(swap-lines ,i ,j) (tree (swap lines (nth i) (nth j)) eol infos dirs)]
      [`(add-dir ,name) (tree lines eol infos (cons name dirs))]
      [`(eol ,s) (tree lines s infos dirs)]
      [`(suffix-line ,i ,s)
       (tree (map (λ (x) (if (equal? x (nth i)) (string-append x s) x)) lines) eol infos dirs)]
      [`(insert-line ,i ,s) (tree (insert lines i s) eol infos dirs)]
      [`(duplicate-line ,i) (tree (insert lines i (nth i)) eol infos dirs)]
      [`(keep-lines ,n) (tree (take lines n) eol infos dirs)]))

  (define (write-info info out)
    (match info
      [(? string? text) (write-string text out)]
      [fields
       (displayln "#lang info" out)
       (for ([(key field) (in-hash fields)])
         (writeln `(define ,key ',field) out))]))

  (define (write-tree t root)
    (match-define (tree lines eol infos dirs) t)
    (make-directory* (build-path root "scripts"))
    (call-with-output-file (build-path root "scripts" "packages.txt")
      (λ (out) (for ([line (in-list lines)]) (write-string (string-append line eol) out))))
    (for ([name (in-list dirs)])
      (make-directory* (build-path root name))
      (call-with-output-file (build-path root name "info.rkt")
        (λ (out) (write-info (hash-ref infos name (hash)) out)))))

  (define (call-with-tree t proc)
    (define root (make-temporary-directory))
    (dynamic-wind void
                  (λ () (write-tree t root) (proc root))
                  (λ () (delete-directory/files root))))

  (define cases
    '([() ()]
      [((swap-deps 1 2)) ()]
      [((sort-deps)) ()]
      [((swap-lines 1 2)) ()]
      [((remove-dep 1)) (deps)]
      [((add deps "datasets" "datasets-extra")) ()]
      [((swap-lines 0 1)) (order)]
      [((swap-lines 0 1) (swap-deps 0 1)) (order)]
      [((add deps "datasets-core" "datasets-r")) (order)]
      [((add deps "datasets-r" ("datasets-fetch" #:version "0.1"))) (order)]
      [((add build-deps "datasets-r" ("datasets-fetch" #:version "0.1"))) (order)]
      [((add build-deps "datasets-fetch" ("datasets-r" #:version "0.1"))) ()]
      [((add build-deps "datasets" "datasets-core")) ()]
      [((insert-line 1 "extra-pkg")
        (add-dir "extra-pkg")
        (add deps "datasets" "extra-pkg")
        (add deps "datasets-r" "extra-pkg"))
       ()]
      [((add deps "datasets-r" datasets-core)) (dep)]
      [((add deps "datasets-r" ("datasets-core" . "0.2"))) (dep)]
      [((add deps "datasets-r" ("datasets-core" "0.2" junk))) (dep)]
      [((add deps "datasets-r" ("datasets-core" #:version 0.2))) (dep)]
      [((add deps "datasets-r" ("datasets-core" #:platform unix #:version))) (dep)]
      [((add deps "datasets-r" ("datasets-core" "0.2"))) ()]
      [((add deps "datasets-r" ("datasets-core" #:platform unix #:version "0.2"))) ()]
      [((add deps "datasets-r" "datasets-r")) (self)]
      [((add deps "datasets" ("datasets" #:version "0.1"))) (self)]
      [((add build-deps "datasets" "datasets")) (self)]
      [((set build-deps "datasets-r" "rackunit-lib")) (dep)]
      [((info-text "datasets-r" "#lang info\n(define deps")) (info)]
      [((add-dir "datasets-foo")) (dirs)]
      [((eol "\r\n")) (line)]
      [((suffix-line 0 " ")) (line)]
      [((insert-line 1 "")) (line)]
      [((duplicate-line 0)) (duplicate)]
      [((keep-lines 1)) (count)]))

  (define (problem-file problem)
    (match problem
      [(list (or 'info 'dep 'self) name _ ...) (format "~a/info.rkt: " name)]
      [_ "scripts/packages.txt: "]))

  (for ([c (in-list cases)])
    (match-define (list changes expected) c)
    (call-with-tree (foldl (λ (change t) (edit t change)) fixture changes)
                    (λ (root)
                      (define problems (package-list-problems root))
                      (define label (format "~s" changes))
                      (check-equal? (remove-duplicates (map car problems)) expected label)
                      (for ([problem (in-list problems)])
                        (check-true (string-prefix? (describe problem) (problem-file problem))
                                    label)))))

  (define (script-calls-raco? t script . args)
    (call-with-tree
     t
     (λ (root)
       (for ([file (in-list '("check-package-list.rkt" "check.sh" "install-check.sh" "packages.sh"))])
         (copy-file (build-path repo-root "scripts" file) (build-path root "scripts" file)))
       (define bin (build-path root "bin"))
       (define marker (build-path root "raco-called"))
       (make-directory* bin)
       (call-with-output-file (build-path bin "raco")
         (λ (out) (fprintf out "#!/bin/sh\ntouch ~s\n" (path->string marker))))
       (file-or-directory-permissions (build-path bin "raco") #o755)
       (define env (environment-variables-copy (current-environment-variables)))
       (environment-variables-set! env
                                   #"PATH"
                                   (bytes-append (path->bytes bin)
                                                 #":"
                                                 (environment-variables-ref env #"PATH")))
       (define output
         (with-output-to-string
          (λ ()
            (parameterize ([current-error-port (current-output-port)]
                           [current-environment-variables env])
              (apply system*
                     (find-executable-path "bash")
                     (build-path root "scripts" script)
                     (map (λ (arg) (if (eq? arg 'root) (path->string root) arg)) args))))))
       (list (file-exists? marker) (regexp-match? #rx"check-package-list: " output)))))

  (for ([c (in-list '([(eol "\r\n") "install-check.sh" root "core"]
                      [(keep-lines 0) "install-check.sh" root "core"]
                      [(keep-lines 0) "check.sh" "core"]))])
    (match-define (list change script args ...) c)
    (check-equal? (apply script-calls-raco? (edit fixture change) script args)
                  '(#f #t)
                  (format "~s" c))))
