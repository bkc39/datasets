#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/cmdline command-line)
         (only-in racket/file file->lines)
         (only-in racket/list check-duplicates drop-right filter-map last)
         (only-in racket/match match)
         (only-in racket/runtime-path define-runtime-path)
         (only-in setup/getinfo get-info/full))

(define-runtime-path repo-root "..")

(define (read-package-list root)
  (file->lines (build-path root "scripts" "packages.txt") #:line-mode 'linefeed))

(define (info-deps dir)
  (match (get-info/full dir)
    [#f '()]
    [info (info 'deps (λ () '()))]))

(define (sibling-deps dir)
  (filter-map (λ (dep)
                (match dep
                  [(or (cons (? string? name) _) (? string? name))
                   (and (regexp-match? #rx"^datasets(-|$)" name) name)]))
              (info-deps dir)))

(define (package-dirs root)
  (filter-map (λ (entry)
                (and (file-exists? (build-path root entry "info.rkt")) (path->string entry)))
              (directory-list root)))

(define (mismatch check)
  (match check
    [(list _ same same) #f]
    [_ check]))

(define (package-list-problems root)
  (define names (read-package-list root))
  (define malformed
    (filter-map (λ (name) (and (not (regexp-match? #px"^[a-z][a-z0-9-]*$" name)) `(line ,name)))
                names))
  (define duplicate (check-duplicates names))
  (cond
    [(pair? malformed) malformed]
    [duplicate `((duplicate ,duplicate))]
    [(< (length names) 2) `((count ,(length names)))]
    [else
     (filter-map mismatch
                 `((deps ,(sibling-deps (build-path root (last names))) ,(drop-right names 1))
                   (dirs ,(package-dirs root) ,(sort names string<?))))]))

(define (describe problem)
  (match problem
    [(list 'line name) (format "~s is not a package name" name)]
    [(list 'duplicate name) (format "~a is listed twice" name)]
    [(list 'count n) (format "~a packages listed; a core and an umbrella are needed" n)]
    [(list 'deps found expected)
     (format "the umbrella's info.rkt sibling deps are ~s, not ~s" found expected)]
    [(list 'dirs found expected)
     (format "the package directories are ~s, not ~s" found expected)]))

(module+ main
  (define root (command-line #:args ([root repo-root]) (path->complete-path root)))
  (define problems (package-list-problems root))
  (for ([problem (in-list problems)])
    (eprintf "check-package-list: scripts/packages.txt: ~a\n" (describe problem)))
  (exit (if (null? problems) 0 1)))

(module+ test
  (require (only-in racket/file delete-directory/files make-directory* make-temporary-directory)
           (only-in racket/list drop remove-duplicates take)
           (only-in racket/match match-define)
           rackunit)

  (struct tree (lines eol deps dirs))

  (define base-lines (read-package-list repo-root))
  (define umbrella (last base-lines))
  (define base (tree base-lines "\n" (info-deps (build-path repo-root umbrella)) base-lines))
  (define (nth i) (list-ref base-lines i))

  (define (swap xs a b)
    (map (λ (x) (cond [(equal? x a) b] [(equal? x b) a] [else x])) xs))

  (define (insert xs i x)
    (append (take xs i) (list x) (drop xs i)))

  (define (edit t change)
    (match-define (tree lines eol deps dirs) t)
    (match change
      ['(none) t]
      [`(swap-deps ,i ,j) (tree lines eol (swap deps (nth i) (nth j)) dirs)]
      [`(remove-dep ,i) (tree lines eol (remove (nth i) deps) dirs)]
      [`(add-dep ,name) (tree lines eol (append deps (list name)) dirs)]
      [`(version-dep ,i) (tree lines eol (swap deps (nth i) `(,(nth i) #:version "0.1")) dirs)]
      [`(swap-lines ,i ,j) (tree (swap lines (nth i) (nth j)) eol deps dirs)]
      [`(add-dir ,name) (tree lines eol deps (cons name dirs))]
      [`(eol ,s) (tree lines s deps dirs)]
      [`(suffix-line ,i ,s) (tree (swap lines (nth i) (string-append (nth i) s)) eol deps dirs)]
      [`(insert-line ,i ,s) (tree (insert lines i s) eol deps dirs)]
      [`(duplicate-line ,i) (tree (insert lines i (nth i)) eol deps dirs)]
      [`(keep-lines ,n) (tree (take lines n) eol deps dirs)]))

  (define (write-tree t root)
    (match-define (tree lines eol deps dirs) t)
    (make-directory* (build-path root "scripts"))
    (call-with-output-file (build-path root "scripts" "packages.txt")
      (λ (out) (for ([line (in-list lines)]) (write-string (string-append line eol) out))))
    (for ([name (in-list dirs)])
      (make-directory* (build-path root name))
      (call-with-output-file (build-path root name "info.rkt")
        (λ (out)
          (displayln "#lang info" out)
          (write `(define deps ',(if (equal? name umbrella) deps '())) out)))))

  (define (call-with-tree t proc)
    (define root (make-temporary-directory))
    (dynamic-wind void
                  (λ () (write-tree t root) (proc root))
                  (λ () (delete-directory/files root))))

  (define cases
    '([(none) ()]
      [(swap-deps 1 2) (deps)]
      [(remove-dep 1) (deps)]
      [(add-dep "datasets-extra") (deps)]
      [(swap-lines 1 2) (deps)]
      [(version-dep 1) ()]
      [(add-dir "datasets-foo") (dirs)]
      [(eol "\r\n") (line)]
      [(suffix-line 0 " ") (line)]
      [(insert-line 1 "") (line)]
      [(duplicate-line 0) (duplicate)]
      [(keep-lines 1) (count)]))

  (for ([c (in-list cases)])
    (match-define (list change expected) c)
    (call-with-tree (edit base change)
                    (λ (root)
                      (check-equal? (remove-duplicates (map car (package-list-problems root)))
                                    expected
                                    (format "~s" change))))))
