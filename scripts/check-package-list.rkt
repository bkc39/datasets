#lang racket/base
(require (only-in racket/file file->lines)
         (only-in racket/list drop-right last)
         (only-in racket/match match match-define)
         (only-in racket/string string-join)
         (only-in setup/getinfo get-info/full))

(define packages (file->lines "scripts/packages.txt"))
(define umbrella (last packages))
(define siblings (drop-right packages 1))

(define checkout-install
  (list (string-join (map (λ (p) (string-append "./" p)) siblings)
                     " "
                     #:before-first "raco pkg install ")
        (string-append "raco pkg install --auto ./" umbrella)))

(define (checkout-install-lines file)
  (for*/list ([line (in-list (file->lines file))]
              [m (in-value (regexp-match #rx"raco pkg install (--auto )?[.]/[^}]*" line))]
              #:when m)
    (car m)))

(define (repo-deps dir)
  (for/list ([dep (in-list ((get-info/full (path->complete-path dir)) 'deps))]
             #:when (and (string? dep) (regexp-match? #rx"^datasets(-|$)" dep)))
    dep))

(define copies
  `(("datasets/info.rkt deps" ,(repo-deps umbrella) ,siblings)
    ("README.md install" ,(checkout-install-lines "README.md") ,checkout-install)
    ("datasets guide install"
     ,(checkout-install-lines "datasets/datasets/main-doc/guide.scrbl")
     ,checkout-install)))

(define mismatches
  (for/list ([copy (in-list copies)]
             #:unless (match copy [(list _ actual expected) (equal? actual expected)]))
    copy))

(for ([copy (in-list mismatches)])
  (match-define (list label actual expected) copy)
  (eprintf "check-package-list: ~a disagrees with scripts/packages.txt\n  found:    ~s\n  expected: ~s\n"
           label actual expected))
(exit (if (null? mismatches) 0 1))
