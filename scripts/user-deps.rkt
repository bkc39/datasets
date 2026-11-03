#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/file file->lines)
         (only-in racket/list append-map remove-duplicates)
         (only-in racket/match match)
         (only-in racket/runtime-path define-runtime-path)
         (only-in pkg/lib find-pkg-installation-scope pkg-directory with-pkg-lock/read-only)
         (only-in setup/getinfo get-info/full))

(define-runtime-path repo-root "..")

(define repo-packages
  (file->lines (build-path repo-root "scripts" "packages.txt") #:line-mode 'linefeed))

(define (dep-name form)
  (match form
    [(? string? name) name]
    [(cons name _) name]))

(define (declared-deps package)
  (define info (get-info/full (build-path repo-root package)))
  (map dep-name (append (info 'deps (λ () '())) (info 'build-deps (λ () '())))))

(module+ main
  (with-pkg-lock/read-only
   (for ([name (in-list (remove-duplicates
                         (append-map declared-deps
                                     (vector->list (current-command-line-arguments)))))]
         #:unless (member name repo-packages)
         #:when (eq? (find-pkg-installation-scope name) 'user))
     (printf "~a ~a\n" name (pkg-directory name)))))
