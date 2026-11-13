#lang racket/base
(require (for-syntax racket/base)
         (only-in pkg/lib find-pkg-installation-scope pkg-directory with-pkg-lock/read-only)
         (only-in racket/file file->lines)
         (only-in racket/function curry negate)
         (only-in racket/list takef)
         (only-in racket/match match)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/set set set-add set-member?)
         (only-in racket/string string-join)
         (only-in setup/getinfo get-info/full))

(define-runtime-path repo-root "..")

(define repo-packages
  (file->lines (build-path repo-root "scripts" "packages.txt") #:line-mode 'linefeed))

(define (dep-name form)
  (match form
    [(? string? name) name]
    [(cons name _) name]))

(define (dependency-closure roots deps-of)
  (define (visit name path seen order)
    (cond
      [(member name path)
       (define loop (takef path (negate (curry equal? name))))
       (raise-user-error 'user-deps
                         "dependency cycle: ~a"
                         (string-join (cons name (append (reverse loop) (list name))) " -> "))]
      [(set-member? seen name) (values seen order)]
      [else
       (define-values (seen* order*)
         (for/fold ([seen (set-add seen name)] [order order]) ([dep (in-list (deps-of name))])
           (visit dep (cons name path) seen order)))
       (values seen* (cons name order*))]))
  (for/fold ([seen (set)] [order '()] #:result (reverse order)) ([root (in-list roots)])
    (visit root '() seen order)))

(define (user-package? name)
  (and (not (member name repo-packages)) (eq? (find-pkg-installation-scope name) 'user)))

(define (declared-deps name)
  (define dir
    (cond
      [(member name repo-packages) (build-path repo-root name)]
      [(user-package? name) (pkg-directory name)]
      [else #f]))
  (define info (and dir (get-info/full dir)))
  (cond
    [info (map dep-name (append (info 'deps (λ () '())) (info 'build-deps (λ () '()))))]
    [else '()]))

(module+ main
  (with-pkg-lock/read-only
   (for ([name (in-list (dependency-closure (vector->list (current-command-line-arguments))
                                            declared-deps))]
         #:when (user-package? name))
     (printf "~a ~a\n" name (pkg-directory name)))))

(module+ test
  (require (only-in rackunit check-equal? check-exn))
  (define graph
    (hash "core" '("base" "lib-a")
          "family" '("core" "lib-b")
          "lib-a" '("lib-c")
          "lib-b" '("lib-a" "lib-d")
          "cyclic" '("lib-a" "lib-e")
          "lib-e" '("lib-f")
          "lib-f" '("lib-c" "lib-e")))
  (define (deps-of name)
    (hash-ref graph name '()))
  (check-equal? (dependency-closure '("core") deps-of) '("base" "lib-c" "lib-a" "core"))
  (check-equal? (dependency-closure '("core" "family") deps-of)
                '("base" "lib-c" "lib-a" "core" "lib-d" "lib-b" "family"))
  (check-equal? (dependency-closure '("family") deps-of)
                '("base" "lib-c" "lib-a" "core" "lib-d" "lib-b" "family"))
  (check-exn #rx"^user-deps: dependency cycle: lib-e -> lib-f -> lib-e$"
             (λ () (dependency-closure '("core" "cyclic") deps-of))))
