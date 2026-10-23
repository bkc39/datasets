#lang racket/base
(require (only-in datasets/core-private/resource with-resource))
(module+ test
  (require (only-in rackunit check-equal? check-exn check-false check-true test-case))

  (test-case "a break raised inside acquire still runs release"
    (define released (box #f))
    (check-exn exn:break?
               (lambda ()
                 (with-resource [r (begin0 'r (break-thread (current-thread)))
                                   (set-box! released #t)]
                   (void))))
    (check-true (unbox released)))

  (test-case "acquire and release run with breaks disabled; the body sees the caller's state"
    (define seen (box '()))
    (define (note! label) (set-box! seen (cons (list label (break-enabled)) (unbox seen))))
    (with-resource [r (note! 'acquire) (note! 'release)]
      (note! 'body))
    (check-equal? (reverse (unbox seen)) '((acquire #f) (body #t) (release #f)))
    (check-false (parameterize-break #f (with-resource [r 1 (void)] (break-enabled))))
    (check-true (with-resource [r 1 (void)] (break-enabled)))))
