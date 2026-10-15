#lang racket/base
(require (only-in datasets/core-private/codec gunzip-sha256)
         (only-in datasets/core-private/resource with-resource)
         (only-in "fixture.rkt" call-with-temporary-fixture gzip-bytes))
(module+ test
  (require (only-in rackunit check-equal? check-exn check-false check-not-false check-true
                    test-case))
  (define (live-threads cust)
    (for/fold ([live '()]) ([v (in-list (custodian-managed-list cust (current-custodian)))])
      (cond [(custodian? v) (append (live-threads v) live)]
            [(and (thread? v) (not (thread-dead? v))) (cons v live)]
            [else live])))

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
    (check-true (with-resource [r 1 (void)] (break-enabled))))

  (test-case "a break during gunzip-sha256 leaves no running thread"
    (call-with-temporary-fixture
     (lambda (dir)
       (define big (build-path dir "big.gz"))
       (call-with-output-file big
         (lambda (out) (write-bytes (gzip-bytes (make-bytes 60000000 0)) out)))
       (with-resource [parent (make-custodian) (custodian-shutdown-all parent)]
         (define worker
           (parameterize ([current-custodian parent])
             (thread (lambda () (with-handlers ([exn:break? void]) (gunzip-sha256 big))))))
         (sleep 0.3)
         (check-false (thread-dead? worker))
         (break-thread worker)
         (check-not-false (sync/timeout 10 (thread-dead-evt worker)))
         (check-equal? (live-threads parent) '()))))))
