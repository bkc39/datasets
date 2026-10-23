#lang racket/base
(require (only-in datasets/core clear-dataset-cache dataset-cached? fetch-dataset load-dataset-table)
         (only-in datasets/core-private/resource with-resource)
         (only-in racket/match match-define)
         (only-in "fixture.rkt" fetch-entry fetch-error? gzip-bytes hex rows->rktd-bytes table-digest
                  toy-artifact toy-at toy-table-digest with-server)
         (only-in "http-fixture.rkt" http-fixture-requests))
(module+ test
  (require (only-in racket/sandbox kill-evaluator make-evaluator sandbox-path-permissions)
           (only-in rackunit check-equal? check-exn check-false check-true test-case))
  (define flipped (bytes-copy toy-artifact))
  (bytes-set! flipped 16 (bitwise-xor 1 (bytes-ref flipped 16)))
  (define (no-remedy? e) (not (regexp-match? #rx"clear-dataset-cache|remedy:" (exn-message e))))

  (test-case "C20 a corrupt cache file raises with remedy, never re-downloads"
    (define unreadable #rx"^load-dataset-table: cannot read dataset data")
    (define mismatch #rx"^load-dataset-table: data sha256 mismatch")
    (define corrupt-cases
      (list (cons (subbytes toy-artifact 0 30) mismatch)
            (cons #"not gzip" unreadable)
            (cons (subbytes toy-artifact 0 10) unreadable)
            (cons flipped mismatch)))
    (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
      (lambda (srv root registry)
        (for ([corrupt-case (in-list corrupt-cases)]
              [downloads (in-naturals 1)])
          (match-define (cons corrupt message) corrupt-case)
          (define p (fetch-dataset 'toy-remote))
          (call-with-output-file p #:exists 'truncate (lambda (o) (write-bytes corrupt o)))
          (check-exn (fetch-error? message (path->string p) "(clear-dataset-cache 'toy-remote)")
                     (lambda () (load-dataset-table 'toy-remote)))
          (check-true (dataset-cached? 'toy-remote))
          (check-equal? (http-fixture-requests srv) (build-list downloads (lambda (_) "/toy")))
          (clear-dataset-cache 'toy-remote)
          (check-equal? (table-digest (load-dataset-table 'toy-remote)) toy-table-digest))
        (define p (fetch-dataset 'toy-remote))
        (call-with-output-file p #:exists 'truncate
          (lambda (o) (write-bytes (subbytes toy-artifact 0 (- (bytes-length toy-artifact) 8)) o)))
        (check-equal? (table-digest (load-dataset-table 'toy-remote)) toy-table-digest))))

  (test-case "C20b a registry sha256 that disagrees with the pinned download has no remedy"
    (define registry-error
      (fetch-error? #rx"^load-dataset-table: registry sha256 does not match the pinned download"
                    "dataset: toy-remote" "package: datasets-fetch-fixture"))
    (with-server (hash "/toy" `(ok ,toy-artifact))
                 (lambda (u) (list (fetch-entry (list (u "/toy")) #:data-sha256 (make-string 64 #\b))))
      (lambda (srv root registry)
        (check-true (file-exists? (fetch-dataset 'toy-remote)))
        (for ([_ (in-range 2)])
          (check-exn (lambda (e) (and (registry-error e) (no-remedy? e)))
                     (lambda () (load-dataset-table 'toy-remote))))
        (check-equal? (http-fixture-requests srv) '("/toy")))))

  (test-case "C20c an unreadable pinned download has no remedy"
    (define registry-error
      (fetch-error? #rx"^load-dataset-table: cannot read the pinned download"
                    "dataset: toy-remote" "package: datasets-fetch-fixture"))
    (define plain (rows->rktd-bytes '((1 "a" 0.5))))
    (with-server (hash "/toy" `(ok ,plain))
                 (lambda (u) (list (fetch-entry (list (u "/toy")) #:sha256 (hex (sha256-bytes plain)))))
      (lambda (srv root registry)
        (check-true (file-exists? (fetch-dataset 'toy-remote)))
        (for ([_ (in-range 2)])
          (check-exn (lambda (e) (and (registry-error e) (no-remedy? e)))
                     (lambda () (load-dataset-table 'toy-remote))))
        (check-equal? (http-fixture-requests srv) '("/toy")))))

  (test-case "C20d a cached file the caller cannot read raises with remedy"
    (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
      (lambda (srv root registry)
        (define p (fetch-dataset 'toy-remote))
        (define elsewhere
          (byte-pregexp (bytes-append #"^(?!" (regexp-quote (path->bytes p)) #"$)")))
        (with-resource [ev (parameterize ([sandbox-path-permissions `((exists ,p) (read ,elsewhere))])
                             (make-evaluator 'racket/base '(require datasets/core)))
                           (kill-evaluator ev)]
          (check-exn (lambda (e)
                       ((fetch-error? #rx"^load-dataset-table: cannot read dataset data"
                                      (path->string p) "(clear-dataset-cache 'toy-remote)")
                        e))
                     (lambda () (ev '(load-dataset-table 'toy-remote)))))
        (check-equal? (http-fixture-requests srv) '("/toy")))))

  (test-case "C20e data that matches its sha256 but does not parse has no remedy"
    (define registry-error
      (fetch-error? #rx"^load-dataset-table: cannot parse dataset data"
                    "dataset: toy-remote" "package: datasets-fetch-fixture"))
    (define truncated #"(1 2")
    (define pinned (gzip-bytes truncated))
    (define recompressed (gzip-bytes truncated 1))
    (check-false (equal? pinned recompressed))
    (with-server (hash "/toy" `(ok ,pinned))
                 (lambda (u)
                   (list (fetch-entry (list (u "/toy")) #:sha256 (hex (sha256-bytes pinned))
                                      #:data-sha256 (hex (sha256-bytes truncated)))))
      (lambda (srv root registry)
        (define p (fetch-dataset 'toy-remote))
        (for ([cached (list pinned recompressed)])
          (call-with-output-file p #:exists 'truncate (lambda (o) (write-bytes cached o)))
          (check-exn (lambda (e) (and (registry-error e) (no-remedy? e)))
                     (lambda () (load-dataset-table 'toy-remote))))
        (check-equal? (http-fixture-requests srv) '("/toy")))))

  (test-case "C20f a mismatched cache file that cannot be re-hashed keeps the remedy"
    (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
      (lambda (srv root registry)
        (define p (fetch-dataset 'toy-remote))
        (call-with-output-file p #:exists 'truncate (lambda (o) (write-bytes flipped o)))
        (define opens 0)
        (define (deny-second-read who path modes)
          (define ours? (and path (memq 'read modes) (equal? (simplify-path path) p)))
          (set! opens (+ opens (if ours? 1 0)))
          (when (and ours? (= opens 2)) (error who "denied: ~a" path)))
        (check-exn (fetch-error? #rx"^load-dataset-table: data sha256 mismatch"
                                 "(clear-dataset-cache 'toy-remote)")
                   (lambda ()
                     (parameterize ([current-security-guard
                                     (make-security-guard (current-security-guard)
                                                          deny-second-read void)])
                       (load-dataset-table 'toy-remote))))
        (check-equal? opens 2)
        (check-equal? (http-fixture-requests srv) '("/toy"))))))
