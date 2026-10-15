#lang racket/base
(require (only-in datasets/core fetch-dataset load-dataset-table)
         (only-in racket/match match)
         (only-in racket/place place-channel-get place-channel-put)
         (only-in "fixture.rkt" call-with-environment call-with-registry-fixtures table-digest))
(provide main)

(define (main ch)
  (match (place-channel-get ch)
    [(list mode registry root)
     (call-with-registry-fixtures (list (string->path registry))
       (lambda ()
         (call-with-environment (hash "DATASETS_HOME" root "DATASETS_OFFLINE" #f)
           (lambda () (place-channel-put ch (run mode))))))]))

(define (run mode)
  (match mode
    ['fetch (with-handlers ([exn:fail? exn-message])
              (path->string (fetch-dataset 'toy-remote)))]
    ['read (read-until-loaded (+ (current-inexact-milliseconds) 20000))]))

(define (attempt-read)
  (with-handlers ([exn:fail? (lambda (e)
                               (values #f (if (regexp-match? #rx"not cached" (exn-message e))
                                              "not-cached"
                                              (exn-message e))))])
    (values #t (table-digest (load-dataset-table 'toy-remote #:download? #f)))))

(define (read-until-loaded deadline)
  (let loop ([outcomes '()])
    (define-values (loaded? outcome) (attempt-read))
    (cond [(or loaded? (> (current-inexact-milliseconds) deadline)) (reverse (cons outcome outcomes))]
          [else (loop (cons outcome outcomes))])))
