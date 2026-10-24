#lang racket/base
(require (only-in datasets/core-tests/fixture call-with-temporary-fixture gzip-bytes)
         (only-in file/tar tar->output)
         (only-in racket/contract/base -> any any/c contract-out hash/c list/c listof or/c)
         (only-in racket/file make-directory*)
         (only-in racket/match match)
         (only-in racket/path path-only)
         (only-in racket/port call-with-output-bytes))
(provide (contract-out
          [call-with-upstream-files (-> (listof (list/c string? upstream/c))
                                        (-> (hash/c string? complete-path? #:immutable #t) any)
                                        any)]))

(define upstream/c
  (or/c (list/c 'gz string?) (list/c 'tgz (listof (list/c string? string?))) (list/c 'plain string?)))

(define (upstream-bytes upstream)
  (match upstream
    [(list 'plain text) (string->bytes/utf-8 text)]
    [(list 'gz text) (gzip-bytes (string->bytes/utf-8 text))]
    [(list 'tgz members) (gzip-bytes (tar-bytes members))]))

(define (tar-bytes members)
  (call-with-temporary-fixture
   (lambda (dir)
     (for ([member (in-list members)])
       (write-member! dir member))
     (parameterize ([current-directory dir])
       (call-with-output-bytes
        (lambda (out)
          (tar->output (map (lambda (m) (string->path (car m))) members) out
                       #:get-timestamp (lambda (_) 0))))))))

(define (write-member! dir member)
  (match member
    [(list name text)
     (define file (build-path dir name))
     (make-directory* (path-only file))
     (call-with-output-file file (lambda (out) (write-string text out)))]))

(define (call-with-upstream-files upstream proc)
  (call-with-temporary-fixture
   (lambda (dir)
     (proc (for/hash ([file (in-list upstream)])
             (match file
               [(list name content)
                (define path (build-path dir name))
                (call-with-output-file path (lambda (out) (write-bytes (upstream-bytes content) out)))
                (values name path)]))))))
