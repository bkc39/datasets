#lang racket/base
(require (only-in datasets/core-private/resource with-custodian)
         (only-in file/gunzip gunzip-through-ports)
         (only-in file/gzip gzip-through-ports)
         (only-in racket/match match)
         (only-in racket/port copy-port open-output-nowhere))
(provide call-with-gunzip-sha256 call-with-gzip-sha256 file-sha256 hex)

(define pipe-limit 65536)

(define (hex bs)
  (apply string-append
         (for/list ([b (in-bytes bs)])
           (string-append (if (< b 16) "0" "") (number->string b 16)))))

(define (file-sha256 path)
  (call-with-input-file path (lambda (in) (hex (sha256-bytes in)))))

(define (drain in)
  (copy-port in (open-output-nowhere)))

(define (worker thunk [release void])
  (thread #:keep 'results
          (lambda () (begin0 (with-handlers ([exn:fail? values]) (thunk)) (release)))))

(define (tee source)
  (define-values (digest-in digest-out) (make-pipe pipe-limit))
  (define-values (copy-in copy-out) (make-pipe pipe-limit))
  (define digest (worker (lambda () (sha256-bytes digest-in))))
  (thread (lambda ()
            (copy-port source digest-out copy-out)
            (close-output-port digest-out)
            (close-output-port copy-out)))
  (values copy-in digest))

(define (finish io digest outcome failure)
  (match (thread-wait io)
    [(? exn? e) (failure e)]
    [_ (values outcome (hex (thread-wait digest)))]))

(define (call-with-gunzip-sha256 path consume failure)
  (with-custodian
    (define-values (source sink) (make-pipe pipe-limit))
    (define inflater
      (worker (lambda () (call-with-input-file path (lambda (in) (gunzip-through-ports in sink))))
              (lambda () (close-output-port sink))))
    (define-values (text digest) (tee source))
    (define outcome (consume text))
    (drain text)
    (finish inflater digest outcome failure)))

(define (call-with-gzip-sha256 path produce failure)
  (with-custodian
    (define-values (source sink) (make-pipe pipe-limit))
    (define-values (text digest) (tee source))
    (define deflater
      (worker (lambda ()
                (call-with-output-file path #:exists 'truncate
                  (lambda (out) (gzip-through-ports text out #f 0))))
              (lambda () (drain text))))
    (define outcome (produce sink))
    (close-output-port sink)
    (finish deflater digest outcome failure)))
