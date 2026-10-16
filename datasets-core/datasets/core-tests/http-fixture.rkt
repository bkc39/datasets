#lang racket/base
(require (only-in racket/contract/base -> any contract-out flat-named-contract hash/c integer-in
                  list/c listof or/c)
         (only-in racket/dict in-dict-keys in-dict-values)
         (only-in racket/match match)
         (only-in racket/tcp tcp-accept tcp-addresses tcp-listen))
(define response/c
  (flat-named-contract 'http-fixture-response
    (or/c (list/c 'ok bytes?) (list/c 'status (integer-in 100 599)) (list/c 'redirect string?)
          (list/c 'truncate bytes? exact-nonnegative-integer?)
          (list/c 'stall bytes? exact-nonnegative-integer?)
          (list/c 'barrier bytes? exact-positive-integer?))))
(provide (contract-out
          [call-with-http-fixture (-> (hash/c string? response/c) (-> http-fixture? any) any)]
          [http-fixture-url (-> http-fixture? string? string?)]
          [http-fixture-requests (-> http-fixture? (listof string?))]
          [http-fixture-stalled-evt (-> http-fixture? evt?)]
          [http-fixture-closed-evt (-> http-fixture? evt?)]))

(struct http-fixture (port log lock stalled closed))

(define (http-fixture-url server path)
  (format "http://127.0.0.1:~a~a" (http-fixture-port server) path))

(define (http-fixture-requests server)
  (call-with-semaphore (http-fixture-lock server)
                       (lambda () (reverse (unbox (http-fixture-log server))))))

(define (http-fixture-stalled-evt server)
  (semaphore-peek-evt (http-fixture-stalled server)))

(define (http-fixture-closed-evt server)
  (http-fixture-closed server))

(define (call-with-http-fixture routes proc)
  (define custodian (make-custodian))
  (define listener
    (parameterize ([current-custodian custodian])
      (tcp-listen 0 64 #t "127.0.0.1")))
  (define-values (_host port _remote-host _remote-port) (tcp-addresses listener #t))
  (define server
    (http-fixture port (box '()) (make-semaphore 1) (make-semaphore 0) (make-semaphore 0)))
  (define barriers (make-hash))
  (define barrier-lock (make-semaphore 1))
  (define (record! path)
    (call-with-semaphore (http-fixture-lock server)
                         (lambda () (set-box! (http-fixture-log server)
                                              (cons path (unbox (http-fixture-log server)))))))
  (define (join-barrier! path n out)
    (define done (make-semaphore 0))
    (define group
      (call-with-semaphore
       barrier-lock
       (lambda ()
         (define waiting (cons (cons out done) (hash-ref barriers path '())))
         (cond [(= (length waiting) n) (hash-remove! barriers path) waiting]
               [else (hash-set! barriers path waiting) #f]))))
    (values group done))
  (define (respond path response in out)
    (match response
      [(list 'ok body) (send-head out 200 body) (write-bytes body out)]
      [(list 'status n) (send-head out n #"")]
      [(list 'redirect location) (send-head out 302 #"" (format "Location: ~a\r\n" location))]
      [(list 'truncate body k) (send-head out 200 body) (write-bytes body out 0 k)]
      [(list 'stall body k)
       (send-head out 200 body)
       (write-bytes body out 0 k)
       (flush-output out)
       (semaphore-post (http-fixture-stalled server))
       (with-handlers ([exn:fail? void])
         (read-byte in))
       (semaphore-post (http-fixture-closed server))]
      [(list 'barrier body n)
       (define-values (group done) (join-barrier! path n out))
       (when group (release-barrier! group body))
       (semaphore-wait done)]))
  (define (serve in out)
    (with-handlers ([exn:fail? void])
      (match (regexp-match #rx"^GET ([^ ]+) HTTP/" (read-line in 'return-linefeed))
        [(list _ path)
         (skip-headers in)
         (record! path)
         (respond path (hash-ref routes path '(status 404)) in out)]
        [#f (void)]))
    (close-output-port out)
    (close-input-port in))
  (parameterize ([current-custodian custodian])
    (thread (lambda ()
              (let loop ()
                (define-values (in out) (tcp-accept listener))
                (thread (lambda () (serve in out)))
                (loop)))))
  (dynamic-wind void
                (lambda () (proc server))
                (lambda () (custodian-shutdown-all custodian))))

(define (send-head out status body [extra ""])
  (write-string (format "HTTP/1.1 ~a Fixture\r\nContent-Length: ~a\r\nConnection: close\r\n~a\r\n"
                        status (bytes-length body) extra)
                out))

(define (skip-headers in)
  (match (read-line in 'return-linefeed)
    [(or "" (? eof-object?)) (void)]
    [_ (skip-headers in)]))

(define (release-barrier! group body)
  (for ([out (in-dict-keys group)])
    (send-head out 200 body))
  (for* ([b (in-bytes body)] [out (in-dict-keys group)])
    (write-byte b out)
    (flush-output out))
  (for ([done (in-dict-values group)])
    (semaphore-post done)))
