#lang racket/base
(require (only-in file/gunzip gunzip-through-ports)
         (only-in json write-json)
         (only-in racket/match match)
         (only-in racket/port call-with-output-bytes))

(define (read-rktd path)
  (define gzipped? (regexp-match? #rx"[.]gz$" path))
  (call-with-input-file path
    (lambda (in)
      (read (if gzipped? (open-input-bytes (call-with-output-bytes (lambda (out) (gunzip-through-ports in out)))) in)))))

(define (jsexpr v)
  (match v
    [(? hash?) (for/hasheq ([(key value) (in-hash v)]) (values (string->symbol key) (jsexpr value)))]
    [(? list?) (map jsexpr v)]
    [_ v]))

(match (current-command-line-arguments)
  [(vector path keys ...)
   (write-json (jsexpr (for/fold ([v (read-rktd path)]) ([key (in-list keys)]) (hash-ref v key))))])
