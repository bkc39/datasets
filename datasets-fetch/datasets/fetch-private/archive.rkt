#lang racket/base
(require (only-in datasets/fetch-private/bytes-port bytes->input-port)
         (only-in file/gunzip gunzip-through-ports inflate)
         (only-in file/untar untar)
         (only-in racket/contract/base -> contract-out)
         (only-in racket/port call-with-output-bytes)
         (only-in threading ~>))
(provide (contract-out
          [gunzip-file (-> path-string? bytes?)]
          [tar-gz-member (-> path-string? string? bytes?)]
          [inflate-zlib (-> bytes? exact-nonnegative-integer? bytes?)]))

(define (gunzip-file path)
  (call-with-input-file path
    (lambda (in) (call-with-output-bytes (lambda (out) (gunzip-through-ports in out))))))

(define (tar-gz-member archive member)
  (let/ec return
    (~> archive
        gunzip-file
        open-input-bytes
        (untar #:filter (lambda (name _stripped type _size _link _time _mode)
                          (and (eq? type 'file) (equal? (path->string name) member)))
               #:handle-entry (lambda (_kind _path in size _attribs) (return (read-bytes size in)))))
    (error 'tar-gz-member "no such member\n  archive: ~a\n  member: ~a" archive member)))

(define (inflate-zlib data limit)
  (define size (bytes-length data))
  (define cmf (and (>= size 2) (bytes-ref data 0)))
  (define flg (and cmf (bytes-ref data 1)))
  (define zlib? (and cmf (= (bitwise-and cmf 15) 8) (<= (arithmetic-shift cmf -4) 7)
                     (zero? (modulo (+ (* cmf 256) flg) 31))))
  (define dictionary? (and zlib? (bitwise-bit-set? flg 5)))
  (define-values (in end-seen?) (bytes->input-port data (min size 2)))
  (define (trailer) (- size 2 (file-position in)))
  (define (truncated?) (or (end-seen?) (< (trailer) 4)))
  (define (invalid e)
    (when (truncated?)
      (error 'inflate-zlib "truncated zlib stream"))
    (error 'inflate-zlib "invalid deflate data\n  detail: ~a" (exn-message e)))
  (define inflated (and zlib? (not dictionary?) (inflate-limited in limit invalid)))
  (cond
    [(not cmf) (error 'inflate-zlib "truncated zlib stream")]
    [(not zlib?) (error 'inflate-zlib "not a zlib stream")]
    [dictionary? (error 'inflate-zlib "preset dictionary not supported")]
    [(truncated?) (error 'inflate-zlib "truncated zlib stream")]
    [(> (trailer) 4) (error 'inflate-zlib "trailing data after the zlib stream")]
    [(= (adler-32 inflated) (integer-bytes->integer data #f #t (- size 4) size)) inflated]
    [else (error 'inflate-zlib "adler-32 mismatch")]))

(define (inflate-limited in limit invalid)
  (define written 0)
  (define chunks '())
  (define out
    (make-output-port 'inflate-zlib always-evt
                      (lambda (buffer start end _non-blocking? _breakable?)
                        (set! written (+ written (- end start)))
                        (when (> written limit)
                          (error 'inflate-zlib "inflated data exceeds the limit\n  limit: ~a" limit))
                        (set! chunks (cons (subbytes buffer start end) chunks))
                        (- end start))
                      void))
  (with-handlers ([(lambda (e) (and (exn:fail? e) (<= written limit))) invalid])
    (inflate in out))
  (apply bytes-append (reverse chunks)))

(define adler-block 5552)

(define (adler-32 data)
  (define size (bytes-length data))
  (for/fold ([a 1] [b 0] #:result (+ (* b 65536) a))
            ([start (in-range 0 size adler-block)])
    (for/fold ([a a] [b b] #:result (values (modulo a 65521) (modulo b 65521)))
              ([byte (in-bytes data start (min size (+ start adler-block)))])
      (values (+ a byte) (+ b a byte)))))
