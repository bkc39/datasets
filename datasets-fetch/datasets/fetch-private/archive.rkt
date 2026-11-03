#lang racket/base
(require (only-in file/gunzip gunzip-through-ports inflate)
         (only-in file/untar untar)
         (only-in racket/contract/base -> ->* contract-out)
         (only-in racket/port call-with-output-bytes)
         (only-in threading ~>))
(provide (contract-out
          [gunzip-file (-> path-string? bytes?)]
          [tar-gz-member (-> path-string? string? bytes?)]
          [inflate-zlib (-> bytes? exact-nonnegative-integer? bytes?)]
          [bytes->input-port (->* (bytes?) (exact-nonnegative-integer?) input-port?)]))

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
  (define in (bytes->input-port data (min size 2)))
  (define-values (out output) (collecting-output-port limit))
  (define outcome
    (and zlib? (not dictionary?)
         (let/ec escape
           (with-handlers ([exn:fail? exn-message])
             (inflate in (out (lambda () (escape 'over))))
             'done))))
  (define trailer (- size 2 (file-position in)))
  (define inflated (and (eq? outcome 'done) (output)))
  (cond
    [(not cmf) (error 'inflate-zlib "truncated zlib stream")]
    [(not zlib?) (error 'inflate-zlib "not a zlib stream")]
    [dictionary? (error 'inflate-zlib "preset dictionary not supported")]
    [(eq? outcome 'over) (error 'inflate-zlib "inflated data exceeds the limit\n  limit: ~a" limit)]
    [(< trailer 4) (error 'inflate-zlib "truncated zlib stream")]
    [(string? outcome) (error 'inflate-zlib "invalid deflate data\n  detail: ~a" outcome)]
    [(> trailer 4) (error 'inflate-zlib "trailing data after the zlib stream")]
    [(= (adler-32 inflated) (integer-bytes->integer data #f #t (- size 4) size)) inflated]
    [else (error 'inflate-zlib "adler-32 mismatch")]))

(define (bytes->input-port data [start 0])
  (define size (bytes-length data))
  (define position start)
  (define (copy! buffer skip advance?)
    (define from (min size (+ position skip)))
    (define n (min (bytes-length buffer) (- size from)))
    (bytes-copy! buffer 0 data from (+ from n))
    (cond
      [(zero? n) eof]
      [else (set! position (+ position (if advance? n 0))) n]))
  (make-input-port 'bytes
                   (lambda (buffer) (copy! buffer 0 #t))
                   (lambda (buffer skip _progress) (copy! buffer skip #f))
                   void))

(define (collecting-output-port limit)
  (define written 0)
  (define chunks '())
  (values (lambda (over)
            (make-output-port 'inflate-zlib always-evt
                              (lambda (buffer start end _non-blocking? _breakable?)
                                (set! written (+ written (- end start)))
                                (cond
                                  [(> written limit) (over)]
                                  [else (set! chunks (cons (subbytes buffer start end) chunks))
                                        (- end start)]))
                              void))
          (lambda () (apply bytes-append (reverse chunks)))))

(define adler-block 5552)

(define (adler-32 data)
  (define size (bytes-length data))
  (for/fold ([a 1] [b 0] #:result (+ (* b 65536) a))
            ([start (in-range 0 size adler-block)])
    (for/fold ([a a] [b b] #:result (values (modulo a 65521) (modulo b 65521)))
              ([byte (in-bytes data start (min size (+ start adler-block)))])
      (values (+ a byte) (+ b a byte)))))
