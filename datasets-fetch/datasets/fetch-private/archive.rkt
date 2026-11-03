#lang racket/base
(require (only-in file/gunzip gunzip-through-ports inflate)
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
  (define flg (and (>= size 2) (bytes-ref data 1)))
  (unless cmf
    (error 'inflate-zlib "truncated zlib stream"))
  (when (or (not (= (bitwise-and cmf 15) 8))
            (> (arithmetic-shift cmf -4) 7)
            (not (zero? (modulo (+ (* cmf 256) flg) 31))))
    (error 'inflate-zlib "not a zlib stream"))
  (when (bitwise-bit-set? flg 5)
    (error 'inflate-zlib "preset dictionary not supported"))
  (inflate-zlib-body data limit))

(define (inflate-zlib-body data limit)
  (define in (open-input-bytes data))
  (void (read-bytes 2 in))
  (define eof-seen? (box #f))
  (define out (open-output-bytes))
  (define outcome
    (let/ec escape
      (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
        (inflate (eof-watching-port in eof-seen?) (limited-output-port out limit (lambda () (escape 'over))))
        'done)))
  (define trailer (- (bytes-length data) (file-position in)))
  (when (eq? outcome 'over)
    (error 'inflate-zlib "inflated data exceeds the limit\n  limit: ~a" limit))
  (when (or (unbox eof-seen?) (< trailer 4))
    (error 'inflate-zlib "truncated zlib stream"))
  (when (string? outcome)
    (error 'inflate-zlib "invalid deflate data\n  detail: ~a" outcome))
  (when (> trailer 4)
    (error 'inflate-zlib "trailing data after the zlib stream"))
  (checked-output (get-output-bytes out #t) (integer-bytes->integer (read-bytes 4 in) #f #t)))

(define (checked-output output expected)
  (cond
    [(= (adler-32 output) expected) output]
    [else (error 'inflate-zlib "adler-32 mismatch")]))

(define (eof-watching-port in eof-seen?)
  (define (noted result)
    (when (eof-object? result) (set-box! eof-seen? #t))
    result)
  (make-input-port 'inflate-zlib
                   (lambda (buffer) (noted (read-bytes-avail!* buffer in)))
                   (lambda (buffer skip _progress) (noted (peek-bytes-avail!* buffer skip #f in)))
                   void))

(define (limited-output-port out limit over)
  (define written 0)
  (make-output-port 'inflate-zlib always-evt
                    (lambda (buffer start end _non-blocking? _breakable?)
                      (set! written (+ written (- end start)))
                      (cond
                        [(> written limit) (over)]
                        [else (write-bytes buffer out start end)]))
                    void))

(define adler-block 5552)

(define (adler-32 data)
  (define size (bytes-length data))
  (for/fold ([a 1] [b 0] #:result (+ (* b 65536) a))
            ([start (in-range 0 size adler-block)])
    (for/fold ([a a] [b b] #:result (values (modulo a 65521) (modulo b 65521)))
              ([byte (in-bytes data start (min size (+ start adler-block)))])
      (values (+ a byte) (+ b a byte)))))
