#lang racket/base
(require (only-in datasets/core-private/resource with-custodian)
         (only-in datasets/fetch-private/bytes-port bytes->input-port)
         (only-in file/gunzip gunzip-through-ports inflate)
         (only-in file/untar untar)
         (only-in file/unzip read-zip-directory unzip-entry zip-directory-entries)
         (only-in racket/contract/base -> contract-out listof)
         (only-in racket/match match)
         (only-in racket/port call-with-output-bytes copy-port open-output-nowhere))
(provide (contract-out
          [gunzip-file (-> path-string? bytes?)]
          [gunzip-bytes (-> bytes? bytes?)]
          [tar-gz-member (-> path-string? string? bytes?)]
          [tar-gz-members (-> path-string? (listof string?) (listof bytes?))]
          [zip-members (-> path-string? (listof string?) (listof bytes?))]
          [inflate-zlib (-> bytes? exact-nonnegative-integer? bytes?)]))

(define pipe-bytes (* 1024 1024))

(define (gunzip-file path)
  (call-with-input-file path
    (lambda (in) (call-with-output-bytes (lambda (out) (gunzip-through-ports in out))))))

(define (gunzip-bytes data)
  (call-with-output-bytes (lambda (out) (gunzip-through-ports (open-input-bytes data) out))))

(define (call-with-gunzip-port path proc)
  (with-custodian
    (define-values (in out) (make-pipe pipe-bytes))
    (define failure (box #f))
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (set-box! failure e))])
                (call-with-input-file path (lambda (file) (gunzip-through-ports file out))))
              (close-output-port out)))
    (define (check-gunzip!)
      (copy-port in (open-output-nowhere))
      (cond
        [(unbox failure) => raise]
        [else (void)]))
    (proc in check-gunzip!)))

(define (tar-gz-member archive member)
  (match (read-tar-gz-members 'tar-gz-member archive (list member))
    [(list data) data]))

(define (tar-gz-members archive names)
  (cond
    [(null? names) '()]
    [else (read-tar-gz-members 'tar-gz-members archive names)]))

(define (read-tar-gz-members who archive names)
  (define wanted (for/hash ([name (in-list names)]) (values name #t)))
  (call-with-gunzip-port
   archive
   (lambda (in check-gunzip!)
     (define found (make-hash))
     (define (member-error message name)
       (check-gunzip!)
       (error who "~a\n  archive: ~a\n  member: ~a" message archive name))
     (define (untar-failure e)
       (define truncated? (eof-object? (peek-byte in)))
       (check-gunzip!)
       (cond
         [truncated?
          (member-error "truncated archive"
                        (for/first ([name (in-list names)] #:unless (hash-has-key? found name)) name))]
         [else (raise e)]))
     (define short-member
       (let/ec stop
         (with-handlers ([exn:fail? untar-failure])
           (untar in
                  #:filter (lambda (name _stripped type _size _link _time _mode)
                             (define key (path->string name))
                             (and (eq? type 'file) (hash-has-key? wanted key) (not (hash-has-key? found key))))
                  #:handle-entry (lambda (_kind path in size _attribs)
                                   (define key (path->string path))
                                   (define data (read-bytes size in))
                                   (cond
                                     [(not (and (bytes? data) (= (bytes-length data) size))) (stop key)]
                                     [else (hash-set! found key data)
                                           (if (= (hash-count found) (hash-count wanted)) (stop #f) '())]))))
         #f))
     (cond
       [short-member (member-error "truncated member" short-member)]
       [else (for/list ([name (in-list names)])
               (hash-ref found name (lambda () (member-error "no such member" name))))]))))

(define (zip-members archive names)
  (define directory (read-zip-directory archive))
  (define entries (zip-directory-entries directory))
  (define keys (map string->bytes/utf-8 names))
  (match (for/first ([name (in-list names)] [key (in-list keys)] #:unless (member key entries)) name)
    [(? string? name) (error 'zip-members "no such member\n  archive: ~a\n  member: ~a" archive name)]
    [#f
     (for/list ([key (in-list keys)])
       (call-with-output-bytes
        (lambda (out)
          (unzip-entry archive directory key (lambda (_name _dir? in) (copy-port in out))))))]))

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
