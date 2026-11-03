#lang racket/base
(require (only-in datasets/fetch-private/archive bytes->input-port inflate-zlib)
         (only-in racket/contract/base ->i -> any/c contract-out flat-named-contract listof non-empty-listof
                  or/c)
         (only-in racket/match match match-define)
         (only-in racket/port copy-port make-limited-input-port open-output-nowhere)
         (only-in threading ~>))
(define mat-name/c
  (flat-named-contract 'mat-name
                       (lambda (v) (and (string? v) (regexp-match? #px"^[A-Za-z][A-Za-z0-9_]{0,62}$" v)))))
(define mat-class/c
  (flat-named-contract 'mat-class
                       (or/c 'double 'single 'int8 'uint8 'int16 'uint16 'int32 'uint32 'int64 'uint64)))
(provide mat-class/c
         (contract-out
          [read-mat (-> input-port? (non-empty-listof mat-name/c) (listof mat-array?))]
          [mat-array? (-> any/c boolean?)]
          [mat-array-class (-> mat-array? mat-class/c)]
          [mat-array-dims (-> mat-array? (non-empty-listof exact-nonnegative-integer?))]
          [mat-array-ref (->i ([a mat-array?] [i exact-nonnegative-integer?])
                              #:pre/name (a i) "index within the array" (< i (mat-array-count a))
                              [_ (or/c exact-integer? flonum?)])]
          [mat-array->bytes (-> byte-storage/c bytes?)]))

(struct mat-array (class dims count kind width data big-endian?))

(struct storage (name width kind))

(struct frame (offset big-endian? memory-left))

(define byte-storage/c
  (flat-named-contract 'byte-storage (lambda (a) (and (mat-array? a) (= (mat-array-width a) 1)))))

(define storage-types
  (hash 1 (storage 'miINT8 1 'signed) 2 (storage 'miUINT8 1 'unsigned)
        3 (storage 'miINT16 2 'signed) 4 (storage 'miUINT16 2 'unsigned)
        5 (storage 'miINT32 4 'signed) 6 (storage 'miUINT32 4 'unsigned)
        7 (storage 'miSINGLE 4 'float) 9 (storage 'miDOUBLE 8 'float)
        12 (storage 'miINT64 8 'signed) 13 (storage 'miUINT64 8 'unsigned)))

(define array-classes
  (hash 1 'cell 2 'struct 3 'object 4 'char 5 'sparse 6 'double 7 'single 8 'int8 9 'uint8 10 'int16
        11 'uint16 12 'int32 13 'uint32 14 'int64 15 'uint64 16 'function 17 'opaque))

(define numeric-classes '(double single int8 uint8 int16 uint16 int32 uint32 int64 uint64))
(define mi-matrix 14)
(define mi-compressed 15)
(define compressed-limit (+ (expt 2 32) 7))
(define header-size 128)
(define chunk-size 65536)

(define (read-mat in names)
  (define big-endian? (read-header in))
  (define-values (seen found) (read-variables in big-endian? names))
  (for/list ([name (in-list names)])
    (match (assoc name found)
      [(cons _ array) array]
      [#f (error 'read-mat "no such variable\n  name: ~a\n  variables: ~a" name seen)])))

(define (read-header in)
  (define header (read-bytes header-size in))
  (define complete? (and (bytes? header) (= (bytes-length header) header-size)))
  (define indicator (and complete? (subbytes header 126 128)))
  (define big-endian? (equal? indicator #"MI"))
  (define version (and complete? (integer-bytes->integer header #f big-endian? 124 126)))
  (cond
    [(not (and complete? (regexp-match? #rx#"^MATLAB " header) (member indicator '(#"IM" #"MI"))))
     (error 'read-mat "not a MAT-file version 5")]
    [(not (= version #x0100))
     (error 'read-mat "unsupported MAT-file version\n  version: 0x~a" (pad-hex version))]
    [(regexp-match? #rx#"^MATLAB 5.0 MAT-file" header) big-endian?]
    [else (error 'read-mat "not a MAT-file version 5")]))

(define (pad-hex n)
  (define digits (number->string n 16))
  (string-append (make-string (max 0 (- 4 (string-length digits))) #\0) digits))

(define (read-variables in big-endian? names)
  (let loop ([seen '()] [found '()])
    (define offset (file-position in))
    (define tag (read-bytes 8 in))
    (define complete? (and (bytes? tag) (= (bytes-length tag) 8)))
    (define-values (name array)
      (cond
        [complete?
         (define-values (type size _small?) (tag-type+size tag big-endian?))
         (read-top-element in type size (frame offset big-endian? #f) names)]
        [else (values #f #f)]))
    (cond
      [(eof-object? tag) (values (reverse seen) found)]
      [(not complete?) (overrun offset)]
      [(not name) (loop seen found)]
      [(member name seen) (error 'read-mat "duplicate variable\n  name: ~a" name)]
      [array (loop (cons name seen) (cons (cons name array) found))]
      [else (loop (cons name seen) found)])))

(define (read-top-element in type size context names)
  (cond
    [(= type mi-matrix)
     (define start (file-position in))
     (define body (make-limited-input-port in size #f))
     (define-values (name array) (read-matrix body context names))
     (drain in body start size context)
     (values name array)]
    [(= type mi-compressed)
     (define inflated (~> (read-exactly in size context) (inflate-zlib compressed-limit)))
     (define inner (bytes->input-port inflated))
     (define inner-context
       (frame (frame-offset context) (frame-big-endian? context)
              (lambda () (- (bytes-length inflated) (file-position inner)))))
     (define tag (read-exactly inner 8 inner-context))
     (define-values (inner-type inner-size _small?) (tag-type+size tag (frame-big-endian? context)))
     (read-top-element inner inner-type inner-size inner-context names)]
    [else (drain in (make-limited-input-port in size #f) (file-position in) size context)
          (values #f #f)]))

(define (tag-type+size tag big-endian?)
  (define word (integer-bytes->integer tag #f big-endian? 0 4))
  (define small-size (arithmetic-shift word -16))
  (cond
    [(zero? small-size) (values word (integer-bytes->integer tag #f big-endian? 4 8) #f)]
    [else (values (bitwise-and word #xffff) small-size #t)]))

(define (read-subelement in context)
  (define tag (read-exactly in 8 context))
  (define-values (type size small?) (tag-type+size tag (frame-big-endian? context)))
  (cond
    [small? (values type size (small-data tag size context))]
    [else (values type size (begin0 (read-exactly in size context)
                                    (read-bytes (modulo (- size) 8) in)))]))

(define (read-subelement-data in context)
  (define-values (_type _size data) (read-subelement in context))
  data)

(define (read-matrix body context names)
  (define flags (read-subelement-data body context))
  (define dims-data (read-subelement-data body context))
  (define name (bytes->string/latin-1 (read-subelement-data body context)))
  (values name (and (member name names) (read-numeric body name flags dims-data context))))

(define (read-numeric body name flags dims-data context)
  (define big-endian? (frame-big-endian? context))
  (define word (if (>= (bytes-length flags) 4) (integer-bytes->integer flags #f big-endian? 0 4) 0))
  (define class-code (bitwise-and word #xff))
  (define class (hash-ref array-classes class-code class-code))
  (define dims-size (bytes-length dims-data))
  (define dims (for/list ([start (in-range 0 (- dims-size 3) 4)])
                 (integer-bytes->integer dims-data #t big-endian? start (+ start 4))))
  (cond
    [(not (memq class numeric-classes))
     (error 'read-mat "unsupported array class\n  name: ~a\n  class: ~a" name class)]
    [(bitwise-bit-set? word 11) (error 'read-mat "complex arrays are not supported\n  name: ~a" name)]
    [(and (>= dims-size 8) (zero? (modulo dims-size 4))) (read-real-part body name class dims context)]
    [else (error 'read-mat "malformed dimensions\n  name: ~a\n  bytes: ~a" name dims-size)]))

(define (read-real-part body name class dims context)
  (define tag (read-exactly body 8 context))
  (define-values (type size small?) (tag-type+size tag (frame-big-endian? context)))
  (define store (hash-ref storage-types type #f))
  (define count (and (andmap exact-nonnegative-integer? dims) (apply * dims)))
  (cond
    [(not store) (error 'read-mat "unsupported storage type\n  name: ~a\n  type: ~a" name type)]
    [(and count (= (* count (storage-width store)) size))
     (mat-array class dims count (storage-kind store) (storage-width store)
                (if small? (small-data tag size context) (read-exactly body size context))
                (frame-big-endian? context))]
    [else (error 'read-mat "data size does not match the dimensions\n  name: ~a\n  dimensions: ~a\n  bytes: ~a"
                 name dims size)]))

(define (small-data tag size context)
  (cond
    [(> size 4) (overrun (frame-offset context))]
    [else (subbytes tag 4 (+ 4 size))]))

(define (read-exactly in n context)
  (define left (frame-memory-left context))
  (define data
    (cond
      [(not left) (read-chunked in n)]
      [(<= n (left)) (read-bytes n in)]
      [else #f]))
  (cond
    [(and (bytes? data) (= (bytes-length data) n)) data]
    [else (overrun (frame-offset context))]))

(define (read-chunked in n)
  (let loop ([left n] [chunks '()])
    (define chunk (if (zero? left) eof (read-bytes (min left chunk-size) in)))
    (cond
      [(eof-object? chunk) (apply bytes-append (reverse chunks))]
      [else (loop (- left (bytes-length chunk)) (cons chunk chunks))])))

(define (drain in body start size context)
  (copy-port body (open-output-nowhere))
  (unless (= (- (file-position in) start) size)
    (overrun (frame-offset context))))

(define (overrun offset)
  (error 'read-mat "element overruns its container\n  offset: ~a" offset))

(define (mat-array-ref a i)
  (match-define (mat-array _ _ _ kind width data big-endian?) a)
  (define start (* i width))
  (match kind
    ['float (floating-point-bytes->real data big-endian? start (+ start width))]
    ['signed (integer-bytes->integer data #t big-endian? start (+ start width))]
    ['unsigned (integer-bytes->integer data #f big-endian? start (+ start width))]))

(define (mat-array->bytes a)
  (mat-array-data a))
