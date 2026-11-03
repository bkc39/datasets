#lang racket/base
(require (only-in datasets/fetch-private/archive inflate-zlib)
         (only-in racket/contract/base ->i -> </c and/c any/c contract-out flat-named-contract listof
                  non-empty-listof or/c)
         (only-in racket/match match match-define)
         (only-in racket/port call-with-output-bytes copy-port make-limited-input-port
                  open-output-nowhere)
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
          [mat-array-ref (->i ([a mat-array?]
                               [i (a) (and/c exact-nonnegative-integer? (</c (apply * (mat-array-dims a))))])
                              [_ (or/c exact-integer? flonum?)])]))

(struct mat-array (class dims kind size data big-endian?))

(struct storage (name size kind))

(define storage-types
  (hash 1 (storage 'miINT8 1 'signed) 2 (storage 'miUINT8 1 'unsigned)
        3 (storage 'miINT16 2 'signed) 4 (storage 'miUINT16 2 'unsigned)
        5 (storage 'miINT32 4 'signed) 6 (storage 'miUINT32 4 'unsigned)
        7 (storage 'miSINGLE 4 'float) 9 (storage 'miDOUBLE 8 'float)
        12 (storage 'miINT64 8 'signed) 13 (storage 'miUINT64 8 'unsigned)))

(define array-classes
  (hash 1 'cell 2 'struct 3 'object 4 'char 5 'sparse 6 'double 7 'single 8 'int8 9 'uint8 10 'int16
        11 'uint16 12 'int32 13 'uint32 14 'int64 15 'uint64 16 'function 17 'opaque))

(define mi-matrix 14)
(define mi-compressed 15)
(define compressed-limit (+ (expt 2 32) 7))
(define header-size 128)

(define (read-mat in names)
  (define big-endian? (read-header in))
  (define variables (read-variables in big-endian? names))
  (for/list ([name (in-list names)])
    (match (assoc name variables)
      [(cons _ array) array]
      [#f (error 'read-mat "no such variable\n  name: ~a\n  variables: ~a" name (map car variables))])))

(define (read-header in)
  (define header (read-bytes header-size in))
  (define complete? (and (bytes? header) (= (bytes-length header) header-size)))
  (define indicator (and complete? (subbytes header 126 128)))
  (define big-endian? (equal? indicator #"MI"))
  (define version (and complete? (integer-bytes->integer header #f big-endian? 124 126)))
  (unless (and complete? (regexp-match? #rx#"^MATLAB " header) (member indicator '(#"IM" #"MI")))
    (error 'read-mat "not a MAT-file version 5"))
  (unless (= version #x0100)
    (error 'read-mat "unsupported MAT-file version\n  version: 0x~a" (pad-hex version)))
  (unless (regexp-match? #rx#"^MATLAB 5.0 MAT-file" header)
    (error 'read-mat "not a MAT-file version 5"))
  big-endian?)

(define (pad-hex n)
  (define digits (number->string n 16))
  (string-append (make-string (max 0 (- 4 (string-length digits))) #\0) digits))

(define (read-variables in big-endian? names)
  (let loop ([found '()])
    (define offset (file-position in))
    (define tag (read-bytes 8 in))
    (cond
      [(eof-object? tag) (reverse found)]
      [(< (bytes-length tag) 8) (overrun offset)]
      [else
       (define-values (type size _small?) (tag-type+size tag big-endian?))
       (define variable (read-top-element in type size offset big-endian? names))
       (loop (add-variable found variable))])))

(define (add-variable found variable)
  (match variable
    [#f found]
    [(cons name _)
     (when (assoc name found)
       (error 'read-mat "duplicate variable\n  name: ~a" name))
     (cons variable found)]))

(define (read-top-element in type size offset big-endian? names)
  (cond
    [(= type mi-matrix)
     (define start (file-position in))
     (define body (make-limited-input-port in size #f))
     (begin0 (read-matrix body offset big-endian? names)
             (drain in body start size offset))]
    [(= type mi-compressed)
     (define inner (~> (read-exactly in size offset) (inflate-zlib compressed-limit) open-input-bytes))
     (define tag (read-exactly inner 8 offset))
     (define-values (inner-type inner-size _small?) (tag-type+size tag big-endian?))
     (read-top-element inner inner-type inner-size offset big-endian? names)]
    [else (drain in (make-limited-input-port in size #f) (file-position in) size offset) #f]))

(define (tag-type+size tag big-endian?)
  (define word (integer-bytes->integer tag #f big-endian? 0 4))
  (define small-size (arithmetic-shift word -16))
  (cond
    [(zero? small-size) (values word (integer-bytes->integer tag #f big-endian? 4 8) #f)]
    [else (values (bitwise-and word #xffff) small-size #t)]))

(define (read-subelement in offset big-endian?)
  (define tag (read-exactly in 8 offset))
  (define-values (type size small?) (tag-type+size tag big-endian?))
  (cond
    [small? (values type size (small-data tag size offset))]
    [else (values type size (begin0 (read-exactly in size offset)
                                    (read-bytes (modulo (- size) 8) in)))]))

(define (read-subelement-data in offset big-endian?)
  (define-values (_type _size data) (read-subelement in offset big-endian?))
  data)

(define (read-matrix body offset big-endian? names)
  (define flags (read-subelement-data body offset big-endian?))
  (define dims-data (read-subelement-data body offset big-endian?))
  (define name (bytes->string/latin-1 (read-subelement-data body offset big-endian?)))
  (cond
    [(member name names) (cons name (read-numeric body name flags dims-data offset big-endian?))]
    [else (cons name #f)]))

(define (read-numeric body name flags dims-data offset big-endian?)
  (define word (if (>= (bytes-length flags) 4) (integer-bytes->integer flags #f big-endian? 0 4) 0))
  (define class-code (bitwise-and word #xff))
  (define class (hash-ref array-classes class-code class-code))
  (define dims (for/list ([start (in-range 0 (- (bytes-length dims-data) 3) 4)])
                 (integer-bytes->integer dims-data #t big-endian? start (+ start 4))))
  (unless (memq class '(double single int8 uint8 int16 uint16 int32 uint32 int64 uint64))
    (error 'read-mat "unsupported array class\n  name: ~a\n  class: ~a" name class))
  (when (bitwise-bit-set? word 11)
    (error 'read-mat "complex arrays are not supported\n  name: ~a" name))
  (read-real-part body name class dims offset big-endian?))

(define (read-real-part body name class dims offset big-endian?)
  (define tag (read-exactly body 8 offset))
  (define-values (type size small?) (tag-type+size tag big-endian?))
  (define store (hash-ref storage-types type #f))
  (define expected (and store (andmap exact-nonnegative-integer? dims)
                        (* (apply * dims) (storage-size store))))
  (unless store
    (error 'read-mat "unsupported storage type\n  name: ~a\n  type: ~a" name type))
  (unless (eqv? expected size)
    (error 'read-mat
           "data size does not match the dimensions\n  name: ~a\n  dimensions: ~a\n  bytes: ~a"
           name dims size))
  (define data (if small? (small-data tag size offset) (read-exactly body size offset)))
  (mat-array class dims (storage-kind store) (storage-size store) data big-endian?))

(define (small-data tag size offset)
  (cond
    [(> size 4) (overrun offset)]
    [else (subbytes tag 4 (+ 4 size))]))

(define (read-exactly in n offset)
  (define data
    (call-with-output-bytes (lambda (out) (copy-port (make-limited-input-port in n #f) out))))
  (cond
    [(= (bytes-length data) n) data]
    [else (overrun offset)]))

(define (drain in body start size offset)
  (copy-port body (open-output-nowhere))
  (unless (= (- (file-position in) start) size)
    (overrun offset)))

(define (overrun offset)
  (error 'read-mat "element overruns its container\n  offset: ~a" offset))

(define (mat-array-ref a i)
  (match-define (mat-array _ _ kind size data big-endian?) a)
  (define start (* i size))
  (match kind
    ['float (floating-point-bytes->real data big-endian? start (+ start size))]
    ['signed (integer-bytes->integer data #t big-endian? start (+ start size))]
    ['unsigned (integer-bytes->integer data #f big-endian? start (+ start size))]))
