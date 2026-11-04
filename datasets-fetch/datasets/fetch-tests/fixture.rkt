#lang racket/base
(require (only-in datasets/core-tests/fixture call-with-temporary-fixture gzip-bytes)
         (only-in datasets/fetch-private/mat mat-class/c)
         (only-in file/gzip deflate)
         (only-in file/tar tar->output)
         (only-in file/zip zip->output zip-entry)
         (only-in racket/contract/base -> ->* any any/c contract-out hash/c integer-in list/c listof
                  non-empty-listof or/c)
         (only-in racket/file file->bytes make-directory*)
         (only-in racket/list append* append-map first second)
         (only-in racket/match match match-define match-lambda)
         (only-in racket/path path-only)
         (only-in racket/port call-with-output-bytes open-output-nowhere)
         (only-in racket/string string-contains? string-prefix?)
         (only-in threading ~>))
(provide (contract-out
          [call-with-upstream-files (-> (listof (list/c string? upstream/c))
                                        (-> (hash/c string? complete-path? #:immutable #t) any)
                                        any)]
          [cifar-10-member-names (listof string?)]
          [cifar-10-member-splits (listof string?)]
          [cifar-10-upstream (-> (listof member/c) list?)]
          [cifar-100-member-names (listof string?)]
          [cifar-100-upstream (-> (listof member/c) list?)]
          [cifar-record-bytes (-> (listof (listof byte?)) bytes?)]
          [emnist-members (-> string? (list/c idx/c idx/c idx/c idx/c) (listof (list/c string? bytes?)))]
          [error-with? (-> symbol? string? ... (-> any/c boolean?))]
          [idx-bytes (->* ((listof exact-nonnegative-integer?) bytes?) (#:magic bytes?) bytes?)]
          [mat-bytes (->* ((listof (list/c string? mat-class/c (non-empty-listof exact-nonnegative-integer?)
                                           (or/c 'miUINT8 'miDOUBLE 'miINT16) (or/c (listof real?) bytes?))))
                          (#:endianness (or/c 'little 'big) #:compressed? boolean?) bytes?)]
          [png-bytes (->* (exact-positive-integer? exact-positive-integer? bytes?)
                          (#:filters (listof (integer-in 0 4)) #:idat-chunks exact-positive-integer?) bytes?)]
          [png-chunk (-> string? bytes? bytes?)]
          [tar-bytes (-> (listof member/c) bytes?)]
          [zlib-bytes (-> bytes? bytes?)]))

(define member/c (list/c string? (or/c string? bytes?)))
(define idx/c (list/c (listof exact-nonnegative-integer?) bytes?))

(define upstream/c
  (or/c (list/c 'gz string?) (list/c 'tgz (listof (list/c string? (or/c string? bytes?)))) (list/c 'plain string?)
        (list/c 'idx-gz (non-empty-listof exact-nonnegative-integer?) bytes?)
        (list/c 'zip (listof (list/c string? bytes?))) (list/c 'bytes bytes?)))

(define (upstream-bytes upstream)
  (match upstream
    [(list 'plain text) (string->bytes/utf-8 text)]
    [(list 'gz text) (gzip-bytes (string->bytes/utf-8 text))]
    [(list 'tgz members) (gzip-bytes (tar-bytes members))]
    [(list 'idx-gz dims data) (gzip-bytes (idx-bytes dims data))]
    [(list 'zip members) (zip-bytes members)]
    [(list 'bytes data) data]))

(define (idx-bytes dims data #:magic [magic (bytes 0 0 8 (length dims))])
  (apply bytes-append magic (append (map (lambda (d) (integer->integer-bytes d 4 #f #t)) dims) (list data))))

(define ((error-with? who . needles) e)
  (and (exn:fail? e) (not (exn:fail:contract? e))
       (string-prefix? (exn-message e) (format "~a: " who))
       (for/and ([needle (in-list needles)]) (string-contains? (exn-message e) needle))))

(define (tar-bytes members)
  (call-with-temporary-fixture
   (lambda (dir)
     (for ([member (in-list members)])
       (write-member! dir member))
     (parameterize ([current-directory dir])
       (call-with-output-bytes
        (lambda (out)
          (tar->output (map (match-lambda [(list name _) (string->path name)]) members) out
                       #:get-timestamp (lambda (_) 0))))))))

(define (write-member! dir member)
  (match-define (list name content) member)
  (define file (build-path dir name))
  (make-directory* (path-only file))
  (call-with-output-file file
    (lambda (out) (if (bytes? content) (write-bytes content out) (write-string content out)))))

(define stored-level 0)
(define deflated-level 6)

(define (deflated-size data)
  (define-values (_in out _crc) (deflate (open-input-bytes data) (open-output-nowhere)))
  out)

(define (zip-bytes members)
  (call-with-temporary-fixture
   (lambda (dir)
     (define file (build-path dir "members.zip"))
     (call-with-output-file file
       (lambda (out)
         (zip->output (for/list ([member (in-list members)])
                        (match-define (list name data) member)
                        (zip-entry 'file (string->path name) data (bytes-length data) (hash)
                                   (if (< (deflated-size data) (bytes-length data)) deflated-level stored-level)))
                      out
                      #:get-timestamp (lambda (_) 0))))
     (file->bytes file))))

(define (call-with-upstream-files upstream proc)
  (call-with-temporary-fixture
   (lambda (dir)
     (proc (for/hash ([file (in-list upstream)])
             (match file
               [(list name content)
                (define path (build-path dir name))
                (call-with-output-file path (lambda (out) (write-bytes (upstream-bytes content) out)))
                (values name path)]))))))

(define (u32 n big-endian?) (integer->integer-bytes n 4 #f big-endian?))

(define (adler-32 data)
  (for/fold ([a 1] [b 0] #:result (+ (* b 65536) a)) ([byte (in-bytes data)])
    (values (modulo (+ a byte) 65521) (modulo (+ b a byte) 65521))))

(define (zlib-bytes data)
  (define deflated (call-with-output-bytes (lambda (out) (deflate (open-input-bytes data) out))))
  (bytes-append (bytes #x78 #x9c) deflated (u32 (adler-32 data) #t)))

(define mat-storage-codes (hash 'miINT8 1 'miUINT8 2 'miINT16 3 'miINT32 5 'miUINT32 6 'miDOUBLE 9))
(define mat-class-codes (hash 'double 6 'single 7 'int8 8 'uint8 9 'int16 10 'uint16 11 'int32 12
                              'uint32 13 'int64 14 'uint64 15))

(define (mat-bytes variables #:endianness [endianness 'little] #:compressed? [compressed? #f])
  (define big-endian? (eq? endianness 'big))
  (define text #"MATLAB 5.0 MAT-file, Platform: racket fixture")
  (apply bytes-append text (make-bytes (- 116 (bytes-length text)) 32) (make-bytes 8 0)
         (integer->integer-bytes #x0100 2 #f big-endian?) (if big-endian? #"MI" #"IM")
         (for/list ([variable (in-list variables)])
           (define matrix (mat-matrix variable big-endian?))
           (cond
             [compressed? (mat-element 15 (zlib-bytes matrix) big-endian? #:pad? #f)]
             [else matrix]))))

(define (mat-matrix variable big-endian?)
  (match-define (list name class dims storage elements) variable)
  (mat-element 14 (bytes-append
                   (mat-element 6 (bytes-append (u32 (hash-ref mat-class-codes class) big-endian?)
                                                (u32 0 big-endian?))
                                big-endian?)
                   (mat-element 5 (apply bytes-append (for/list ([d (in-list dims)])
                                                        (integer->integer-bytes d 4 #t big-endian?)))
                                big-endian?)
                   (mat-element 1 (string->bytes/latin-1 name) big-endian?)
                   (mat-element (hash-ref mat-storage-codes storage) (mat-values storage elements big-endian?)
                                big-endian?))
               big-endian?))

(define (mat-values storage elements big-endian?)
  (cond
    [(bytes? elements) elements]
    [else
     (apply bytes-append
            (for/list ([v (in-list elements)])
              (match storage
                ['miUINT8 (bytes v)]
                ['miINT16 (integer->integer-bytes v 2 #t big-endian?)]
                ['miDOUBLE (real->floating-point-bytes v 8 big-endian?)])))]))

(define (mat-element type data big-endian? #:pad? [pad? #t])
  (define size (bytes-length data))
  (cond
    [(and pad? (<= 1 size 4) (not (= type 14)))
     (bytes-append (u32 (+ (* size 65536) type) big-endian?) data (make-bytes (- 4 size) 0))]
    [else (bytes-append (u32 type big-endian?) (u32 size big-endian?) data
                        (make-bytes (if pad? (modulo (- size) 8) 0) 0))]))

(define (crc-32 data)
  (bitwise-xor #xffffffff
               (for/fold ([crc #xffffffff]) ([byte (in-bytes data)])
                 (for/fold ([crc (bitwise-xor crc byte)]) ([_ (in-range 8)])
                   (define shifted (arithmetic-shift crc -1))
                   (if (odd? crc) (bitwise-xor shifted #xedb88320) shifted)))))

(define (png-chunk type data)
  (define typed (bytes-append (string->bytes/latin-1 type) data))
  (bytes-append (u32 (bytes-length data) #t) typed (u32 (crc-32 typed) #t)))

(define (png-bytes width height pixels #:filters [filters '(0)] #:idat-chunks [idat-chunks 1])
  (define (pixel x y) (if (and (>= x 0) (>= y 0)) (bytes-ref pixels (+ (* y width) x)) 0))
  (define raw
    (apply bytes-append
           (for/list ([y (in-range height)])
             (define filter-type (list-ref filters (modulo y (length filters))))
             (apply bytes filter-type
                    (for/list ([x (in-range width)])
                      (define predicted (png-predict filter-type (pixel (- x 1) y) (pixel x (- y 1))
                                                     (pixel (- x 1) (- y 1))))
                      (modulo (- (pixel x y) predicted) 256))))))
  (define compressed (zlib-bytes raw))
  (define size (bytes-length compressed))
  (define piece (quotient (+ size idat-chunks -1) idat-chunks))
  (apply bytes-append #"\211PNG\r\n\32\n"
         (png-chunk "IHDR" (bytes-append (u32 width #t) (u32 height #t) (bytes 8 0 0 0 0)))
         (append (for/list ([i (in-range idat-chunks)])
                   (~> compressed
                       (subbytes (min size (* i piece)) (min size (* (add1 i) piece)))
                       (png-chunk "IDAT" _)))
                 (list (png-chunk "IEND" #"")))))

(define (png-predict filter-type a b c)
  (define p (- (+ a b) c))
  (define pa (abs (- p a)))
  (define pb (abs (- p b)))
  (define pc (abs (- p c)))
  (match filter-type
    [0 0]
    [1 a]
    [2 b]
    [3 (quotient (+ a b) 2)]
    [4 (cond [(and (<= pa pb) (<= pa pc)) a] [(<= pb pc) b] [else c])]))

(define cifar-10-batches
  (append (for/list ([k (in-range 1 6)]) (list (format "cifar-10-batches-bin/data_batch_~a.bin" k) "train"))
          '(("cifar-10-batches-bin/test_batch.bin" "test"))))
(define cifar-10-member-names (map first cifar-10-batches))
(define cifar-10-member-splits (map second cifar-10-batches))
(define cifar-100-member-names '("cifar-100-binary/train.bin" "cifar-100-binary/test.bin"))

(define (cifar-record-bytes records)
  (apply bytes (append* records)))

(define (in-tar-order names members)
  (append-map (lambda (name) (filter (lambda (member) (equal? (first member) name)) members)) names))

(define (cifar-10-upstream members)
  (match-define (list batch-1 batch-2 batch-3 batch-4 batch-5 test) cifar-10-member-names)
  `(("cifar-10-binary.tar.gz" (tgz ,(in-tar-order (list batch-3 batch-1 test batch-5 batch-2 batch-4) members)))))

(define (cifar-100-upstream members)
  `(("cifar-100-binary.tar.gz" (tgz ,members))))

(define (emnist-members split contents)
  (for/list ([part (in-list '("train-images-idx3" "train-labels-idx1" "test-images-idx3" "test-labels-idx1"))]
             [content (in-list contents)])
    (match-define (list dims data) content)
    (list (format "gzip/emnist-~a-~a-ubyte.gz" split part) (gzip-bytes (idx-bytes dims data)))))
