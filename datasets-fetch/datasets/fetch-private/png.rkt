#lang racket/base
(require (only-in datasets/fetch-private/archive inflate-zlib)
         (only-in racket/contract/base -> contract-out)
         (only-in racket/list splitf-at)
         (only-in racket/match match))
(provide (contract-out
          [read-png-gray8 (-> bytes? (values exact-positive-integer? exact-positive-integer? bytes?))]))

(define signature #"\211PNG\r\n\32\n")
(define pixel-budget (expt 2 26))

(struct chunk (type data))

(define (read-png-gray8 png)
  (define chunks (and (equal? (subbytes png 0 (min 8 (bytes-length png))) signature) (png-chunks png)))
  (match chunks
    [#f (error 'read-png-gray8 "not a PNG file")]
    [(cons (chunk "IHDR" header) rest)
     #:when (= (bytes-length header) 13)
     (define-values (width height) (checked-header header))
     (define raw (inflate-zlib (image-data rest) (+ (* height (add1 width)) 1)))
     (values width height (unfilter raw width height))]
    [_ (error 'read-png-gray8 "first chunk is not IHDR")]))

(define (png-chunks png)
  (define size (bytes-length png))
  (let loop ([start 8] [chunks '()])
    (define type (bytes->string/latin-1 (subbytes png (min size (+ start 4)) (min size (+ start 8)))))
    (define end (and (<= (+ start 8) size)
                     (+ start 12 (integer-bytes->integer png #f #t start (+ start 4)))))
    (cond
      [(= start size) (reverse chunks)]
      [(or (not end) (> end size)) (error 'read-png-gray8 "chunk overruns the file\n  type: ~a" type)]
      [(and (equal? type "IEND") (< end size)) (error 'read-png-gray8 "data after IEND")]
      [(equal? type "IEND") (reverse (cons (chunk type #"") chunks))]
      [else (loop end (cons (chunk type (subbytes png (+ start 8) (- end 4))) chunks))])))

(define (checked-header header)
  (define width (integer-bytes->integer header #f #t 0 4))
  (define height (integer-bytes->integer header #f #t 4 8))
  (match (bytes->list (subbytes header 8 13))
    [(list depth colour compression filter interlace)
     (unless (and (= depth 8) (= colour 0) (= compression 0) (= filter 0) (= interlace 0))
       (error 'read-png-gray8
              "unsupported image\n  bit depth: ~a\n  colour type: ~a\n  interlace: ~a"
              depth colour interlace))
     (when (or (zero? width) (zero? height))
       (error 'read-png-gray8 "empty image\n  width: ~a\n  height: ~a" width height))
     (when (> (* width height) pixel-budget)
       (error 'read-png-gray8 "image too large\n  width: ~a\n  height: ~a" width height))
     (values width height)]))

(define (image-data chunks)
  (define kept (filter (lambda (c) (critical? (chunk-type c))) chunks))
  (define-values (idat after) (splitf-at kept (lambda (c) (equal? (chunk-type c) "IDAT"))))
  (match after
    ['() (error 'read-png-gray8 "missing IEND")]
    [(list* (chunk "IEND" _) _) #:when (null? idat) (error 'read-png-gray8 "missing IDAT")]
    [(list (chunk "IEND" _)) (apply bytes-append (map chunk-data idat))]
    [(list* (chunk type _) _) (error 'read-png-gray8 "unsupported critical chunk\n  type: ~a" type)]))

(define (critical? type)
  (char-upper-case? (string-ref type 0)))

(define (unfilter raw width height)
  (define expected (* height (add1 width)))
  (define pixels (make-bytes (* width height)))
  (unless (= (bytes-length raw) expected)
    (error 'read-png-gray8 "image data size mismatch\n  expected: ~a\n  given: ~a"
           expected (bytes-length raw)))
  (for ([row (in-range height)])
    (unfilter-row! pixels raw row width))
  pixels)

(define (unfilter-row! pixels raw row width)
  (define filter-type (bytes-ref raw (* row (add1 width))))
  (define base (* row width))
  (define predict (row-predictor filter-type row))
  (for ([x (in-range width)])
    (define a (if (zero? x) 0 (bytes-ref pixels (+ base x -1))))
    (define b (if (zero? row) 0 (bytes-ref pixels (- (+ base x) width))))
    (define c (if (or (zero? x) (zero? row)) 0 (bytes-ref pixels (- (+ base x -1) width))))
    (define filtered (bytes-ref raw (+ (* row (add1 width)) 1 x)))
    (bytes-set! pixels (+ base x) (bitwise-and (+ filtered (predict a b c)) 255))))

(define (row-predictor filter-type row)
  (match filter-type
    [0 (lambda (_a _b _c) 0)]
    [1 (lambda (a _b _c) a)]
    [2 (lambda (_a b _c) b)]
    [3 (lambda (a b _c) (quotient (+ a b) 2))]
    [4 paeth]
    [_ (error 'read-png-gray8 "unknown filter type\n  row: ~a\n  filter: ~a" row filter-type)]))

(define (paeth a b c)
  (define p (- (+ a b) c))
  (define pa (abs (- p a)))
  (define pb (abs (- p b)))
  (define pc (abs (- p c)))
  (cond
    [(and (<= pa pb) (<= pa pc)) a]
    [(<= pb pc) b]
    [else c]))
