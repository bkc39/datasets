#lang racket/base
(require (only-in datasets/core-private/table dataset-missing)
         (only-in racket/unsafe/ops unsafe-vector*->immutable-vector!)
         (only-in racket/vector vector-copy vector-extend))
(provide (struct-out parse-failure) read-canonical-columns write-canonical-row)

(struct parse-failure (detail))

(define (fail form . vs)
  (raise (parse-failure (apply format form vs))))

(define (write-canonical-row row out)
  (write-string "  " out)
  (write row out)
  (newline out))

(define (read-canonical-columns in indices width rows)
  (cond
    [(not (equal? (read-bytes-line in 'linefeed) #"(")) (fail "expected an opening parenthesis")]
    [else
     (define columns (for/vector ([_ (in-list indices)]) (make-vector rows)))
     (let loop ([columns columns] [slots (column-slots columns indices width)] [capacity rows] [count 0])
       (define line (read-bytes-line in 'linefeed))
       (cond
         [(eof-object? line) (fail "unexpected end of data after row ~a" count)]
         [(and (equal? line #")") (eof-object? (peek-byte in)))
          (cons (freeze-columns columns count) count)]
         [(equal? line #")") (fail "unexpected data after the closing parenthesis")]
         [(= count capacity)
          (define larger (max 16 (* 2 count)))
          (define grown (for/vector ([c (in-vector columns)]) (vector-extend c larger)))
          (read-row! line count (column-slots grown indices width) width)
          (loop grown (column-slots grown indices width) larger (add1 count))]
         [else
          (read-row! line count slots width)
          (loop columns slots capacity (add1 count))]))]))

(define (column-slots columns indices width)
  (define slots (make-vector width #f))
  (for ([column (in-vector columns)] [i (in-list indices)])
    (vector-set! slots i column))
  slots)

(define (row-line? line)
  (define n (bytes-length line))
  (and (> n 3)
       (eqv? (bytes-ref line 0) 32) (eqv? (bytes-ref line 1) 32) (eqv? (bytes-ref line 2) 40)
       (eqv? (bytes-ref line (sub1 n)) 41)))

(define (read-row! line row slots width)
  (define end (sub1 (bytes-length line)))
  (cond
    [(not (row-line? line)) (fail "row ~a is not a list" row)]
    [else
     (let loop ([i 3] [k 0])
       (define j (cell-end line i end row))
       (cond
         [(= k width) (fail "row ~a has more than ~a values" row width)]
         [(vector-ref slots k) => (lambda (column) (vector-set! column row (cell-value line i j row)))]
         [else (cell-value line i j row)])
       (cond
         [(and (= j end) (< (add1 k) width)) (fail "row ~a has ~a values" row (add1 k))]
         [(= j end) (void)]
         [(eqv? (bytes-ref line j) 32) (loop (add1 j) (add1 k))]
         [else (fail "row ~a: expected a space after value ~a" row k)]))]))

(define (cell-end line i end row)
  (cond
    [(>= i end) (fail "row ~a: expected a value" row)]
    [(eqv? (bytes-ref line i) 34)
     (let loop ([j (add1 i)])
       (cond
         [(>= j end) (fail "row ~a: unterminated string" row)]
         [(eqv? (bytes-ref line j) 92) (loop (+ j 2))]
         [(eqv? (bytes-ref line j) 34) (add1 j)]
         [else (loop (add1 j))]))]
    [(eqv? (bytes-ref line i) 32) (fail "row ~a: expected a value" row)]
    [else
     (let loop ([j i])
       (cond
         [(or (= j end) (eqv? (bytes-ref line j) 32)) j]
         [else (loop (add1 j))]))]))

(define (cell-value line i j row)
  (cond
    [(eqv? (bytes-ref line i) 34) (string-value line i j row)]
    [(small-integer line i j)]
    [(token=? line i j #"missing") dataset-missing]
    [(number-value line i j)]
    [else (fail "row ~a: unexpected value ~a" row (subbytes line i j))]))

(define (small-integer line i j)
  (define start (if (and (< i j) (eqv? (bytes-ref line i) 45)) (add1 i) i))
  (and (< start j) (<= (- j start) 18)
       (let loop ([p start] [n 0])
         (cond
           [(= p j) (if (= start i) n (- n))]
           [(<= 48 (bytes-ref line p) 57) (loop (add1 p) (+ (* n 10) (- (bytes-ref line p) 48)))]
           [else #f]))))

(define (token=? line i j token)
  (and (= (- j i) (bytes-length token))
       (for/and ([a (in-bytes line i j)] [b (in-bytes token)]) (eqv? a b))))

(define (number-value line i j)
  (define v (string->number (bytes->string/latin-1 line #f i j) 10))
  (and (or (flonum? v) (exact-integer? v)) v))

(define (string-value line i j row)
  (define start (add1 i))
  (define stop (sub1 j))
  (cond
    [(not (bytes-utf-8-length line #f start stop)) (fail "row ~a: invalid UTF-8 in a string" row)]
    [(for/or ([b (in-bytes line start stop)]) (eqv? b 92))
     (with-handlers ([exn:fail:read? (lambda (e) (fail "row ~a: ~a" row (exn-message e)))])
       (string->immutable-string (read (open-input-bytes (subbytes line i j)))))]
    [else (string->immutable-string (bytes->string/utf-8 line #f start stop))]))

(define (freeze-columns columns count)
  (unsafe-vector*->immutable-vector!
   (for/vector #:length (vector-length columns) ([column (in-vector columns)])
     (unsafe-vector*->immutable-vector!
      (if (= count (vector-length column)) column (vector-copy column 0 count))))))
