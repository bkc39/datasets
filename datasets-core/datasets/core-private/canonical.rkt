#lang racket/base
(require (only-in datasets/core-private/convert row-value?)
         (only-in datasets/core-private/table dataset-missing)
         (only-in racket/match match-define)
         (only-in racket/unsafe/ops unsafe-vector*->immutable-vector!)
         (only-in racket/vector vector-copy vector-extend))
(provide (struct-out parse-failure) read-canonical-columns write-canonical-rows)

(struct parse-failure (detail))

(define (fail form . vs)
  (raise (parse-failure (apply format form vs))))

(define space (char->integer #\space))
(define quote-mark (char->integer #\"))
(define backslash (char->integer #\\))
(define minus (char->integer #\-))
(define digit-0 (char->integer #\0))
(define digit-9 (char->integer #\9))
(define close-paren (char->integer #\)))
(define row-prefix #"  (")
(define exponent #"e[+-]?(?:0?[0-9]{1,2}|[12][0-9]{2}|3[01][0-9]|32[0-4])")
(define number-rx
  (byte-pregexp
   (bytes-append #"^(?:-?[0-9]{1,1000}|-?[0-9]{1,21}(?:\\.[0-9]{1,21})?(?:" exponent
                 #")?|[+-]inf\\.0|\\+nan\\.0)$")))
(define integer-bound (expt 10 1000))
(define cell-budget (expt 2 26))
(define preview-bytes 40)

(define (write-canonical-rows rows width out)
  (write-string "(\n" out)
  (cond
    [(for/or ([row rows] [index (in-naturals)])
       (cond
         [(canonical-row? row width) (write-canonical-row row out) #f]
         [else (list index row)]))
     => values]
    [else (write-string ")\n" out) #f]))

(define (canonical-row? row width)
  (and (list? row) (= (length row) width) (andmap canonical-value? row)))

(define (canonical-value? v)
  (and (row-value? v) (or (not (exact-integer? v)) (< (abs v) integer-bound))))

(define (write-canonical-row row out)
  (match-define (cons first more) row)
  (write-bytes row-prefix out)
  (write first out)
  (for ([v (in-list more)])
    (write-char #\space out)
    (write v out))
  (write-string ")\n" out))

(define (read-canonical-columns in indices width rows [budget cell-budget])
  (cond
    [(not (equal? (read-bytes-line in 'linefeed) #"(")) (fail "expected an opening parenthesis")]
    [else
     (define capacity (min rows (quotient budget (max 1 (length indices)))))
     (define columns (for/vector ([_ (in-list indices)]) (make-vector capacity)))
     (let loop ([columns columns] [slots (column-slots columns indices width)] [capacity capacity] [count 0])
       (define line (read-bytes-line in 'linefeed))
       (cond
         [(eof-object? line) (fail "unexpected end of data after row ~a" count)]
         [(and (equal? line #")") (eof-object? (peek-byte in)))
          (cons (freeze-columns columns count) count)]
         [(equal? line #")") (fail "unexpected data after the closing parenthesis")]
         [else
          (define-values (room room-slots room-capacity)
            (make-room columns slots capacity count rows indices width))
          (read-row! line count room-slots width)
          (loop room room-slots room-capacity (add1 count))]))]))

(define (make-room columns slots capacity count rows indices width)
  (cond
    [(< count capacity) (values columns slots capacity)]
    [else
     (define doubled (max 16 (* 2 count)))
     (define larger (if (< count rows) (min rows doubled) doubled))
     (define grown (for/vector ([c (in-vector columns)]) (vector-extend c larger)))
     (values grown (column-slots grown indices width) larger)]))

(define (column-slots columns indices width)
  (define slots (make-vector width #f))
  (for ([column (in-vector columns)] [i (in-list indices)])
    (vector-set! slots i column))
  slots)

(define (row-line? line)
  (define n (bytes-length line))
  (and (> n 3) (equal? (subbytes line 0 3) row-prefix) (eqv? (bytes-ref line (sub1 n)) close-paren)))

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
         [(eqv? (bytes-ref line j) space) (loop (add1 j) (add1 k))]
         [else (fail "row ~a: expected a space after value ~a" row k)]))]))

(define (cell-end line i end row)
  (cond
    [(>= i end) (fail "row ~a: expected a value" row)]
    [(eqv? (bytes-ref line i) quote-mark)
     (let loop ([j (add1 i)])
       (cond
         [(>= j end) (fail "row ~a: unterminated string" row)]
         [(eqv? (bytes-ref line j) backslash) (loop (+ j 2))]
         [(eqv? (bytes-ref line j) quote-mark) (add1 j)]
         [else (loop (add1 j))]))]
    [(eqv? (bytes-ref line i) space) (fail "row ~a: expected a value" row)]
    [else
     (let loop ([j i])
       (cond
         [(or (= j end) (eqv? (bytes-ref line j) space)) j]
         [else (loop (add1 j))]))]))

(define (cell-value line i j row)
  (cond
    [(eqv? (bytes-ref line i) quote-mark) (string-value line i j row)]
    [(small-integer line i j)]
    [(token=? line i j #"missing") dataset-missing]
    [(regexp-match? number-rx line i j) (string->number (bytes->string/latin-1 line #f i j) 10)]
    [else (fail "row ~a: unexpected value ~a" row (token-preview line i j))]))

(define (token-preview line i j)
  (define shown (bytes->string/utf-8 line #\uFFFD i (min j (+ i preview-bytes))))
  (if (> (- j i) preview-bytes) (string-append shown "…") shown))

(define (small-integer line i j)
  (define start (if (and (< i j) (eqv? (bytes-ref line i) minus)) (add1 i) i))
  (and (< start j) (<= (- j start) 18)
       (let loop ([p start] [n 0])
         (cond
           [(= p j) (if (= start i) n (- n))]
           [(<= digit-0 (bytes-ref line p) digit-9)
            (loop (add1 p) (+ (* n 10) (- (bytes-ref line p) digit-0)))]
           [else #f]))))

(define (token=? line i j token)
  (and (= (- j i) (bytes-length token))
       (for/and ([a (in-bytes line i j)] [b (in-bytes token)]) (eqv? a b))))

(define (string-value line i j row)
  (define start (add1 i))
  (define stop (sub1 j))
  (cond
    [(not (bytes-utf-8-length line #f start stop)) (fail "row ~a: invalid UTF-8 in a string" row)]
    [(for/or ([b (in-bytes line start stop)]) (eqv? b backslash))
     (with-handlers ([exn:fail:read? (lambda (e) (fail "row ~a: ~a" row (exn-message e)))])
       (string->immutable-string (read (open-input-bytes (subbytes line i j)))))]
    [else (string->immutable-string (bytes->string/utf-8 line #f start stop))]))

(define (freeze-columns columns count)
  (unsafe-vector*->immutable-vector!
   (for/vector #:length (vector-length columns) ([column (in-vector columns)])
     (unsafe-vector*->immutable-vector!
      (if (= count (vector-length column)) column (vector-copy column 0 count))))))
