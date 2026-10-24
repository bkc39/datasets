#lang racket/base
(require (only-in racket/contract/base ->* -> and/c contract-out flat-named-contract list/c listof
                  non-empty-listof or/c)
         (only-in racket/format ~s)
         (only-in racket/list index-of range)
         (only-in racket/match match match-define)
         (only-in racket/string string-join))
(provide (contract-out
          [csv-rows (->* (input-port? (non-empty-listof column-spec/c))
                         (#:header? boolean? #:missing (listof string?) #:separator char?)
                         sequence?)]
          [info->column-specs (-> (and/c hash? immutable?) (non-empty-listof column-spec/c))]))

(define column-spec/c
  (flat-named-contract 'column-spec (list/c string? (or/c 'integer 'real 'string))))

(define real-rx #px"^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$")
(define quoted-rx #px"^\"((?:[^\"]|\"\")*)\"$")

(struct record (line fields))
(struct quoted (text))

(define (csv-rows in specs #:header? [header? #t] #:missing [missing '("")] #:separator [separator #\,])
  (define split (record-splitter separator))
  (make-do-sequence
   (lambda ()
     (define next-record (record-reader in split))
     (define indices (and header? (header-indices (next-record) specs)))
     (define (next-row _)
       (match (next-record)
         [(? eof-object?) eof]
         [(record line fields) (convert-record line fields specs indices missing)]))
     (values next-row values #f #f (lambda (row) (not (eof-object? row))) #f))))

(define (record-reader in split)
  (define line 0)
  (define (read-text)
    (define text (read-line in 'linefeed))
    (when (string? text) (set! line (add1 line)))
    text)
  (lambda ()
    (define start (add1 line))
    (let loop ([text (read-text)])
      (cond
        [(eof-object? text) eof]
        [(odd? (quote-count text))
         (match (read-text)
           [(? eof-object?) (fail "unterminated quoted field" "line" start)]
           [more (loop (string-append text "\n" more))])]
        [else (record start (split (strip-cr text)))]))))

(define (quote-count text)
  (for/sum ([c (in-string text)]) (if (char=? c #\") 1 0)))

(define (strip-cr text)
  (define n (string-length text))
  (if (and (positive? n) (char=? (string-ref text (sub1 n)) #\return)) (substring text 0 (sub1 n)) text))

(define ((record-splitter separator) text)
  (define fields (split-fields text separator))
  (if (zero? (quote-count text)) fields (map decode-field fields)))

(define (split-fields text separator)
  (let loop ([start 0] [i 0] [in-quotes? #f] [fields '()])
    (cond
      [(= i (string-length text)) (reverse (cons (substring text start i) fields))]
      [(char=? (string-ref text i) #\") (loop start (add1 i) (not in-quotes?) fields)]
      [(and (not in-quotes?) (char=? (string-ref text i) separator))
       (loop (add1 i) (add1 i) #f (cons (substring text start i) fields))]
      [else (loop start (add1 i) in-quotes? fields)])))

(define (decode-field raw)
  (match raw
    [(regexp quoted-rx (list _ body)) (quoted (regexp-replace* #rx"\"\"" body "\""))]
    [(regexp #rx"\"") (fail "malformed quoted field" "field" raw)]
    [_ raw]))

(define (field-text field)
  (if (quoted? field) (quoted-text field) field))

(define (header-indices header specs)
  (match header
    [(? eof-object?) (fail "missing header" "expected" (map car specs))]
    [(record _ fields)
     (define names (map field-text fields))
     (for/list ([spec (in-list specs)])
       (or (index-of names (car spec))
           (fail "column not in header" "column" (car spec) "header" names)))]))

(define (convert-record line fields specs indices missing)
  (define cells (list->vector fields))
  (define width (vector-length cells))
  (cond
    [(and (not indices) (not (= width (length specs))))
     (fail "wrong number of fields" "line" line "expected" (length specs) "given" width)]
    [else
     (for/list ([spec (in-list specs)] [i (in-list (or indices (range width)))])
       (cond
         [(>= i width) (fail "too few fields" "line" line "column" (car spec) "given" width)]
         [else (convert-field (vector-ref cells i) spec line missing)]))]))

(define (convert-field field spec line missing)
  (match-define (list name type) spec)
  (define text (field-text field))
  (cond
    [(and (string? field) (member field missing)) 'missing]
    [(eq? type 'string) text]
    [(and (eq? type 'integer) (parse-integer text))]
    [(and (eq? type 'real) (regexp-match? real-rx text))
     (exact->inexact (string->number text 10 'number-or-false 'decimal-as-inexact))]
    [else (fail (if (eq? type 'integer) "not an integer" "not a real number")
                "line" line "column" name "value" text)]))

(define (parse-integer text)
  (define n (string-length text))
  (define sign (and (positive? n) (memv (string-ref text 0) '(#\- #\+)) (string-ref text 0)))
  (let loop ([i (if sign 1 0)] [value 0])
    (cond
      [(= i n) (and (> n (if sign 1 0)) (if (eqv? sign #\-) (- value) value))]
      [(char<=? #\0 (string-ref text i) #\9)
       (loop (add1 i) (+ (* value 10) (- (char->integer (string-ref text i)) 48)))]
      [else #f])))

(define (fail message . fields)
  (raise (exn:fail (apply string-append "csv-rows: " message (field-lines fields))
                   (current-continuation-marks))))

(define (field-lines fields)
  (match fields
    ['() '()]
    [(list* key value more)
     (cons (format "\n  ~a: ~a" key (if (list? value) (string-join (map ~s value) " ") value))
           (field-lines more))]))

(define (info->column-specs info)
  (for/list ([column (in-list (hash-ref info "columns"))])
    (list (hash-ref column "original-name")
          (match (hash-ref column "type")
            ["integer" 'integer]
            ["real" 'real]
            [(or "categorical" "string" "identifier") 'string]))))
