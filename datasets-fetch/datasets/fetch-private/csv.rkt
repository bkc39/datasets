#lang racket/base
(require (only-in racket/contract/base ->* -> and/c contract-out flat-named-contract list/c listof
                  non-empty-listof or/c)
         (only-in racket/format ~s)
         (only-in racket/list index-of range)
         (only-in racket/match match match-define match-lambda)
         (only-in racket/string string-join))
(provide (contract-out
          [csv-rows (->* (input-port? (non-empty-listof column-spec/c))
                         (#:header? boolean? #:missing (listof string?) #:separator char?)
                         sequence?)]
          [info->column-specs (-> (and/c hash? immutable?) (non-empty-listof column-spec/c))]))

(define column-spec/c
  (flat-named-contract 'column-spec (list/c string? (or/c 'integer 'real 'string))))

(define integer-rx #px"^[-+]?[0-9]+$")
(define real-rx #px"^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$")
(define quoted-rx #px"^\"((?:[^\"]|\"\")*)\"$")

(struct record (line fields))
(struct quoted (text))

(define spec-name (match-lambda [(list name _) name]))

(define (csv-rows in specs #:header? [header? #t] #:missing [missing '("")] #:separator [separator #\,])
  (regexp-try-match #rx"^﻿" in)
  (define next-record (record-reader in separator))
  (match-define (cons width indices)
    (if header?
        (header-layout (next-record) specs)
        (cons (length specs) (range (length specs)))))
  (in-producer (lambda ()
                 (match (next-record)
                   [(? eof-object?) eof]
                   [(record line fields) (convert-record line fields width indices specs missing)]))
               eof))

(define (record-reader in separator)
  (define line 0)
  (define (read-text)
    (define text (read-line in 'linefeed))
    (set! line (+ line (if (string? text) 1 0)))
    text)
  (define (read-record start lines open?)
    (cond
      [(not open?) (strip-cr (string-join (reverse lines) "\n"))]
      [else (match (read-text)
              [(? eof-object?) (csv-error "unterminated quoted field" "line" start)]
              [text (read-record start (cons text lines) (even? (quote-count text)))])]))
  (define (next-record)
    (define start (add1 line))
    (match (read-text)
      [(? eof-object?) eof]
      [text (match (read-record start (list text) (odd? (quote-count text)))
              ["" (next-record)]
              [joined (record start (split-record joined start separator))])]))
  next-record)

(define (quote-count text)
  (for/sum ([c (in-string text)]) (if (char=? c #\") 1 0)))

(define (strip-cr text)
  (define n (string-length text))
  (if (and (positive? n) (char=? (string-ref text (sub1 n)) #\return)) (substring text 0 (sub1 n)) text))

(define (split-record text line separator)
  (define fields (split-fields text separator))
  (if (zero? (quote-count text)) fields (map (lambda (field) (decode-field field line)) fields)))

(define (split-fields text separator)
  (let loop ([start 0] [i 0] [in-quotes? #f] [fields '()])
    (cond
      [(= i (string-length text)) (reverse (cons (substring text start i) fields))]
      [(char=? (string-ref text i) #\") (loop start (add1 i) (not in-quotes?) fields)]
      [(and (not in-quotes?) (char=? (string-ref text i) separator))
       (loop (add1 i) (add1 i) #f (cons (substring text start i) fields))]
      [else (loop start (add1 i) in-quotes? fields)])))

(define (decode-field raw line)
  (match raw
    [(regexp quoted-rx (list _ body)) (quoted (regexp-replace* #rx"\"\"" body "\""))]
    [(regexp #rx"\"") (csv-error "malformed quoted field" "line" line "field" raw)]
    [_ raw]))

(define (field-text field)
  (if (quoted? field) (quoted-text field) field))

(define (header-layout header specs)
  (match header
    [(? eof-object?) (csv-error "missing header" "expected" (map spec-name specs))]
    [(record line fields)
     (define names (map field-text fields))
     (cons (length names)
           (for/list ([name (in-list (map spec-name specs))])
             (or (index-of names name)
                 (csv-error "column not in header" "line" line "column" name "header" names))))]))

(define (convert-record line fields width indices specs missing)
  (define cells (list->vector fields))
  (cond
    [(not (= (vector-length cells) width))
     (csv-error "wrong number of fields" "line" line "expected" width "given" (vector-length cells))]
    [else (for/list ([spec (in-list specs)] [i (in-list indices)])
            (convert-field (vector-ref cells i) spec line missing))]))

(define (convert-field field spec line missing)
  (match-define (list name type) spec)
  (define text (field-text field))
  (cond
    [(and (string? field) (member field missing)) 'missing]
    [(eq? type 'string) text]
    [(and (eq? type 'integer) (regexp-match? integer-rx text)) (string->number text 10)]
    [(and (eq? type 'real) (regexp-match? real-rx text)) (string->number (string-append "#i" text) 10)]
    [else (csv-error (if (eq? type 'integer) "not an integer" "not a real number")
                     "line" line "column" name "value" text)]))

(define (csv-error message . fields)
  (error 'csv-rows "~a~a" message (apply string-append (field-lines fields))))

(define (field-lines fields)
  (match fields
    ['() '()]
    [(list* key value more)
     (cons (format "\n  ~a: ~a" key (if (list? value) (string-join (map ~s value) " ") value))
           (field-lines more))]))

(define (info->column-specs info)
  (for/list ([column (in-list (hash-ref info "columns"))])
    (define name (hash-ref column "original-name"))
    (list name
          (match (hash-ref column "type")
            ["integer" 'integer]
            ["real" 'real]
            [(or "categorical" "string" "identifier") 'string]
            [type (error 'info->column-specs "unknown column type\n  column: ~a\n  type: ~s" name type)]))))
