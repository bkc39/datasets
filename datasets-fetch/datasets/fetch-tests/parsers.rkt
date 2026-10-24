#lang racket/base
(require (only-in datasets/core dataset-missing)
         (only-in datasets/fetch-private/archive gunzip-file tar-gz-member)
         (only-in datasets/fetch-private/csv csv-rows info->column-specs)
         (only-in datasets/fetch-tests/digest column-digest)
         (only-in datasets/fetch-tests/fixture call-with-upstream-files)
         (only-in racket/match match match-define))
(module+ test
  (require (only-in rackunit check-equal? check-exn test-case))
  (define (csv-case-rows input specs options)
    (define sorted (sort options keyword<? #:key car))
    (for/list ([row (keyword-apply csv-rows (map car sorted) (map cadr sorted)
                                   (list (open-input-string input) specs))])
      row))
  (define ((csv-error? . needles) e)
    (and (exn:fail? e) (not (exn:fail:contract? e))
         (regexp-match? #rx"^csv-rows: " (exn-message e))
         (for/and ([n (in-list needles)]) (regexp-match? (regexp-quote n) (exn-message e)))))
  (define csv-cases
    '((header-select "rownames,b,a\n1,x,2\n2,\"y,z\",\n" (("a" integer) ("b" string)) ()
                     (rows (2 "x") (missing "y,z")))
      (quoting-crlf "\"a \"\"q\"\"\",1\r\n\"line\nbreak\",2\r\n" (("s" string) ("n" integer))
                    ((#:header? #f))
                    (rows ("a \"q\"" 1) ("line\nbreak" 2)))
      (real "1\n2.5\n-3e2" (("x" real)) ((#:header? #f)) (rows (1.0) (2.5) (-300.0)))
      (int-error "n\n1\n2.5\n" (("n" integer)) () (error "line: 3" "column: n" "value: 2.5"))
      (real-rejects-ratio "1/2\n" (("x" real)) ((#:header? #f)) (error "line: 1" "value: 1/2"))
      (real-rejects-radix "#x10\n" (("x" real)) ((#:header? #f)) (error "value: #x10"))
      (real-rejects-inf "inf\n" (("x" real)) ((#:header? #f)) (error "value: inf"))
      (int-rejects-sign-only "+\n" (("x" integer)) ((#:header? #f)) (error "value: +"))
      (missing-header-column "b\n1\n" (("a" integer)) () (error "column not in header" "column: a"))
      (empty-input "" (("a" integer)) () (error "missing header"))
      (ragged "1,2\n3\n" (("a" integer) ("b" integer)) ((#:header? #f)) (error "line: 2"))
      (unterminated-quote "a\n\"x\n" (("a" string)) () (error "unterminated quoted field" "line: 2"))
      (stray-quote "a\nx\"y\"\n" (("a" string)) () (error "malformed quoted field"))
      (quoted-empty-is-a-value "a,b\n\"\",\n" (("a" string) ("b" string)) () (rows ("" missing)))
      (signs-and-blank-lines "n\n-4\n+5\n\n" (("n" integer)) () (rows (-4) (5) (missing)))
      (separator-and-markers "a;b\nNA;1\n;2\n" (("b" integer) ("a" string))
                             ((#:separator #\;) (#:missing ("NA")))
                             (rows (1 missing) (2 "")))))

  (test-case "P csv-rows: case table"
    (for ([c (in-list csv-cases)])
      (match-define (list name input specs options expected) c)
      (match expected
        [(cons 'rows rows) (check-equal? (csv-case-rows input specs options) rows (symbol->string name))]
        [(cons 'error needles)
         (check-exn (apply csv-error? needles) (lambda () (csv-case-rows input specs options))
                    (symbol->string name))])))

  (test-case "P csv-rows reads lazily: rows before a bad line come out, the bad line raises when reached"
    (define seen (box '()))
    (check-exn (csv-error? "line: 3")
               (lambda ()
                 (for ([row (csv-rows (open-input-string "1\n2\nx\n") '(("n" integer)) #:header? #f)])
                   (set-box! seen (cons row (unbox seen))))))
    (check-equal? (reverse (unbox seen)) '((1) (2))))

  (test-case "P info->column-specs maps registry types by original name"
    (check-equal? (info->column-specs
                   (hash "columns" (for/list ([type (in-list '("integer" "real" "categorical" "string"
                                                               "identifier"))]
                                              [i (in-naturals)])
                                     (hash "name" (format "c~a" i) "original-name" (format "C~a" i)
                                           "type" type))))
                  '(("C0" integer) ("C1" real) ("C2" string) ("C3" string) ("C4" string))))

  (test-case "P gunzip-file and tar-gz-member read members by path; a missing member names both"
    (call-with-upstream-files
     '(("a.gz" (gz "plain text\n"))
       ("b.tgz" (tgz (("Dir/one.txt" "first") ("Dir/two.data" "second\n") ("empty" "")))))
     (lambda (files)
       (check-equal? (gunzip-file (hash-ref files "a.gz")) #"plain text\n")
       (for ([c (in-list '(("Dir/two.data" #"second\n") ("Dir/one.txt" #"first") ("empty" #"")))])
         (match-define (list member expected) c)
         (check-equal? (tar-gz-member (hash-ref files "b.tgz") member) expected member))
       (for ([member (in-list '("Dir/three" "Dir" "two.data"))])
         (check-exn (lambda (e)
                      (and (exn:fail? e)
                           (regexp-match? #rx"^tar-gz-member: no such member" (exn-message e))
                           (regexp-match? (regexp-quote (path->string (hash-ref files "b.tgz")))
                                          (exn-message e))
                           (regexp-match? (regexp-quote member) (exn-message e))))
                    (lambda () (tar-gz-member (hash-ref files "b.tgz") member)))))))

  (test-case "digest: the tagged encoding scripts/fetch-parity.py computes"
    (check-equal? (column-digest (vector -7 2.5 "é" dataset-missing))
                  "25244dfb83471407d64e6575f4436748ef5f6ae22b0835263444c3b67de930d5")))
