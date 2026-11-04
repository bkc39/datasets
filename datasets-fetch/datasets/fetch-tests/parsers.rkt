#lang racket/base
(require (only-in datasets/core dataset-missing dataset-table)
         (only-in datasets/core-tests/fixture call-with-memory-limit gzip-bytes)
         (only-in datasets/fetch-private/archive gunzip-bytes gunzip-file tar-gz-member tar-gz-members
                  zip-members)
         (only-in datasets/fetch-private/csv csv-rows info->column-specs)
         (only-in datasets/fetch-private/idx read-idx)
         (only-in datasets/fetch-tests/digest column-digest parity-problems)
         (only-in datasets/fetch-tests/fixture call-with-upstream-files error-with? idx-bytes tar-bytes)
         (only-in racket/list make-list range)
         (only-in racket/random crypto-random-bytes)
         (only-in racket/file file->bytes)
         (only-in racket/match match match-define)
         (only-in racket/path path-only)
         (only-in racket/sequence sequence->list))
(module+ test
  (require (only-in rackunit check-eqv? check-equal? check-exn check-not-false check-regexp-match test-case))
  (define (csv-case-rows input specs keywords arguments)
    (sequence->list (keyword-apply csv-rows keywords arguments (list (open-input-string input) specs))))
  (define (csv-error? . needles) (apply error-with? 'csv-rows needles))
  (define csv-cases
    '((header-select "rownames,b,a\n1,x,2\n2,\"y,z\",\n" (("a" integer) ("b" string)) () ()
                     (rows (2 "x") (missing "y,z")))
      (quoting-crlf "\"a \"\"q\"\"\",1\r\n\"line\nbreak\",2\r\n" (("s" string) ("n" integer))
                    (#:header?) (#f)
                    (rows ("a \"q\"" 1) ("line\nbreak" 2)))
      (real "1\n2.5\n-3e2" (("x" real)) (#:header?) (#f) (rows (1.0) (2.5) (-300.0)))
      (int-error "n\n1\n2.5\n" (("n" integer)) () () (error "line: 3" "column: n" "value: 2.5"))
      (real-rejects-ratio "1/2\n" (("x" real)) (#:header?) (#f) (error "line: 1" "value: 1/2"))
      (real-rejects-radix "#x10\n" (("x" real)) (#:header?) (#f) (error "value: #x10"))
      (real-rejects-inf "inf\n" (("x" real)) (#:header?) (#f) (error "value: inf"))
      (int-rejects-sign-only "+\n" (("x" integer)) (#:header?) (#f) (error "value: +"))
      (missing-header-column "b\n1\n" (("a" integer)) () () (error "column not in header" "column: a"))
      (empty-input "" (("a" integer)) () () (error "missing header"))
      (ragged "1,2\n3\n" (("a" integer) ("b" integer)) (#:header?) (#f) (error "line: 2"))
      (header-row-too-wide "a,b\n1,2,3\n" (("a" integer)) () ()
                           (error "wrong number of fields" "line: 2" "expected: 2" "given: 3"))
      (header-row-too-narrow "a,b\n1\n" (("a" integer)) () ()
                             (error "wrong number of fields" "line: 2" "expected: 2" "given: 1"))
      (unterminated-quote "a\n\"x\n" (("a" string)) () () (error "unterminated quoted field" "line: 2"))
      (stray-quote "a\n\n\"q\nr\"\nx\"y\"\n" (("a" string)) () ()
                   (error "malformed quoted field" "line: 5" "field: x\"y\""))
      (quoted-empty-is-a-value "a,b\n\"\",\n" (("a" string) ("b" string)) () () (rows ("" missing)))
      (signs-and-blank-lines "n\n-4\n\n\r\n+5\n\n" (("n" integer)) () () (rows (-4) (5)))
      (blank-line-in-quotes "\"x\n\ny\"\n" (("s" string)) (#:header?) (#f) (rows ("x\n\ny")))
      (quoted-empty-alone "s\n\"\"\n" (("s" string)) () () (rows ("")))
      (byte-order-mark "\uFEFFa,b\r\n1,2\r\n" (("a" integer)) () () (rows (1)))
      (byte-order-mark-only-at-start "s\nx\n\uFEFFy\n" (("s" string)) () () (rows ("x") ("\uFEFFy")))
      (separator-and-markers "a;b\nNA;1\n;2\n" (("b" integer) ("a" string))
                             (#:missing #:separator) (("NA") #\;)
                             (rows (1 missing) (2 "")))))

  (test-case "csv-rows: case table"
    (for ([c (in-list csv-cases)])
      (match-define (list name input specs keywords arguments expected) c)
      (match expected
        [(cons 'rows rows)
         (check-equal? (csv-case-rows input specs keywords arguments) rows (symbol->string name))]
        [(cons 'error needles)
         (check-exn (apply csv-error? needles) (lambda () (csv-case-rows input specs keywords arguments))
                    (symbol->string name))])))

  (test-case "csv-rows reads reals with their sign, so -0 is -0.0"
    (for ([c (in-list '(("-0" -0.0) ("-0.0" -0.0) ("+0" 0.0) ("0" 0.0) ("-.0" -0.0)))])
      (match-define (list text expected) c)
      (match-define (list (list value)) (csv-case-rows text '(("x" real)) '(#:header?) '(#f)))
      (check-eqv? value expected text)))

  (test-case "an unterminated quote before many lines fails in time linear in the input"
    (define input (apply string-append "a\nx\"y\n" (make-list 50000 "0123456789,0123456789,0123456789\n")))
    (define result (box #f))
    (define worker
      (thread (lambda ()
                (set-box! result (with-handlers ([exn:fail? exn-message])
                                   (csv-case-rows input '(("a" string)) '() '()))))))
    (define finished (sync/timeout 5 worker))
    (kill-thread worker)
    (check-not-false finished "the reader gave up within 5 s")
    (check-regexp-match #rx"^csv-rows: unterminated quoted field\n  line: 2$" (unbox result)))

  (test-case "csv-rows reads lazily: rows before a bad line come out, the bad line raises when reached"
    (define seen (box '()))
    (check-exn (csv-error? "line: 3")
               (lambda ()
                 (for ([row (csv-rows (open-input-string "1\n2\nx\n") '(("n" integer)) #:header? #f)])
                   (set-box! seen (cons row (unbox seen))))))
    (check-equal? (reverse (unbox seen)) '((1) (2))))

  (define (idx-input dims size #:magic [magic (bytes 0 0 8 (length dims))])
    (define data (apply bytes (range size)))
    (values (idx-bytes dims data #:magic magic) data))
  (define (read-idx-list input)
    (call-with-values (lambda () (read-idx (open-input-bytes input))) list))

  (test-case "read-idx reads the dimensions and the data"
    (for ([dims (in-list '((2 2 2) (3)))])
      (define-values (input data) (idx-input dims (apply * dims)))
      (check-equal? (read-idx-list input) (list dims data) (format "~a" dims))))

  (test-case "read-idx: malformed files"
    (for ([c (in-list '((element-type (0 0 9 1) (8) 8 "unsupported element type" "type: 9")
                        (short-data (0 0 8 3) (2 2 2) 7 "short data" "expected bytes: 8" "given bytes: 7")
                        (trailing-data (0 0 8 3) (2 2 2) 9 "trailing data")
                        (no-dimensions (0 0 8 0) () 0 "no dimensions")
                        (zero-dimension (0 0 8 2) (3 0) 0 "zero dimension")
                        (bad-magic (1 0 8 1) (1) 1 "not an IDX file")
                        (short-magic (0 0) () 0 "short magic number")
                        (short-dimensions (0 0 8 2) (5) 0 "short dimension")
                        (huge-dimensions-on-short-data (0 0 8 2) (4294967295 4294967295) 8
                                                       "short data" "expected bytes: 18446744065119617025")))])
      (match-define (list* name magic dims size needles) c)
      (define-values (input _) (idx-input dims size #:magic (apply bytes magic)))
      (check-exn (apply error-with? 'read-idx needles) (lambda () (read-idx-list input))
                 (symbol->string name))))

  (test-case "info->column-specs maps registry types by original name"
    (check-equal? (info->column-specs
                   (hash "columns" (for/list ([type (in-list '("integer" "real" "categorical" "string"
                                                               "identifier"))]
                                              [i (in-naturals)])
                                     (hash "name" (format "c~a" i) "original-name" (format "C~a" i)
                                           "type" type))))
                  '(("C0" integer) ("C1" real) ("C2" string) ("C3" string) ("C4" string)))
    (check-exn (error-with? 'info->column-specs "unknown column type\n  column: C0\n  type: \"date\"")
               (lambda () (info->column-specs (hash "columns" (list (hash "original-name" "C0" "type" "date")))))))

  (test-case "gunzip-file and tar-gz-member read members by path; a missing member names both"
    (call-with-upstream-files
     '(("a.gz" (gz "plain text\n"))
       ("b.tgz" (tgz (("Dir/one.txt" "first") ("Dir/two.data" "second\n") ("empty" "")))))
     (lambda (files)
       (check-equal? (gunzip-file (hash-ref files "a.gz")) #"plain text\n")
       (for ([c (in-list '(("Dir/two.data" #"second\n") ("Dir/one.txt" #"first") ("empty" #"")))])
         (match-define (list member expected) c)
         (check-equal? (tar-gz-member (hash-ref files "b.tgz") member) expected member))
       (for ([member (in-list '("Dir/three" "Dir" "two.data"))])
         (check-exn (error-with? 'tar-gz-member "no such member" (path->string (hash-ref files "b.tgz"))
                                 member)
                    (lambda () (tar-gz-member (hash-ref files "b.tgz") member)))))))

  (define archive-members
    `(("Dir/one.txt" #"first") ("Dir/two.data" #"second\n") ("empty" #"")
      ("noise.bin" ,(crypto-random-bytes 4096)) ("text.txt" ,(apply bytes-append (make-list 400 #"repeated ")))))
  (define (local-header-methods zip)
    (for/list ([m (in-list (regexp-match-positions* #rx#"PK\3\4" zip))])
      (integer-bytes->integer zip #f #f (+ (car m) 8) (+ (car m) 10))))

  (test-case "gunzip-bytes inverts gzip"
    (for ([data (in-list (list #"" #"plain text\n" (crypto-random-bytes 70000)))])
      (check-equal? (gunzip-bytes (gzip-bytes data)) data)))

  (test-case "tar-gz-members and zip-members return members in request order, matched by exact path"
    (call-with-upstream-files
     `(("a.tgz" (tgz ,archive-members)) ("a.zip" (zip ,archive-members)))
     (lambda (files)
       (define requests '(("text.txt" "Dir/one.txt" "noise.bin" "empty" "Dir/two.data") ("empty" "empty") ()))
       (define (expected names) (for/list ([name (in-list names)]) (cadr (assoc name archive-members))))
       (for* ([request (in-list requests)] [read-members (in-list (list tar-gz-members zip-members))])
         (check-equal? (read-members (hash-ref files (if (eq? read-members zip-members) "a.zip" "a.tgz")) request)
                       (expected request) (format "~a" request)))
       (check-equal? (tar-gz-members (hash-ref files "a.tgz") '("Dir/one.txt" "Dir/two.data" "empty"))
                     (for/list ([name (in-list '("Dir/one.txt" "Dir/two.data" "empty"))])
                       (tar-gz-member (hash-ref files "a.tgz") name)))
       (check-equal? (sort (local-header-methods (file->bytes (hash-ref files "a.zip"))) <) '(0 0 0 0 8))
       (for ([c (in-list `((,tar-gz-members tar-gz-members "a.tgz") (,zip-members zip-members "a.zip")))])
         (match-define (list read-members who archive) c)
         (check-equal? (with-handlers ([exn:fail? exn-message]) (read-members (hash-ref files archive) '("Dir")))
                       (format "~a: no such member\n  archive: ~a\n  member: Dir" who (hash-ref files archive))))
       (for* ([c (in-list '(("a.tgz" tar-gz-members) ("a.zip" zip-members)))]
              [member (in-list '("Dir" "Dir/three" "two.data" "dir/one.txt"))])
         (match-define (list archive who) c)
         (define read-members (if (eq? who 'zip-members) zip-members tar-gz-members))
         (check-exn (error-with? who "no such member" (format "archive: ~a" (hash-ref files archive))
                                 (format "member: ~a" member))
                    (lambda () (read-members (hash-ref files archive) (list "empty" member)))
                    (format "~a ~a" archive member))))))

  (test-case "tar-gz-members raises the gunzip error of a corrupt archive, not the tar reader's"
    (call-with-upstream-files
     '(("bad.tgz" (bytes #"\37\213\10\0\0\0\0\0\0\3\377\377\377\377")))
     (lambda (files)
       (check-exn #rx"^inflate: "
                  (lambda () (tar-gz-members (hash-ref files "bad.tgz") '("x")))))))

  (define member-a (make-bytes 200000 65))
  (define member-b
    (apply bytes-append (for/list ([i (in-range 50000)]) (string->bytes/utf-8 (number->string (* i 7919))))))
  (define truncation-tar (tar-bytes `(("a" ,member-a) ("b" ,member-b))))

  (test-case "a gzip stream that ends inside a member raises the inflate error, never a short member"
    (define gz (gzip-bytes truncation-tar))
    (call-with-upstream-files
     `(("cut.tgz" (bytes ,(subbytes gz 0 (quotient (bytes-length gz) 2)))))
     (lambda (files)
       (define archive (hash-ref files "cut.tgz"))
       (check-equal? (tar-gz-members archive '("a")) (list member-a))
       (for ([read-member (in-list (list (lambda () (tar-gz-member archive "b"))
                                         (lambda () (tar-gz-members archive '("a" "b")))
                                         (lambda () (tar-gz-members archive '("b" "c")))))])
         (check-exn #rx"^inflate: "
                    read-member)))))

  (test-case "a tar that ends inside a member raises truncated member"
    (call-with-upstream-files
     `(("cut.tgz" (bytes ,(gzip-bytes (subbytes truncation-tar 0 (- (bytes-length truncation-tar) 100000))))))
     (lambda (files)
       (define archive (hash-ref files "cut.tgz"))
       (check-exn (error-with? 'tar-gz-member "truncated member" (format "archive: ~a" archive) "member: b")
                  (lambda () (tar-gz-member archive "b"))))))

  (define (tar-entries members)
    (define tar (tar-bytes members))
    (subbytes tar 0 (- (bytes-length tar) 1024)))
  (define (gzip-prefix tar size) (gzip-bytes (subbytes tar 0 size)))

  (define ab-tar (tar-bytes `(("a" ,(make-bytes 1000 97)) ("b" ,(make-bytes 3000 98)))))

  (test-case "a tar that ends at or inside a header or padding raises truncated archive, not untar's error"
    (define tars
      (hash 'ab ab-tar
            'azb (tar-bytes `(("a" ,(make-bytes 1000 97)) ("z" #"") ("b" ,(make-bytes 3000 98))))))
    (check-equal? (bytes-length (hash-ref tars 'ab)) 6144)
    (for ([c (in-list '((ab 1536 ("b")) (ab 1536 ("a" "b")) (ab 1520 ("b")) (ab 1520 ("a" "b"))
                        (ab 1636 ("b")) (azb 2048 ("z" "b"))))])
      (match-define (list tar size names) c)
      (call-with-upstream-files
       `(("cut.tgz" (bytes ,(gzip-prefix (hash-ref tars tar) size))))
       (lambda (files)
         (define archive (hash-ref files "cut.tgz"))
         (check-exn (error-with? 'tar-gz-members "truncated archive" (format "archive: ~a" archive) "member: b")
                    (lambda () (tar-gz-members archive names))
                    (format "~a" c))))))

  (test-case "a bad header field in an archive that goes on raises untar's error, not truncated archive"
    (define bad-mode (bytes-append (subbytes ab-tar 0 1636) #"zzzzzzz\0" (subbytes ab-tar 1644)))
    (call-with-upstream-files
     `(("bad-mode.tgz" (bytes ,(gzip-bytes bad-mode))))
     (lambda (files)
       (check-exn #rx"^untar: bad number" (lambda () (tar-gz-members (hash-ref files "bad-mode.tgz") '("b")))))))

  (test-case "tar-gz-members of no names returns no members without opening the archive"
    (call-with-upstream-files
     `(("cut.tgz" (bytes ,(gzip-prefix ab-tar 1536))))
     (lambda (files)
       (check-equal? (tar-gz-members (hash-ref files "cut.tgz") '()) '())
       (check-equal? (tar-gz-members (build-path (path-only (hash-ref files "cut.tgz")) "absent.tgz") '()) '()))))

  (test-case "tar-gz-members takes the first of duplicate names and skips the others unread"
    (define n (* 40 1024 1024))
    (define cut (bytes-append (tar-entries '(("a" #"first"))) (tar-bytes `(("a" ,(make-bytes 4096 50))))))
    (call-with-upstream-files
     `(("dup.tgz" (bytes ,(gzip-bytes (bytes-append (tar-entries `(("a" ,(make-bytes n 1))))
                                                    (tar-bytes `(("a" ,(make-bytes n 2)) ("b" #"b")))))))
       ("dup-cut.tgz" (bytes ,(gzip-prefix cut 3584))))
     (lambda (files)
       (check-equal? (call-with-memory-limit (* 64 1024 1024) 120
                                             (lambda () (tar-gz-members (hash-ref files "dup.tgz") '("a" "b"))))
                     (list (make-bytes n 1) #"b"))
       (define dup-cut (hash-ref files "dup-cut.tgz"))
       (check-equal? (tar-gz-members dup-cut '("a")) (list #"first"))
       (check-exn (error-with? 'tar-gz-members "truncated archive" (format "archive: ~a" dup-cut) "member: b")
                  (lambda () (tar-gz-members dup-cut '("a" "b")))))))

  (define (stored-gzip-then-corrupt data)
    (apply bytes-append #"\37\213\10\0\0\0\0\0\0\3"
           (append (for/list ([start (in-range 0 (bytes-length data) 65535)])
                     (define size (min 65535 (- (bytes-length data) start)))
                     (bytes-append #"\0" (integer->integer-bytes size 2 #f #f)
                                   (integer->integer-bytes (bitwise-xor #xffff size) 2 #f #f)
                                   (subbytes data start (+ start size))))
                   (list #"\377"))))

  (test-case "a gzip that fails past more than a pipe of trailing zeros raises the inflate error, not no such member"
    (call-with-upstream-files
     `(("corrupt.tgz" (bytes ,(stored-gzip-then-corrupt
                               (bytes-append (tar-bytes '(("a" #"x"))) (make-bytes (* 2 1024 1024) 0))))))
     (lambda (files)
       (check-equal? (tar-gz-members (hash-ref files "corrupt.tgz") '("a")) (list #"x"))
       (check-exn #rx"^inflate: " (lambda () (tar-gz-members (hash-ref files "corrupt.tgz") '("a" "missing")))))))

  (test-case "tar-gz-members streams: skipping a 96 MB member stays under a 32 MB memory limit"
    (define tail #"the last member")
    (call-with-upstream-files
     `(("zeros.tgz" (bytes ,(gzip-bytes (tar-bytes `(("zeros" ,(make-bytes (* 96 1024 1024) 0)) ("tail" ,tail)))))))
     (lambda (files)
       (check-equal? (call-with-memory-limit (* 32 1024 1024) 120
                                             (lambda () (tar-gz-members (hash-ref files "zeros.tgz") '("tail"))))
                     (list tail)))))

  (test-case "parity-problems on a canonical-sha256 reference: the row count, then the registry sha256"
    (define table (dataset-table '("x") '((1 2))))
    (define (problems rows sha)
      (parity-problems 'tiny table #:info (hash "sha256" "aa")
                       #:reference (hash 'rows rows 'canonical-sha256 sha)))
    (check-equal? (problems 2 "aa") '())
    (check-equal? (problems 3 "aa") '("(tiny parity rows): expected 3, actual 2"))
    (check-equal? (problems 2 "bb") '("(tiny parity canonical-sha256): expected \"bb\", actual \"aa\""))
    (check-equal? (length (problems 3 "bb")) 2))

  (test-case "digest encodes tagged values"
    (check-equal? (column-digest (vector -7 2.5 "é" dataset-missing))
                  "25244dfb83471407d64e6575f4436748ef5f6ae22b0835263444c3b67de930d5")))
