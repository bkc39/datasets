#lang racket/base
(require (only-in datasets/core dataset-missing)
         (only-in datasets/fetch-private/archive gunzip-file tar-gz-member)
         (only-in datasets/fetch-private/csv csv-rows info->column-specs)
         (only-in datasets/fetch-private/idx read-idx)
         (only-in datasets/fetch-tests/digest column-digest)
         (only-in datasets/fetch-tests/fixture call-with-upstream-files error-with? idx-bytes)
         (only-in racket/list make-list range)
         (only-in racket/match match match-define)
         (only-in racket/sequence sequence->list))
(module+ test
  (require (for-syntax racket/base)
           (only-in datasets/core-tests/fixture call-with-memory-limit)
           (only-in datasets/fetch-private/archive inflate-zlib)
           (only-in datasets/fetch-private/bytes-port bytes->input-port)
           (only-in datasets/fetch-private/mat mat-array->bytes mat-array-class mat-array-dims mat-array-ref
                    read-mat)
           (only-in datasets/fetch-private/png read-png-gray8)
           (only-in datasets/fetch-tests/fixture mat-bytes png-bytes png-chunk zlib-bytes)
           (only-in racket/contract/combinator exn:fail:contract:blame?)
           (only-in racket/file file->bytes)
           (only-in racket/runtime-path define-runtime-path)
           (only-in rackunit check-eqv? check-equal? check-exn check-not-false check-regexp-match check-true
                    test-case)
           (only-in threading ~>))
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

  (test-case "digest encodes tagged values"
    (check-equal? (column-digest (vector -7 2.5 "é" dataset-missing))
                  "25244dfb83471407d64e6575f4436748ef5f6ae22b0835263444c3b67de930d5"))

  (define-runtime-path fixtures "fixtures")

  (define (mat-summary array)
    (list (mat-array-class array) (mat-array-dims array)
          (for/list ([i (in-range (apply * (mat-array-dims array)))]) (mat-array-ref array i))))
  (define (read-mat-summaries input names)
    (map mat-summary (read-mat (open-input-bytes input) names)))
  (define scipy-x
    '(0 126 42 168 84 210 14 140 56 182 98 224 28 154 70 196 112 238 7 133 49 175 91 217 21 147 63 189 105
        231 35 161 77 203 119 245))

  (test-case "read-mat reads scipy's compressed and plain files by name, in column-major order"
    (for ([file (in-list '("scipy-mini.mat" "scipy-mini-plain.mat"))])
      (define input (file->bytes (build-path fixtures file)))
      (define expected `((uint8 (2 3 3 2) ,scipy-x) (double (2 1) (10.0 3.0))))
      (check-equal? (read-mat-summaries input '("X" "y")) expected file)
      (check-equal? (read-mat-summaries input '("y" "X")) (reverse expected) file)))

  (define mat-variables
    '(("X" uint8 (2 3) miUINT8 (1 2 3 4 5 6))
      ("labels" double (3 1) miUINT8 (10 1 7))
      ("s" int16 (2 2) miINT16 (-300 5 -1 32767))
      ("w" double (1 2) miDOUBLE (0.5 -2.25))))
  (define mat-expected
    '((uint8 (2 3) (1 2 3 4 5 6)) (double (3 1) (10 1 7)) (int16 (2 2) (-300 5 -1 32767))
      (double (1 2) (0.5 -2.25))))
  (define mat-names '("X" "labels" "s" "w"))
  (define (last-bit-flipped input)
    (define last-byte (sub1 (bytes-length input)))
    (patched input last-byte (list (bitwise-xor 1 (bytes-ref input last-byte)))))
  (define (class-offset endianness) (if (eq? endianness 'big) 147 144))
  (define (patched input offset replacement)
    (bytes-append (subbytes input 0 offset) (apply bytes replacement)
                  (subbytes input (+ offset (length replacement)))))

  (test-case "read-mat honours both byte orders, compressed or not, and decodes by storage type"
    (for* ([endianness (in-list '(little big))] [compressed? (in-list '(#f #t))])
      (define input (mat-bytes mat-variables #:endianness endianness #:compressed? compressed?))
      (check-equal? (read-mat-summaries input mat-names) mat-expected (format "~a ~a" endianness compressed?))))

  (test-case "read-mat skips an unrequested char or cell variable after its name"
    (for* ([endianness (in-list '(little big))] [class-code (in-list '(4 1))])
      (define input (patched (mat-bytes (cons '("c" uint8 (1 3) miUINT8 (104 105 33)) mat-variables)
                                        #:endianness endianness)
                             (class-offset endianness) (list class-code)))
      (check-equal? (read-mat-summaries input mat-names) mat-expected (format "~a ~a" endianness class-code))))

  (define mat-one '(("X" uint8 (2 3) miUINT8 (1 2 3 4 5 6))))
  (define (u32 n [big-endian? #f]) (integer->integer-bytes n 4 #f big-endian?))
  (define (small-tagged type payload)
    (bytes-append (u32 (+ (arithmetic-shift (bytes-length payload) 16) type)) (make-bytes 4 0) payload))
  (define (compressed-element element)
    (define z (zlib-bytes element))
    (bytes-append (bytes 15 0 0 0) (u32 (bytes-length z)) z))
  (define (mat-reject-input edit)
    (match edit
      [`(patch ,offset ,replacement) (patched (mat-bytes mat-one) offset replacement)]
      [`(variables ,variables) (mat-bytes variables)]
      [`(keep ,n) (subbytes (mat-bytes mat-one) 0 n)]
      [`(dims ,raw)
       (define input (mat-bytes mat-one))
       (define dims (bytes-append (bytes 5 0 0 0) (u32 (length raw)) (apply bytes raw)
                                  (make-bytes (modulo (- (length raw)) 8) 0)))
       (define body (bytes-append (subbytes input 136 152) dims (subbytes input 168)))
       (bytes-append (subbytes input 0 128) (bytes 14 0 0 0) (u32 (bytes-length body))
                     body)]
      [`(compress ,depth)
       (define input (mat-bytes mat-one))
       (bytes-append (subbytes input 0 128)
                     (for/fold ([element (subbytes input 128)]) ([_ (in-range depth)]) (compressed-element element)))]
      [`(compress-element ,type ,data)
       (bytes-append (subbytes (mat-bytes mat-one) 0 128)
                     (compressed-element (bytes-append (u32 type)
                                                       (u32 (length data))
                                                       (apply bytes data))))]
      [`(small-tag ,type)
       (define element (subbytes (mat-bytes mat-one) 128))
       (bytes-append (subbytes (mat-bytes mat-one) 0 128)
                     (small-tagged type (if (= type 14) (subbytes element 8) (zlib-bytes element))))]
      [`(compressed-small-tag)
       (bytes-append (subbytes (mat-bytes mat-one) 0 128)
                     (compressed-element (small-tagged 14 (subbytes (mat-bytes mat-one) 136))))]
      [`(compressed ,cut ,appended ,flip?)
       (define input (mat-bytes mat-one #:compressed? #t))
       (define body (bytes-append (subbytes input 136 (- (bytes-length input) cut)) (apply bytes appended)))
       (bytes-append (subbytes input 0 132) (u32 (bytes-length body))
                     (if flip? (last-bit-flipped body) body))]))

  (test-case "read-mat: malformed files and unsupported variables"
    (for ([c (in-list '((bad-text (patch 0 (88)) ("X") read-mat "not a MAT-file version 5")
                        (version-2 (patch 124 (0 2)) ("X") read-mat "unsupported MAT-file version"
                                   "version: 0x0200")
                        (bad-endian (patch 126 (88 88)) ("X") read-mat "not a MAT-file version 5")
                        (short-header (keep 100) ("X") read-mat "not a MAT-file version 5")
                        (no-variables (variables ()) ("X") read-mat "no such variable" "variables: ()")
                        (size-beyond-file (patch 132 (0 1 0 0)) ("X") read-mat
                                          "element overruns its container" "offset: 128")
                        (compressed-compressed (compress 2) ("X") read-mat "compressed element is not a matrix"
                                               "offset: 128" "type: 15")
                        (compressed-uint8 (compress-element 2 (1 2 3 4 5 6 7 8)) ("X") read-mat
                                          "compressed element is not a matrix" "offset: 128" "type: 2")
                        (small-matrix (small-tag 14) ("X") read-mat "element overruns its container"
                                      "offset: 128")
                        (small-compressed (small-tag 15) ("X") read-mat "element overruns its container"
                                          "offset: 128")
                        (compressed-small-matrix (compressed-small-tag) ("X") read-mat
                                                 "element overruns its container" "offset: 128")
                        (zlib-truncated (compressed 3 () #f) ("X") inflate-zlib "truncated zlib stream")
                        (zlib-adler (compressed 0 () #t) ("X") inflate-zlib "adler-32 mismatch")
                        (zlib-trailing (compressed 0 (0) #f) ("X") inflate-zlib
                                       "trailing data after the zlib stream")
                        (missing (variables (("X" uint8 (1 1) miUINT8 (1)))) ("Z") read-mat "no such variable"
                                 "name: Z" "variables: (X)")
                        (duplicate (variables (("X" uint8 (1 1) miUINT8 (1)) ("X" uint8 (1 1) miUINT8 (2))))
                                   ("X") read-mat "duplicate variable" "name: X")
                        (complex (patch 145 (8)) ("X") read-mat "complex arrays are not supported" "name: X")
                        (char (patch 144 (4)) ("X") read-mat "unsupported array class" "name: X" "class: char")
                        (data-size (variables (("X" uint8 (2 3) miUINT8 (1 2 3 4 5)))) ("X") read-mat
                                   "data size does not match the dimensions" "dimensions: (2 3)" "bytes: 5")
                        (storage-utf8 (patch 176 (16 0 0 0)) ("X") read-mat "unsupported storage type" "name: X"
                                      "type: 16")
                        (no-dims (dims ()) ("X") read-mat "malformed dimensions" "name: X" "bytes: 0")
                        (six-byte-dims (dims (2 0 0 0 3 0)) ("X") read-mat "malformed dimensions" "bytes: 6")
                        (one-dim (dims (6 0 0 0)) ("X") read-mat "malformed dimensions" "bytes: 4")))])
      (match-define (list* name edit names who needles) c)
      (check-exn (apply error-with? who needles)
                 (lambda () (~> edit mat-reject-input open-input-bytes (read-mat names)))
                 (symbol->string name))))

  (test-case "read-mat skips a small-format element before or after a variable"
    (for* ([endianness (in-list '(little big))] [after? (in-list '(#f #t))])
      (define input (mat-bytes mat-one #:endianness endianness))
      (define small (bytes-append (u32 (+ (arithmetic-shift 3 16) 2) (eq? endianness 'big)) #"abc\0"))
      (check-equal? (read-mat-summaries (if after?
                                            (bytes-append input small)
                                            (bytes-append (subbytes input 0 128) small (subbytes input 128)))
                                        '("X"))
                    '((uint8 (2 3) (1 2 3 4 5 6)))
                    (format "~a ~a" endianness after?))))

  (test-case "read-mat allocates nothing from a declared size: 2^32-1 bytes declared on a 200-byte file"
    (define input (bytes-append (patched (mat-bytes mat-one) 132 '(255 255 255 255)) (make-bytes 8 0)))
    (check-equal? (bytes-length input) 200)
    (define outcome
      (call-with-memory-limit (* 16 1024 1024) 2
                              (lambda ()
                                (with-handlers ([exn:fail? exn-message])
                                  (read-mat (open-input-bytes input) '("X"))))))
    (check-equal? outcome "read-mat: element overruns its container\n  offset: 128"))

  (define (blames-caller? e)
    (and (exn:fail:contract:blame? e) (regexp-match? #rx"blaming: [^\n]*parsers\\.rkt" (exn-message e))))

  (test-case "mat-array-ref blames its caller for an index outside the array"
    (match-define (list array) (read-mat (open-input-bytes (mat-bytes mat-one)) '("X")))
    (for ([index (in-list '(6 -1))])
      (check-exn blames-caller? (lambda () (mat-array-ref array index))
                 (format "~a" index))))

  (test-case "mat-array->bytes returns one-byte storage column-major and blames a caller with wider storage"
    (for ([file (in-list '("scipy-mini.mat" "scipy-mini-plain.mat"))])
      (match-define (list x y) (read-mat (open-input-bytes (file->bytes (build-path fixtures file))) '("X" "y")))
      (check-equal? (mat-array->bytes x) (apply bytes (for/list ([i (in-range 36)]) (mat-array-ref x i))) file)
      (check-equal? (mat-array->bytes x) (apply bytes scipy-x) file)
      (check-exn blames-caller? (lambda () (mat-array->bytes y)) file))
    (match-define (list labels) (read-mat (open-input-bytes (mat-bytes mat-variables)) '("labels")))
    (check-equal? (mat-array->bytes labels) (bytes 10 1 7)))

  (test-case "mat-array->bytes is immutable storage: bytes-set! raises and mat-array-ref is unchanged"
    (for* ([compressed? (in-list '(#f #t))]
           [variable (in-list '(("X" uint8 (2 3) miUINT8 (1 2 3 4 5 6)) ("t" uint8 (1 3) miUINT8 (4 5 6))))])
      (match-define (list name _ _ _ (cons first _)) variable)
      (match-define (list array)
        (read-mat (open-input-bytes (mat-bytes (list variable) #:compressed? compressed?)) (list name)))
      (define storage (mat-array->bytes array))
      (check-exn exn:fail:contract? (lambda () (bytes-set! storage 0 99))
                 (format "~a ~a" compressed? name))
      (check-equal? (mat-array-ref array 0) first (format "~a ~a" compressed? name))))

  (test-case "mat-array-ref costs the same on a rank-1024 array as on a rank-2 array of the same bytes"
    (define (array-of-rank rank)
      (car (read-mat (open-input-bytes (mat-bytes `(("X" uint8 ,(cons 1024 (make-list (sub1 rank) 1)) miUINT8
                                                         ,(make-bytes 1024 1)))))
                     '("X"))))
    (define (fastest-ms array)
      (for/fold ([fastest +inf.0]) ([_ (in-range 3)])
        (define start (current-process-milliseconds))
        (for ([i (in-range 500000)]) (mat-array-ref array (bitwise-and i 1023)))
        (min fastest (- (current-process-milliseconds) start))))
    (define rank-2-ms (max 1 (fastest-ms (array-of-rank 2))))
    (define rank-1024-ms (fastest-ms (array-of-rank 1024)))
    (check-true (<= rank-1024-ms (* 2 rank-2-ms)) (format "rank 1024: ~a ms, rank 2: ~a ms" rank-1024-ms rank-2-ms)))

  (define (read-png-list input)
    (call-with-values (lambda () (read-png-gray8 input)) list))

  (test-case "read-png-gray8 decodes Pillow's reading of a file using every filter type"
    (check-equal? (~> (build-path fixtures "filters-7x6.png") file->bytes read-png-list)
                  (list 7 6 (bytes 0 37 74 111 148 185 222 101 157 213 13 69 125 181 202 21 96 171 246 65 140
                                   47 141 235 73 167 14 108 148 5 118 231 97 210 67 249 125 1 142 18 150 35))))

  (test-case "read-png-gray8 inverts png-bytes for every filter assignment and IDAT split"
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 58)
      (for ([i (in-range 200)])
        (define width (add1 (random 9)))
        (define height (add1 (random 9)))
        (define levels (if (even? i) 256 3))
        (define pixels (apply bytes (for/list ([_ (in-range (* width height))])
                                      (* (random levels) (if (= levels 3) 127 1)))))
        (define filters (for/list ([_ (in-range height)]) (random 5)))
        (define chunks (add1 (random 3)))
        (check-equal? (read-png-list (png-bytes width height pixels #:filters filters #:idat-chunks chunks))
                      (list width height pixels)
                      (format "~a×~a filters ~a chunks ~a" width height filters chunks)))))

  (test-case "read-png-gray8: Paeth breaks a tie with the up-left byte toward the left, then the up byte"
    (for ([pixels (in-list (list (bytes 10 5 20 77) (bytes 10 20 5 77)))])
      (check-equal? (read-png-list (png-bytes 2 2 pixels #:filters '(0 4))) (list 2 2 pixels)
                    (format "~a" (bytes->list pixels)))))

  (define png-base (png-bytes 3 2 (bytes 1 2 3 4 5 6)))
  (define (png-reject-input edit)
    (match edit
      [`(patch ,offset ,replacement) (patched png-base offset replacement)]
      [`(drop ,start ,end) (bytes-append (subbytes png-base 0 start)
                                         (subbytes png-base (modulo end (bytes-length png-base))))]
      [`(insert ,type ,data) (bytes-append (subbytes png-base 0 33) (png-chunk type (apply bytes data))
                                           (subbytes png-base 33))]
      [`(idat ,raw) (bytes-append (subbytes png-base 0 33) (png-chunk "IDAT" (zlib-bytes (apply bytes raw)))
                                  (png-chunk "IEND" #""))]
      [`(truncate ,n) (subbytes png-base 0 (- (bytes-length png-base) n))]
      [`(keep ,n) (subbytes png-base 0 n)]
      [`(append ,extra) (bytes-append png-base (apply bytes extra))]))

  (test-case "read-png-gray8: malformed and unsupported files"
    (for ([c (in-list '((signature (patch 0 (0)) "not a PNG file")
                        (short-signature (keep 5) "not a PNG file")
                        (ihdr-not-first (drop 8 33) "first chunk is not IHDR")
                        (bit-depth-16 (patch 24 (16)) "unsupported image" "bit depth: 16")
                        (colour-type-2 (patch 25 (2)) "unsupported image" "colour type: 2")
                        (interlace-1 (patch 28 (1)) "unsupported image" "interlace: 1")
                        (zero-width (patch 16 (0 0 0 0)) "empty image" "width: 0")
                        (palette (insert "PLTE" (0 0 0)) "unsupported critical chunk" "type: PLTE")
                        (filter-5 (idat (0 1 2 3 5 4 5 6)) "unknown filter type" "row: 1" "filter: 5")
                        (idat-short (idat (0 1 2 3 0 4 5)) "image data size mismatch" "expected: 8" "given: 7")
                        (idat-long (idat (0 1 2 3 0 4 5 6 7)) "image data size mismatch" "expected: 8"
                                   "given: 9")
                        (no-idat (drop 33 -12) "missing IDAT")
                        (no-iend (truncate 12) "missing IEND")
                        (after-iend (append (0)) "data after IEND")
                        (too-large (patch 16 (0 0 32 0 0 0 32 1)) "image too large" "width: 8192"
                                   "height: 8193")
                        (length-past-end (patch 33 (0 1 0 0)) "chunk overruns the file" "type: IDAT")
                        (header-past-end (truncate 6) "chunk overruns the file" "type: IE")))])
      (match-define (list* name edit needles) c)
      (check-exn (apply error-with? 'read-png-gray8 needles) (lambda () (read-png-list (png-reject-input edit)))
                 (symbol->string name))))

  (test-case "read-png-gray8: Paeth ties on hand-written scanlines go to the left byte, then the up byte"
    (check-equal? (read-png-list (png-reject-input '(idat (0 20 25 15 4 246 20 84))))
                  (list 3 2 (bytes 20 25 15 10 30 99))))

  (test-case "read-png-gray8 skips ancillary chunks"
    (check-equal? (read-png-list (png-reject-input '(insert "tEXt" (67 0 104 105))))
                  (list 3 2 (bytes 1 2 3 4 5 6))))

  (test-case "inflate-zlib inverts deflate with a zlib header and adler-32"
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 58)
      (for ([size (in-list '(0 1 2 100 5552 5553 70000))])
        (define data (apply bytes (for/list ([_ (in-range size)]) (random 256))))
        (check-equal? (inflate-zlib (zlib-bytes data) size) data (format "~a bytes" size)))))

  (test-case "bytes->input-port reads the bytes from start, counts its position, and records reaching the end"
    (define data (bytes 9 8 1 2 3 4 5))
    (define-values (in end-seen?) (bytes->input-port data 2))
    (define buffer (make-bytes 2))
    (check-equal? (list (peek-bytes-avail! buffer 2 #f in) buffer (file-position in) (end-seen?))
                  (list 2 (bytes 3 4) 0 #f))
    (check-equal? (list (read-bytes 2 in) (file-position in) (peek-bytes 1 0 in) (end-seen?))
                  (list (bytes 1 2) 2 (bytes 3) #f))
    (check-equal? (list (read-bytes 9 in) (file-position in) (end-seen?)) (list (bytes 3 4 5) 5 #t))
    (check-equal? (list (read-byte in) (file-position in)) (list eof 5)))

  (define (deflate-block-type z) (bitwise-and (arithmetic-shift (bytes-ref z 2) -1) 3))
  (test-case "inflate-zlib reports every cut of a fixed, a dynamic and a stored stream as truncated"
    (for ([block-type (in-list '(1 2 0))]
          [data (in-list (list #"hello, hello, hello hello"
                               (string->bytes/latin-1
                                (apply string-append (for/list ([i (in-range 40)])
                                                       (format "~a squared is ~a; " i (* i i)))))
                               (apply bytes (for/list ([i (in-range 64)]) (modulo (* i 97) 256)))))])
      (define z (zlib-bytes data))
      (check-eqv? (deflate-block-type z) block-type)
      (for ([k (in-range 2 (bytes-length z))])
        (check-exn (error-with? 'inflate-zlib "truncated zlib stream") (lambda () (inflate-zlib (subbytes z 0 k) 10000))
                   (format "block type ~a cut at ~a of ~a" block-type k (bytes-length z))))))

  (define zlib-hello (zlib-bytes #"hello, hello, hello"))
  (test-case "inflate-zlib: malformed streams and the output limit"
    (for ([c (in-list `((empty #"" 100 "truncated zlib stream")
                        (one-byte #"x" 100 "truncated zlib stream")
                        (method-7 ,(bytes #x77 #x09 1 0 0 0 0) 100 "not a zlib stream")
                        (window-8 ,(bytes #x88 #x1c 1 0 0 0 0) 100 "not a zlib stream")
                        (check-bits ,(bytes #x78 #x9d 1 0 0 0 0) 100 "not a zlib stream")
                        (dictionary ,(bytes #x78 #x20 0 0 0 0 1 0 0 0 0) 100 "preset dictionary not supported")
                        (truncated ,(subbytes zlib-hello 0 (- (bytes-length zlib-hello) 2)) 100
                                   "truncated zlib stream")
                        (truncated-body ,(subbytes zlib-hello 0 8) 100 "truncated zlib stream")
                        (adler ,(last-bit-flipped zlib-hello) 100 "adler-32 mismatch")
                        (trailing ,(bytes-append zlib-hello #"\0") 100 "trailing data after the zlib stream")
                        (block-type-3 ,(bytes #x78 #x9c #xff 0 0 0 0) 100 "invalid deflate data")
                        (limit ,zlib-hello 18 "inflated data exceeds the limit" "limit: 18")))])
      (match-define (list* name input limit needles) c)
      (check-exn (apply error-with? 'inflate-zlib needles) (lambda () (inflate-zlib input limit))
                 (symbol->string name)))
    (check-equal? (inflate-zlib zlib-hello 19) #"hello, hello, hello"))

  (test-case "inflate-zlib stops at the limit: 10 MB of output under a 1 MB limit grows less than 8 MB"
    (define bomb (zlib-bytes (make-bytes (* 10 1024 1024) 0)))
    (define outcome
      (call-with-memory-limit (* 8 1024 1024) 60
                              (lambda ()
                                (with-handlers ([exn:fail? exn-message])
                                  (inflate-zlib bomb (* 1024 1024))))))
    (check-true (< (bytes-length bomb) (* 16 1024)) (format "~a bytes compressed" (bytes-length bomb)))
    (check-equal? outcome (format "inflate-zlib: inflated data exceeds the limit\n  limit: ~a" (* 1024 1024))))

  (define big-size (* 40 1024 1024))
  (define big-mat (mat-bytes `(("X" uint8 (1024 ,(quotient big-size 1024)) miUINT8 ,(make-bytes big-size 7)))
                             #:compressed? #t))
  (test-case "inflate-zlib and read-mat of a compressed 40 MB array hold at most about twice the inflated size"
    (for ([c (in-list (list (list (+ big-size 56) (lambda () (bytes-length (inflate-zlib (subbytes big-mat 136)
                                                                                         (* 2 big-size)))))
                            (list big-size (lambda () (~> big-mat open-input-bytes (read-mat '("X")) car
                                                          mat-array->bytes bytes-length)))))])
      (match-define (list expected thunk) c)
      (check-equal? (call-with-memory-limit (+ (quotient (* 22 big-size) 10) (* 4 1024 1024)) 60 thunk) expected))))
