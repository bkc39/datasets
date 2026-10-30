#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-missing dataset-table-columns fetch-dataset
                  load-dataset-table)
         (only-in datasets/core-tests/fixture cache-files call-with-fetch-fixture rows->rktd-bytes)
         (only-in datasets/fetch-private/convert california-housing-rows covtype-rows csv-header-rows
                  esl-zip-file-names esl-zip-rows idx-file-names idx-image-rows)
         (only-in datasets/fetch-tests/fixture call-with-upstream-files error-with?)
         (only-in file/sha1 bytes->hex-string)
         (only-in net/url path->url url->string)
         (only-in racket/file file->string)
         (only-in racket/flonum fl/)
         (only-in racket/list last make-list take)
         (only-in racket/match == match match-define match-lambda)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/sequence sequence->list)
         (only-in threading ~>))
(define-runtime-path fixtures "fixtures")
(module+ test
  (require (only-in rackunit check-equal? check-exn test-case))
  (define (fixture name) (file->string (build-path fixtures name)))
  (define (soil k) (append (make-list k 0) '(1) (make-list (- 39 k) 0)))
  (define covtype-lines (fixture "covtype-mini.data"))
  (define covtype-expected
    (list (append '(2596 51 3 258 0 510 221 232 148 6279 1 0 0 0) (soil 28) '(5))
          (append '(2590 56 2 212 -6 390 220 235 151 6225 1 0 0 0) (soil 28) '(5))))
  (define cal-lines (fixture "cal_housing-mini.data"))
  (define cal-expected
    (list (list 8.3252 41.0 (fl/ 880.0 126.0) (fl/ 129.0 126.0) 322.0 (fl/ 322.0 126.0) 37.88 -122.23
                (fl/ 452600.0 100000.0))
          (list 8.3014 21.0 (fl/ 7099.0 1138.0) (fl/ 1106.0 1138.0) 2401.0 (fl/ 2401.0 1138.0) 37.86 -122.22
                (fl/ 358500.0 100000.0))))
  (define flights-expected (call-with-input-file (build-path fixtures "flights-mini.rktd") read))
  (define pixels 784)
  (define (pixel i k) (modulo (+ (* 31 k) (* 97 i)) 256))
  (define (image-bytes from count)
    (apply bytes (for*/list ([i (in-range from (+ from count))] [k (in-range pixels)]) (pixel i k))))
  (define idx-contents
    (for/hash ([name (in-list idx-file-names)]
               [content (in-list `((idx-gz (2 28 28) ,(image-bytes 0 2)) (idx-gz (2) ,(bytes 7 2))
                                   (idx-gz (1 28 28) ,(image-bytes 2 1)) (idx-gz (1) ,(bytes 9))))])
      (values name content)))
  (define (idx-upstream contents)
    (for/list ([name (in-list idx-file-names)]) (list name (hash-ref contents name))))
  (define idx-expected
    (for/list ([i (in-range 3)] [label (in-list '(7 2 9))] [split (in-list '("train" "train" "test"))])
      (append (for/list ([k (in-range pixels)]) (pixel i k)) (list label split))))
  (define zip-info
    (let ([info (dataset-info 'zip)])
      (define columns (hash-ref info "columns"))
      (hash-set* info "column-count" 6 "columns" (append (take columns 5) (list (last columns))))))
  (define (zip-upstream train test)
    (map list esl-zip-file-names `((gz ,train) (gz ,test))))
  (define zip-train (fixture "zip-mini.train"))
  (define zip-test (fixture "zip-mini.test"))
  (define zip-expected
    '((6 -1.0 -0.631 0.862 -0.0 "train") (5 1.0 0.0 -1.0 -1.0 "train") (9 -1.0 0.0 -0.0 1.0 "test")))
  (define (file-entry files file)
    (define path (hash-ref files file))
    (hash "name" file
          "urls" (list (~> path path->url url->string))
          "sha256" (~> path (call-with-input-file sha256-bytes) bytes->hex-string)))
  (define (mini-name name) (string->symbol (format "~a-mini" name)))
  (define (mini-entry name info upstream expected files)
    (hash-set* info
               "name" (symbol->string (mini-name name)) "rows" (length expected)
               "package" "datasets-fetch-fixture"
               "sha256" (~> expected rows->rktd-bytes sha256-bytes bytes->hex-string)
               "fetch" (hash-set (hash-ref info "fetch") "files"
                                 (map (match-lambda [(list file _) (file-entry files file)]) upstream))))
  (define csv-info
    (hash "columns" (map (match-lambda [(list name type) (hash "name" name "original-name" name "type" type)])
                         '(("y" "real") ("name" "string") ("x" "integer")))))
  (define registry-cases
    `((covtype ,covtype-rows (("covtype.data.gz" (gz ,covtype-lines))) ,(dataset-info 'covtype)
               ,covtype-expected)
      (california-housing ,california-housing-rows
                          (("cal_housing.tgz" (tgz (("CaliforniaHousing/cal_housing.domain" "x: continuous.\n")
                                                    ("CaliforniaHousing/cal_housing.data" ,cal-lines)))))
                          ,(dataset-info 'california-housing)
                          ,cal-expected)
      (mnist ,idx-image-rows ,(idx-upstream idx-contents) ,(dataset-info 'mnist) ,idx-expected)
      (fashion-mnist ,idx-image-rows ,(idx-upstream idx-contents) ,(dataset-info 'fashion-mnist)
                     ,idx-expected)
      (zip ,esl-zip-rows ,(zip-upstream zip-train zip-test) ,zip-info ,zip-expected)
      (nycflights13-flights ,csv-header-rows (("flights.csv" (plain ,(fixture "flights-mini.csv"))))
                            ,(dataset-info 'nycflights13-flights)
                            ,flights-expected)))
  (define converter-cases
    `(,@registry-cases
      (csv-header ,csv-header-rows (("data.csv" (plain ,(fixture "rdatasets-mini.csv")))) ,csv-info
                  ((2.5 "a, b" 1) (missing missing missing)))))

  (define (outside-levels info rows)
    (for*/list ([row (in-list rows)]
                [(column value) (in-parallel (hash-ref info "columns") row)]
                #:when (equal? (hash-ref column "type") "categorical")
                #:unless (member value (hash-ref column "levels")))
      (list (hash-ref column "name") value)))

  (test-case "converters on miniatures give the exact rows, categorical values among the registry levels"
    (for ([c (in-list converter-cases)])
      (match-define (list name converter upstream info expected) c)
      (call-with-upstream-files upstream
        (lambda (files)
          (define rows (sequence->list (converter files info)))
          (check-equal? rows expected (symbol->string name))
          (check-equal? (outside-levels info rows) '() (symbol->string name))))))

  (test-case "converters reject malformed upstream files"
    (for ([c (in-list `((,covtype-rows (("covtype.data.gz" (gz "1,2,3\n"))) ,(dataset-info 'covtype)
                                      csv-rows "wrong number of fields")
                        (,covtype-rows (("covtype.data.gz" (gz ,(string-append "x" covtype-lines))))
                                      ,(dataset-info 'covtype) csv-rows "not an integer")
                        (,california-housing-rows (("cal_housing.tgz" (tgz (("cal_housing.data" ,cal-lines)))))
                                                 ,(dataset-info 'california-housing)
                                                 tar-gz-member "no such member")
                        (,csv-header-rows (("a.csv" (plain "y\n")) ("b.csv" (plain "y\n"))) ,csv-info
                                          csv-header-rows "expected one file")))])
      (match-define (list converter upstream info who needle) c)
      (call-with-upstream-files upstream
        (lambda (files)
          (check-exn (error-with? who needle) (lambda () (sequence->list (converter files info))))))))

  (test-case "esl-zip-rows names the file and line of a malformed line"
    (for ([c (in-list `((,zip-train "9 -1 0 -0 1\n7 1 0 1\n"
                         "wrong number of tokens\n  file: zip.test.gz\n  line: 2\n  expected: 5\n  given: 4")
                        ("6 -1 0 0 1 0 0\n" ,zip-test
                         "wrong number of tokens\n  file: zip.train.gz\n  line: 1\n  expected: 5\n  given: 7")
                        (,zip-train "9 -1 abc -0 1\n"
                         "not a number\n  file: zip.test.gz\n  line: 1\n  token: abc")
                        ("6.5 -1 0 0 1\n" ,zip-test
                         "digit is not a whole number from 0 to 9\n  file: zip.train.gz\n  line: 1\n  token: 6.5")
                        (,zip-train "9 -1 0 -0 1\n10 -1 0 -0 1\n"
                         "digit is not a whole number from 0 to 9\n  file: zip.test.gz\n  line: 2\n  token: 10")
                        (,zip-train "-1 -1 0 -0 1\n"
                         "digit is not a whole number from 0 to 9\n  file: zip.test.gz\n  line: 1\n  token: -1")))])
      (match-define (list train test message) c)
      (call-with-upstream-files (zip-upstream train test)
        (lambda (files)
          (check-exn (lambda (e) (and (exn:fail? e) (equal? (exn-message e) (format "esl-zip-rows: ~a" message))))
                     (lambda () (sequence->list (esl-zip-rows files zip-info)))
                     message)))))

  (test-case "idx-image-rows rejects files that disagree with each other or with the registry"
    (for ([c (in-list `(("train-labels-idx1-ubyte.gz" (idx-gz (3) ,(bytes 7 2 1)) idx-image-rows
                         "image and label counts differ" "split: train" "images: 2" "labels: 3")
                        ("t10k-images-idx3-ubyte.gz" (idx-gz (1 27 29) ,(make-bytes 783 0)) idx-image-rows
                         "image size does not match the column count" "split: test"
                         ,(format "expected pixels: ~a" pixels) "given: 27x29")
                        ("train-images-idx3-ubyte.gz" (idx-gz (2 ,pixels) ,(image-bytes 0 2)) idx-image-rows
                         "expected 3-dimensional images and 1-dimensional labels" "split: train")
                        ("train-labels-idx1-ubyte.gz" (idx-gz (2 1) ,(bytes 7 2)) idx-image-rows
                         "expected 3-dimensional images and 1-dimensional labels" "split: train"
                         "labels: (2 1)")
                        ("t10k-labels-idx1-ubyte.gz" (gz "not idx") read-idx "not an IDX file")))])
      (match-define (list* name upstream who needles) c)
      (call-with-upstream-files (idx-upstream (hash-set idx-contents name upstream))
        (lambda (files)
          (check-exn (apply error-with? who needles)
                     (lambda () (sequence->list (idx-image-rows files (dataset-info 'mnist))))
                     name)))))

  (test-case "the converters run through core: loaded from the registry entry, cached converted"
    (for ([c (in-list registry-cases)])
      (match-define (list name _ upstream info expected) c)
      (call-with-upstream-files upstream
        (lambda (files)
          (define mini (mini-name name))
          (define entry (mini-entry name info upstream expected files))
          (call-with-fetch-fixture (list entry)
            (lambda (root _registry)
              (define table (load-dataset-table mini))
              (check-equal? (for/list ([i (in-range (length expected))])
                              (for/list ([column (in-vector (dataset-table-columns table))])
                                (match (vector-ref column i)
                                  [(== dataset-missing eq?) 'missing]
                                  [v v])))
                            expected)
              (check-equal? (cache-files root)
                            (list (format "~a/~a.rktd.gz" mini (hash-ref entry "sha256"))))
              (check-equal? (fetch-dataset mini) (build-path root (car (cache-files root))))))))))

  (test-case "core names the zip file that cannot be downloaded"
    (for ([missing (in-list esl-zip-file-names)])
      (define upstream (zip-upstream zip-train zip-test))
      (call-with-upstream-files upstream
        (lambda (files)
          (define entry (mini-entry 'zip zip-info upstream zip-expected files))
          (delete-file (hash-ref files missing))
          (call-with-fetch-fixture (list entry)
            (lambda (_root _registry)
              (check-exn (error-with? 'load-dataset-table "cannot download dataset" (format "file: ~a" missing))
                         (lambda () (load-dataset-table 'zip-mini))
                         missing))))))))
