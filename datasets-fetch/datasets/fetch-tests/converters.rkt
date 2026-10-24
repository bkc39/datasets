#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-missing dataset-table-columns fetch-dataset
                  load-dataset-table)
         (only-in datasets/core-tests/fixture cache-files call-with-fetch-fixture rows->rktd-bytes)
         (only-in datasets/fetch-private/convert california-housing-rows covtype-rows csv-header-rows)
         (only-in datasets/fetch-tests/fixture call-with-upstream-files)
         (only-in file/sha1 bytes->hex-string)
         (only-in net/url path->url url->string)
         (only-in racket/file file->string)
         (only-in racket/flonum fl/)
         (only-in racket/list make-list)
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
                          ,cal-expected)))
  (define converter-cases
    `(,@registry-cases
      (csv-header ,csv-header-rows (("data.csv" (plain ,(fixture "rdatasets-mini.csv")))) ,csv-info
                  ((2.5 "a, b" 1) (missing missing missing)))))

  (test-case "converters on miniatures give the exact rows"
    (for ([c (in-list converter-cases)])
      (match-define (list name converter upstream info expected) c)
      (call-with-upstream-files upstream
        (lambda (files)
          (check-equal? (sequence->list (converter files info)) expected (symbol->string name))))))

  (test-case "converters reject malformed upstream files"
    (for ([c (in-list `((,covtype-rows (("covtype.data.gz" (gz "1,2,3\n"))) ,(dataset-info 'covtype)
                                      #rx"^csv-rows: wrong number of fields")
                        (,covtype-rows (("covtype.data.gz" (gz ,(string-append "x" covtype-lines))))
                                      ,(dataset-info 'covtype) #rx"^csv-rows: not an integer")
                        (,california-housing-rows (("cal_housing.tgz" (tgz (("cal_housing.data" ,cal-lines)))))
                                                 ,(dataset-info 'california-housing)
                                                 #rx"^tar-gz-member: no such member")
                        (,csv-header-rows (("a.csv" (plain "y\n")) ("b.csv" (plain "y\n"))) ,csv-info
                                          #rx"^csv-header-rows: expected one file")))])
      (match-define (list converter upstream info rx) c)
      (call-with-upstream-files upstream
        (lambda (files)
          (check-exn (lambda (e) (and (exn:fail? e) (regexp-match? rx (exn-message e))))
                     (lambda () (sequence->list (converter files info))))))))

  (test-case "the converters run through core: loaded from the registry entry, cached converted"
    (for ([c (in-list registry-cases)])
      (match-define (list name _ (list (list file upstream)) info expected) c)
      (call-with-upstream-files `((,file ,upstream))
        (lambda (files)
          (define mini (string->symbol (format "~a-mini" name)))
          (define fetch (hash-ref info "fetch"))
          (define entry
            (hash-set* info
                       "name" (symbol->string mini) "rows" (length expected)
                       "package" "datasets-fetch-fixture"
                       "sha256" (~> expected rows->rktd-bytes sha256-bytes bytes->hex-string)
                       "fetch" (hash-set fetch "files"
                                         (list (hash "name" file
                                                     "urls" (list (~> files (hash-ref file) path->url url->string))
                                                     "sha256" (call-with-input-file (hash-ref files file)
                                                               (lambda (in) (bytes->hex-string (sha256-bytes in)))))))))
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
              (check-equal? (fetch-dataset mini) (build-path root (car (cache-files root)))))))))))
