#lang racket/base
(require (only-in datasets/core dataset-info dataset-missing dataset-table-columns fetch-dataset
                  load-dataset-table)
         (only-in datasets/core-tests/fixture cache-files call-with-fetch-fixture rows->rktd-bytes)
         (only-in datasets/fetch-private/convert california-housing-rows covtype-rows csv-header-rows)
         (only-in datasets/fetch-tests/fixture call-with-upstream-files)
         (only-in file/sha1 bytes->hex-string)
         (only-in net/url path->url url->string)
         (only-in racket/flonum fl/)
         (only-in racket/list make-list)
         (only-in racket/match == match match-define))
(module+ test
  (require (only-in rackunit check-equal? check-exn test-case))
  (define (soil k) (append (make-list k 0) '(1) (make-list (- 39 k) 0)))
  (define covtype-lines
    (string-append
     "2596,51,3,258,0,510,221,232,148,6279,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"
     "0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,5\n"
     "2590,56,2,212,-6,390,220,235,151,6225,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"
     "0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,5\n"))
  (define covtype-expected
    (list (append '(2596 51 3 258 0 510 221 232 148 6279 1 0 0 0) (soil 28) '(5))
          (append '(2590 56 2 212 -6 390 220 235 151 6225 1 0 0 0) (soil 28) '(5))))
  (define cal-lines
    (string-append
     "-122.230000,37.880000,41.000000,880.000000,129.000000,322.000000,126.000000,8.325200,452600.000000\n"
     "-122.220000,37.860000,21.000000,7099.000000,1106.000000,2401.000000,1138.000000,8.301400,358500.000000\n"))
  (define cal-expected
    (list (list 8.3252 41.0 (fl/ 880.0 126.0) (fl/ 129.0 126.0) 322.0 (fl/ 322.0 126.0) 37.88 -122.23
                (fl/ 452600.0 100000.0))
          (list 8.3014 21.0 (fl/ 7099.0 1138.0) (fl/ 1106.0 1138.0) 2401.0 (fl/ 2401.0 1138.0) 37.86 -122.22
                (fl/ 358500.0 100000.0))))
  (define csv-info
    (hash "columns" (for/list ([c (in-list '(("y" "real") ("name" "string") ("x" "integer")))])
                      (hash "name" (car c) "original-name" (car c) "type" (cadr c)))))
  (define converter-cases
    `((covtype ,covtype-rows (("covtype.data.gz" (gz ,covtype-lines))) ,(dataset-info 'covtype)
               ,covtype-expected)
      (california-housing ,california-housing-rows
                          (("cal_housing.tgz" (tgz (("CaliforniaHousing/cal_housing.domain" "x: continuous.\n")
                                                    ("CaliforniaHousing/cal_housing.data" ,cal-lines)))))
                          ,(dataset-info 'california-housing)
                          ,cal-expected)
      (csv-header ,csv-header-rows
                  (("data.csv" (plain "\"rownames\",\"name\",\"x\",\"y\"\n\"1\",\"a, b\",1,2.5\n\"2\",,,\n")))
                  ,csv-info
                  ((2.5 "a, b" 1) (missing missing missing)))))

  (test-case "V converters on miniatures give the exact rows"
    (for ([c (in-list converter-cases)])
      (match-define (list name converter upstream info expected) c)
      (call-with-upstream-files upstream
        (lambda (files)
          (check-equal? (for/list ([row (converter files info)]) row) expected (symbol->string name))))))

  (test-case "V california-housing divides as scikit-learn does"
    (check-equal? (car cal-expected)
                  '(8.3252 41.0 6.984126984126984 1.0238095238095237 322.0 2.5555555555555554 37.88 -122.23
                    4.526)))

  (test-case "V converters reject malformed upstream files"
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
                     (lambda () (for/list ([row (converter files info)]) row)))))))

  (test-case "V the converters run through core: loaded from the registry entry, cached converted"
    (for ([c (in-list converter-cases)] #:unless (eq? (car c) 'csv-header))
      (match-define (list name _ (list (list file upstream)) info expected) c)
      (call-with-upstream-files `((,file ,upstream))
        (lambda (files)
          (define mini (string->symbol (format "~a-mini" name)))
          (define fetch (hash-ref info "fetch"))
          (define entry
            (hash-set* info
                       "name" (symbol->string mini) "rows" (length expected)
                       "package" "datasets-fetch-fixture"
                       "sha256" (bytes->hex-string (sha256-bytes (rows->rktd-bytes expected)))
                       "fetch" (hash-set fetch "files"
                                         (list (hash "name" file
                                                     "urls" (list (url->string (path->url (hash-ref files file))))
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
