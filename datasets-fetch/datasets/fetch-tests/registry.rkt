#lang racket/base
(require (only-in datasets/core dataset-info dataset-names)
         (only-in datasets/core-tests/fixture call-with-environment call-with-temporary-fixture)
         (only-in file/sha1 bytes->hex-string)
         (only-in racket/match match-define match-lambda)
         (only-in racket/string string-join)
         (only-in threading ~>))
(module+ test
  (require (only-in rackunit check-equal? check-false check-not-false check-regexp-match check-true
                    test-case))
  (define (indexed prefix n) (for/list ([i (in-range n)]) (format "~a-~a" prefix i)))
  (define sets
    `((california-housing
       20640 ("med-inc" "house-age" "ave-rooms" "ave-bedrms" "population" "ave-occup" "latitude" "longitude"
              "med-house-val")
       (("cal_housing.tgz" "https://ndownloader.figshare.com/files/5976036"
                           "aaa5c9a6afe2225cc2aed2723682ae403280c4a3695a2ddda4ffb5d8215ea681"))
       "california-housing-rows" "NOASSERTION")
      (covtype
       581012 ,(append '("elevation" "aspect" "slope" "horizontal-distance-to-hydrology"
                         "vertical-distance-to-hydrology" "horizontal-distance-to-roadways" "hillshade-9am"
                         "hillshade-noon" "hillshade-3pm" "horizontal-distance-to-fire-points")
                       (indexed "wilderness-area" 4) (indexed "soil-type" 40) '("cover-type"))
       (("covtype.data.gz"
         "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz"
         "614360d0257557dd1792834a85a1cdebfadc3c4f30b011d56afee7ffb5b15771"))
       "covtype-rows" "CC-BY-4.0")))
  (define (sha256-hex s) (~> s string->bytes/utf-8 sha256-bytes bytes->hex-string))

  (test-case "the datasets-fetch registry: shapes, pins, converters, terms"
    (check-equal? (dataset-names #:package "datasets-fetch") (map car sets))
    (for ([s (in-list sets)])
      (match-define (list name rows columns files binding license) s)
      (define info (dataset-info name))
      (define fetch (hash-ref info "fetch"))
      (check-equal? (map (lambda (k) (hash-ref info k)) '("tier" "package" "rows" "column-count"))
                    (list "fetch" "datasets-fetch" rows (length columns)))
      (check-equal? (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")) columns)
      (check-equal? (hash-ref fetch "format") "convert")
      (check-equal? (hash-ref fetch "converter")
                    (hash "module" "datasets/fetch-private/convert" "binding" binding))
      (check-equal? (map (match-lambda
                           [(hash-table ("name" name) ("urls" (list* url _)) ("sha256" sha))
                            (list name url sha)])
                         (hash-ref fetch "files"))
                    files)
      (check-equal? (hash-ref info "source-sha256")
                    (~> (map (match-lambda [(list _ _ sha) sha]) files) (string-join "\n") sha256-hex))
      (check-not-false (member license '("CC-BY-4.0" "MIT" "CC0-1.0" "NOASSERTION")))
      (check-equal? (hash-ref info "license") license)
      (check-regexp-match #px"Downloads [0-9.]+ MB" (hash-ref info "notes"))))

  (test-case "queries load no converter"
    (call-with-temporary-fixture
     (lambda (home)
       (call-with-environment (hash "DATASETS_HOME" (path->string home))
         (lambda ()
           (parameterize ([current-namespace (make-base-namespace)])
             (namespace-require 'datasets/core)
             (for ([form (in-list '((dataset-names)
                                    (dataset-info 'covtype)
                                    (dataset-cached? 'covtype)
                                    (with-handlers ([exn:fail? exn-message])
                                      (load-dataset-table 'covtype #:download? #f))))])
               (eval form))
             (check-regexp-match #rx"downloads are disabled"
                                 (eval '(with-handlers ([exn:fail? exn-message])
                                          (load-dataset-table 'california-housing #:download? #f))))
             (check-true (module-declared? 'datasets/core #f))
             (check-false (module-declared? 'datasets/fetch-private/convert #f)))))))))
