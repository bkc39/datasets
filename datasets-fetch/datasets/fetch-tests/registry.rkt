#lang racket/base
(require (only-in datasets/core dataset-info dataset-names)
         (only-in datasets/core-doc/catalog-rows catalog-column-rows)
         (only-in datasets/core-tests/fixture call-with-environment call-with-temporary-fixture)
         (only-in datasets/fetch-private/convert idx-file-names)
         (only-in datasets/fetch-tests/digest parity-reference)
         (only-in file/sha1 bytes->hex-string)
         (only-in racket/list drop make-list take)
         (only-in racket/match match match-define match-lambda)
         (only-in racket/string string-join)
         (only-in threading ~>))
(module+ test
  (require (only-in rackunit check-equal? check-false check-not-false check-regexp-match check-true
                    test-case))
  (define (indexed prefix n) (for/list ([i (in-range n)]) (format "~a-~a" prefix i)))
  (define image-columns
    (append (for/list ([k (in-range 1 785)]) (format "pixel~a" k)) '("label" "split")))
  (define (plane-columns . prefixes)
    (for*/list ([prefix (in-list prefixes)] [k (in-range 1 1025)]) (format "~a~a" prefix k)))
  (define (idx-files base shas)
    (for/list ([name (in-list idx-file-names)] [sha (in-list shas)])
      (list name (string-append base name) sha)))
  (define (toronto-files name sha) `((,name ,(string-append "https://www.cs.toronto.edu/~kriz/" name) ,sha)))
  (define (emnist-set name rows)
    `(,name ,rows ,image-columns
            (("gzip.zip" "https://biometrics.nist.gov/cs_links/EMNIST/gzip.zip"
                         "fb9bb67e33772a9cc0b895e4ecf36d2cf35be8b709693c3564cea2a019fcda8e"))
            ,(format "~a-rows" name) "NOASSERTION"))
  (define sets
    `((california-housing
       20640 ("med-inc" "house-age" "ave-rooms" "ave-bedrms" "population" "ave-occup" "latitude" "longitude"
              "med-house-val")
       (("cal_housing.tgz" "https://ndownloader.figshare.com/files/5976036"
                           "aaa5c9a6afe2225cc2aed2723682ae403280c4a3695a2ddda4ffb5d8215ea681"))
       "california-housing-rows" "NOASSERTION")
      (cifar-10
       60000 ,(append (plane-columns "red" "green" "blue") '("label" "split"))
       ,(toronto-files "cifar-10-binary.tar.gz"
                       "c4a38c50a1bc5f3a1c5537f2155ab9d68f9f25eb1ed8d9ddda3db29a59bca1dd")
       "cifar-10-rows" "NOASSERTION")
      (cifar-100
       60000 ,(append (plane-columns "red" "green" "blue") '("label" "coarse-label" "split"))
       ,(toronto-files "cifar-100-binary.tar.gz"
                       "58a81ae192c23a4be8b1804d68e518ed807d710a4eb253b1f2a199162a40d8ec")
       "cifar-100-rows" "NOASSERTION")
      (covtype
       581012 ,(append '("elevation" "aspect" "slope" "horizontal-distance-to-hydrology"
                         "vertical-distance-to-hydrology" "horizontal-distance-to-roadways" "hillshade-9am"
                         "hillshade-noon" "hillshade-3pm" "horizontal-distance-to-fire-points")
                       (indexed "wilderness-area" 4) (indexed "soil-type" 40) '("cover-type"))
       (("covtype.data.gz"
         "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz"
         "614360d0257557dd1792834a85a1cdebfadc3c4f30b011d56afee7ffb5b15771"))
       "covtype-rows" "CC-BY-4.0")
      ,(emnist-set 'emnist-balanced 131600)
      ,(emnist-set 'emnist-digits 280000)
      ,(emnist-set 'emnist-letters 145600)
      (fashion-mnist
       70000 ,image-columns
       ,(idx-files "http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/"
                   '("3aede38d61863908ad78613f6a32ed271626dd12800ba2636569512369268a84"
                     "a04f17134ac03560a47e3764e11b92fc97de4d1bfaf8ba1a3aa29af54cc90845"
                     "346e55b948d973a97e58d2351dde16a484bd415d4595297633bb08f03db6a073"
                     "67da17c76eaffca5446c3361aaab5c3cd6d1c2608764d35dfb1850b086bf8dd5"))
       "idx-image-rows" "MIT")
      (kmnist
       70000 ,image-columns
       ,(idx-files "https://codh.rois.ac.jp/kmnist/dataset/kmnist/"
                   '("51467d22d8cc72929e2a028a0428f2086b092bb31cfb79c69cc0a90ce135fde4"
                     "e38f9ebcd0f3ebcdec7fc8eabdcdaef93bb0df8ea12bee65224341c8183d8e17"
                     "edd7a857845ad6bb1d0ba43fe7e794d164fe2dce499a1694695a792adfac43c5"
                     "20bb9a0ef54c7db3efc55a92eef5582c109615df22683c380526788f98e42a1c"))
       "idx-image-rows" "CC-BY-SA-4.0")
      (mnist
       70000 ,image-columns
       ,(idx-files "https://ossci-datasets.s3.amazonaws.com/mnist/"
                   '("440fcabf73cc546fa21475e81ea370265605f56be210a4024d2ca8f203523609"
                     "3552534a0a558bbed6aed32b30c495cca23d567ec52cac8be1a0730e8010255c"
                     "8d422c7b0a1c1c79245a5bcf07fe86e33eeafee792b84584aec276f5a2dbc4e6"
                     "f7ae60f92e00ec6debd23a6088c31dbd2371eca3ffa0defaefb259924204aec6"))
       "idx-image-rows" "NOASSERTION")
      (nycflights13-flights
       336776 ("year" "month" "day" "dep-time" "sched-dep-time" "dep-delay" "arr-time" "sched-arr-time"
               "arr-delay" "carrier" "flight" "tailnum" "origin" "dest" "air-time" "distance" "hour"
               "minute" "time-hour")
       (("flights.csv"
         "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/cc03c29690889dbe83089f5206e2422db8c3f71f/csv/nycflights13/flights.csv"
         "b791a2590c9167aa0bd3cb45af3f64101180878cf088c183eae4011e3a654f42"))
       "csv-header-rows" "CC0-1.0")
      (zip
       9298 ,(append '("digit") (for/list ([k (in-range 1 257)]) (format "pixel~a" k)) '("split"))
       (("zip.train.gz" "https://hastie.su.domains/ElemStatLearn/datasets/zip.train.gz"
                        "66ebf231ab51f2cf6ac1a892d0f98ccffee1079d7a5db638c1f11f366046ca16")
        ("zip.test.gz" "https://hastie.su.domains/ElemStatLearn/datasets/zip.test.gz"
                       "8dacc3c6e136af6c3488de9e5113a7daf140c8c2f1f0b61c54ee02f03f1a962f"))
       "esl-zip-rows" "NOASSERTION")))
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
      (check-not-false (member license '("CC-BY-4.0" "CC-BY-SA-4.0" "MIT" "CC0-1.0" "NOASSERTION")))
      (check-equal? (hash-ref info "license") license)
      (check-regexp-match #px"Downloads [0-9.]+ MB" (hash-ref info "notes"))))

  (test-case "the catalog shows the pixel columns of the image sets as one row"
    (for ([name (in-list '(mnist fashion-mnist))])
      (check-equal? (map car (catalog-column-rows (hash-ref (dataset-info name) "columns")))
                    '("pixel1 … pixel784" "label" "split")
                    (symbol->string name)))
    (check-equal? (map car (catalog-column-rows (hash-ref (dataset-info 'zip) "columns")))
                  '("digit" "pixel1 … pixel256" "split"))
    (for ([name (in-list '(kmnist emnist-balanced emnist-letters emnist-digits))])
      (check-equal? (map car (catalog-column-rows (hash-ref (dataset-info name) "columns")))
                    '("pixel1 … pixel784" "label" "split")
                    (symbol->string name)))
    (check-equal? (map car (catalog-column-rows (hash-ref (dataset-info 'cifar-10) "columns")))
                  '("red1 … red1024" "green1 … green1024" "blue1 … blue1024" "label" "split"))
    (check-equal? (map car (catalog-column-rows (hash-ref (dataset-info 'cifar-100) "columns")))
                  '("red1 … red1024" "green1 … green1024" "blue1 … blue1024" "label" "coarse-label" "split")))

  (define pixel-rx #px"^(pixel|red|green|blue)([0-9]+)$")
  (define (pixel-run names size)
    (match (map (lambda (name) (regexp-match pixel-rx name)) names)
      [(list (list _ (and prefix (or "red" "pixel")) _) _ ...)
       (for*/list ([p (in-list (if (equal? prefix "red") '("red" "green" "blue") '("pixel")))]
                   [k (in-range 1 (add1 (quotient size (if (equal? prefix "red") 3 1))))])
         (format "~a~a" p k))]
      [_ #f]))

  (test-case "image-shape: C x H x W pixel columns, one contiguous run in plane then row-major order"
    (define shaped
      (for/list ([name (in-list (dataset-names #:package "datasets-fetch"))]
                 #:when (hash-has-key? (dataset-info name) "image-shape"))
        name))
    (check-equal? shaped '(cifar-10 cifar-100 emnist-balanced emnist-digits emnist-letters fashion-mnist kmnist
                                    mnist zip))
    (for ([name (in-list shaped)])
      (define info (dataset-info name))
      (match-define (list c h w) (hash-ref info "image-shape"))
      (define names (map (lambda (column) (hash-ref column "name")) (hash-ref info "columns")))
      (define start (for/first ([n (in-list names)] [i (in-naturals)] #:when (regexp-match? pixel-rx n)) i))
      (define size (* c h w))
      (check-equal? (length (filter (lambda (n) (regexp-match? pixel-rx n)) names)) size (symbol->string name))
      (check-equal? (take (drop names start) size) (pixel-run (drop names start) size) (symbol->string name))
      (check-equal? start (if (eq? name 'zip) 1 0) (symbol->string name))))

  (test-case "parity.json pins each image set's rows and canonical text as the registry does"
    (for ([name (in-list '(cifar-10 cifar-100 kmnist emnist-balanced emnist-letters emnist-digits))])
      (define info (dataset-info name))
      (define reference (parity-reference name))
      (check-equal? (list (hash-ref reference 'rows) (hash-ref reference 'canonical-sha256))
                    (list (hash-ref info "rows") (hash-ref info "sha256"))
                    (symbol->string name))))

  (test-case "zip types: digit integer, pixels real, split categorical train then test"
    (check-equal? (for/list ([column (in-list (hash-ref (dataset-info 'zip) "columns"))])
                    (list (hash-ref column "type") (hash-ref column "levels")))
                  `(("integer" ()) ,@(make-list 256 '("real" ())) ("categorical" ("train" "test")))))

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
                                    (dataset-info 'zip)
                                    (dataset-cached? 'zip)
                                    (dataset-info 'cifar-10)
                                    (dataset-cached? 'emnist-balanced)
                                    (with-handlers ([exn:fail? exn-message])
                                      (load-dataset-table 'covtype #:download? #f))))])
               (eval form))
             (check-regexp-match #rx"downloads are disabled"
                                 (eval '(with-handlers ([exn:fail? exn-message])
                                          (load-dataset-table 'california-housing #:download? #f))))
             (check-true (module-declared? 'datasets/core #f))
             (check-false (module-declared? 'datasets/fetch-private/convert #f)))))))))
