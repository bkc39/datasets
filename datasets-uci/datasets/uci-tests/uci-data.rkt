#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-missing dataset-missing? dataset-names
                  dataset-packages dataset-table-column dataset-table-columns dataset-table-names
                  dataset-table-row-count load-dataset-table)
         (only-in datasets/core-tests/fixture gunzip-file hex)
         (only-in racket/file file->string)
         (only-in racket/list check-duplicates drop filter-map filter-not first remove-duplicates
                  take)
         (only-in racket/match match match-define)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/string string-contains? string-prefix?)
         (only-in racket/vector vector-count vector-map)
         (only-in setup/getinfo get-info/full))
(define-runtime-path uci-data "../uci-data")
(define-runtime-path package-root "../..")
(module+ test
  (require (only-in rackunit check-= check-equal? check-false check-not-false check-regexp-match
                    check-true)
           (only-in threading ~>))
  (define pixels (for*/list ([i 8] [j 8]) (format "pixel-~a-~a" i j)))
  (define spam-words
    '("make" "address" "all" "3d" "our" "over" "remove" "internet" "order" "mail" "receive" "will"
      "people" "report" "addresses" "free" "business" "email" "you" "credit" "your" "font" "000"
      "money" "hp" "hpl" "george" "650" "lab" "labs" "telnet" "857" "data" "415" "85" "technology"
      "1999" "parts" "pm" "direct" "cs" "meeting" "original" "project" "re" "edu" "table"
      "conference"))
  (define bike-daily-names
    '("instant" "dteday" "season" "yr" "mnth" "holiday" "weekday" "workingday" "weathersit" "temp"
      "atemp" "hum" "windspeed" "casual" "registered" "cnt"))
  (define shapes
    `((abalone 4177 ("sex" "length" "diameter" "height" "whole-weight" "shucked-weight"
                     "viscera-weight" "shell-weight" "rings"))
      (adult 48842 ("age" "workclass" "fnlwgt" "education" "education-num" "marital-status"
                    "occupation" "relationship" "race" "sex" "capital-gain" "capital-loss"
                    "hours-per-week" "native-country" "income" "split"))
      (auto-mpg 398 ("mpg" "cylinders" "displacement" "horsepower" "weight" "acceleration"
                     "model-year" "origin" "car-name"))
      (bank-marketing 41188 ("age" "job" "marital" "education" "default" "housing" "loan" "contact"
                             "month" "day-of-week" "duration" "campaign" "pdays" "previous"
                             "poutcome" "emp-var-rate" "cons-price-idx" "cons-conf-idx"
                             "euribor3m" "nr-employed" "y"))
      (banknote-authentication 1372 ("variance" "skewness" "curtosis" "entropy" "class"))
      (bike-sharing-daily 731 ,bike-daily-names)
      (bike-sharing-hourly 17379 ,(append (take bike-daily-names 5) '("hr")
                                          (drop bike-daily-names 5)))
      (car-evaluation 1728 ("buying" "maint" "doors" "persons" "lug-boot" "safety" "class"))
      (concrete-strength 1030 ("cement" "blast-furnace-slag" "fly-ash" "water" "superplasticizer"
                               "coarse-aggregate" "fine-aggregate" "age"
                               "concrete-compressive-strength"))
      (digits 1797 ,(append pixels '("digit")))
      (energy-efficiency 768 ("relative-compactness" "surface-area" "wall-area" "roof-area"
                              "overall-height" "orientation" "glazing-area"
                              "glazing-area-distribution" "heating-load" "cooling-load"))
      (heart-disease 303 ("age" "sex" "cp" "trestbps" "chol" "fbs" "restecg" "thalach" "exang"
                          "oldpeak" "slope" "ca" "thal" "num"))
      (mushroom 8124 ("class" "cap-shape" "cap-surface" "cap-color" "bruises" "odor"
                      "gill-attachment" "gill-spacing" "gill-size" "gill-color" "stalk-shape"
                      "stalk-root" "stalk-surface-above-ring" "stalk-surface-below-ring"
                      "stalk-color-above-ring" "stalk-color-below-ring" "veil-type" "veil-color"
                      "ring-number" "ring-type" "spore-print-color" "population" "habitat"))
      (optdigits 5620 ,(append pixels '("digit" "split")))
      (seeds 210 ("area" "perimeter" "compactness" "length-of-kernel" "width-of-kernel"
                  "asymmetry-coefficient" "length-of-kernel-groove" "class"))
      (spambase 4601 ,(append (map (lambda (w) (string-append "word-freq-" w)) spam-words)
                              (map (lambda (c) (string-append "char-freq-" c))
                                   '("semicolon" "paren" "bracket" "exclamation" "dollar" "hash"))
                              '("capital-run-length-average" "capital-run-length-longest"
                                "capital-run-length-total" "spam")))
      (student-performance 1044 ("school" "sex" "age" "address" "famsize" "pstatus" "medu" "fedu"
                                 "mjob" "fjob" "reason" "guardian" "traveltime" "studytime"
                                 "failures" "schoolsup" "famsup" "paid" "activities" "nursery"
                                 "higher" "internet" "romantic" "famrel" "freetime" "goout" "dalc"
                                 "walc" "health" "absences" "g1" "g2" "g3" "subject"))
      (wine-quality 6497 ("fixed-acidity" "volatile-acidity" "citric-acid" "residual-sugar"
                          "chlorides" "free-sulfur-dioxide" "total-sulfur-dioxide" "density" "ph"
                          "sulphates" "alcohol" "quality" "colour"))))
  (define ids (map first shapes))
  (define bike-note
    (regexp
     (string-append "casual \\+ registered = cnt, so leave casual and registered out when "
                    "predicting cnt\\. Descriptions follow the UCI dataset page, which corrects "
                    "the bundled Readme\\.txt \\(season 1:springer, temp divided by 41, atemp by "
                    "50\\); season 1 is winter")))
  (define citation-pattern
    #px"doi:10\\.24432/C5[0-9A-Z]{4}\\. https://archive\\.ics\\.uci\\.edu/dataset/[0-9]+/")
  (define column-metas
    (for/hash ([id (in-list ids)])
      (values id (for/hash ([meta (in-list (hash-ref (dataset-info id) "columns"))])
                   (values (hash-ref meta "name") meta)))))
  (define (data-file id) (build-path uci-data "data" (format "~a.rktd.gz" id)))
  (define tables (for/hash ([id (in-list ids)]) (values id (load-dataset-table id))))
  (define (column id name) (dataset-table-column (hash-ref tables id) name))
  (define (column-meta id name key) (~> column-metas (hash-ref id) (hash-ref name) (hash-ref key)))
  (define (column-count id name keep?) (vector-count keep? (column id name)))
  (define (column-sum id name)
    (for/sum ([v (in-vector (column id name))] #:unless (dataset-missing? v)) v))
  (define (column-type name levels identifiers present)
    (cond
      [(member name identifiers) "identifier"]
      [(pair? levels) "categorical"]
      [(andmap exact-integer? present) "integer"]
      [(andmap real? present) "real"]
      [else "string"]))
  (define (typed-columns id type)
    (filter-map (lambda (meta) (and (equal? (hash-ref meta "type") type) (hash-ref meta "name")))
                (hash-ref (dataset-info id) "columns")))
  (define spot-checks
    `((categorical abalone ("sex"))
      (categorical adult ("workclass" "education" "marital-status" "occupation" "relationship" "race"
                          "sex" "native-country" "income" "split"))
      (categorical auto-mpg ())
      (categorical bank-marketing ("job" "marital" "education" "default" "housing" "loan" "contact"
                                   "month" "day-of-week" "poutcome" "y"))
      (categorical car-evaluation ("buying" "maint" "doors" "persons" "lug-boot" "safety" "class"))
      (categorical heart-disease ())
      (categorical optdigits ("split"))
      (categorical student-performance ("school" "sex" "address" "famsize" "pstatus" "mjob" "fjob"
                                        "reason" "guardian" "schoolsup" "famsup" "paid"
                                        "activities" "nursery" "higher" "internet" "romantic"
                                        "subject"))
      (categorical wine-quality ("colour"))
      (identifiers auto-mpg ())
      (meta auto-mpg "car-name" "type" "string")
      (identifiers bike-sharing-daily ("instant"))
      (identifiers bike-sharing-hourly ("instant"))
      (identifiers digits ())
      (row digits 0 ("pixel-0-2" "pixel-0-3" "pixel-0-4" "digit") (5 13 9 0))
      (row digits 1796 ("pixel-7-4" "pixel-7-5" "pixel-7-6" "pixel-7-7" "digit") (14 12 1 0 8))
      (sum digits "digit" 8070)
      (sum digits "pixel-3-4" 17839)
      (meta digits "pixel-0-0" "original-name" "pixel_0_0")
      (meta digits "digit" "original-name" "class")
      (info digits "suggested-targets" ("digit"))
      (row optdigits 0 ("pixel-0-1" "pixel-0-2" "pixel-0-3" "digit" "split") (1 6 15 0 "train"))
      (row optdigits 3823 ("pixel-0-2" "digit" "split") (5 0 "test"))
      (level-counts optdigits "split" (3823 1797))
      (sum optdigits "digit" 25263)
      (row adult 0 ("age" "workclass" "income" "split") (39 "State-gov" "<=50K" "train"))
      (row adult 32561 ("age" "fnlwgt" "occupation" "income" "split")
           (25 226802 "Machine-op-inspct" "<=50K" "test"))
      (row adult 48841 ("hours-per-week" "income" "split") (60 ">50K" "test"))
      (level-counts adult "split" (32561 16281))
      (level-counts adult "income" (37155 11687))
      (missing adult ("workclass" "occupation" "native-country") (2799 2809 857))
      (sum adult "age" 1887430)
      (sum adult "fnlwgt" 9263575662)
      (info adult "suggested-targets" ("income"))
      (row auto-mpg 0 ("mpg" "cylinders" "horsepower" "weight" "car-name")
           (18.0 8 130 3504 "chevrolet chevelle malibu"))
      (row auto-mpg 32 ("horsepower") (,dataset-missing))
      (missing auto-mpg ("horsepower" "mpg") (6 0))
      (sum auto-mpg "weight" 1182229)
      (sum auto-mpg "mpg" 9358.8 1e-9)
      (row abalone 0 ("sex" "length" "rings") ("M" 0.455 15))
      (level-counts abalone "sex" (1528 1307 1342))
      (sum abalone "rings" 41493)
      (meta abalone "length" "unit" "mm / 200")
      (row wine-quality 0 ("fixed-acidity" "ph" "quality" "colour") (7.4 3.51 5 "red"))
      (row wine-quality 6496 ("density" "quality" "colour") (0.98941 6 "white"))
      (level-counts wine-quality "colour" (1599 4898))
      (sum wine-quality "quality" 37802)
      (meta wine-quality "ph" "original-name" "pH")
      (row bank-marketing 0 ("age" "job" "month" "duration" "pdays" "nr-employed" "y")
           (56 "housemaid" "may" 261 999 5191.0 "no"))
      (row bank-marketing 41187 ("job" "education" "emp-var-rate" "y")
           ("retired" "professional.course" -1.1 "no"))
      (missing bank-marketing ("job" "marital" "education" "default" "housing" "loan" "contact")
               (330 80 1731 8597 990 990 0))
      (level-counts bank-marketing "y" (36548 4640))
      (sum bank-marketing "duration" 10638243)
      (meta bank-marketing "default" "levels" ("no" "yes"))
      (meta bank-marketing "day-of-week" "original-name" "day_of_week")
      (row heart-disease 0 ,(map symbol->string '(age sex cp trestbps chol fbs restecg thalach exang
                                                   oldpeak slope ca thal num))
           (63 1 1 145 233 1 2 150 0 2.3 3 0 6 0))
      (row heart-disease 166 ("ca") (,dataset-missing))
      (missing heart-disease ("ca" "thal") (4 2))
      (sum heart-disease "num" 284)
      (sum heart-disease "chol" 74748)
      (row mushroom 0 ("class" "cap-shape" "odor" "stalk-root" "habitat") ("p" "x" "p" "e" "u"))
      (row mushroom 8123 ("class" "stalk-root" "habitat") ("e" ,dataset-missing "l"))
      (level-counts mushroom "class" (4208 3916))
      (level-counts mushroom "odor" (400 400 192 576 2160 36 3528 256 576))
      (missing mushroom ("stalk-root") (2480))
      (meta mushroom "bruises" "original-name" "bruises?")
      (meta mushroom "stalk-root" "levels" ("b" "c" "e" "r"))
      (info-match mushroom "notes"
                  #rx"Donated by J\\. Schlimmer \\(1987\\); the UCI page names no creator")
      (row car-evaluation 0 ("buying" "doors" "class") ("vhigh" "2" "unacc"))
      (row car-evaluation 1727 ("buying" "doors" "persons" "class") ("low" "5more" "more" "vgood"))
      (level-counts car-evaluation "class" (1210 384 69 65))
      (sum spambase "spam" 1813)
      (sum spambase "capital-run-length-total" 1303414)
      (sum spambase "char-freq-exclamation" 1237.995 1e-9)
      (meta spambase "char-freq-semicolon" "original-name" "char_freq_;")
      (row banknote-authentication 1371 ("variance" "skewness" "curtosis" "entropy" "class")
           (-2.5419 -0.65804 2.6842 1.1952 1))
      (sum banknote-authentication "class" 610)
      (row seeds 0 ("area" "compactness" "class") (15.26 0.871 1))
      (sum seeds "class" 420)
      (sum seeds "area" 3117.98 1e-9)
      (row concrete-strength 0 ("cement" "water" "age" "concrete-compressive-strength")
           (540.0 162.0 28 79.98611076))
      (sum concrete-strength "age" 47032)
      (meta concrete-strength "age" "unit" "days")
      (meta concrete-strength "water" "original-name" "Water  (component 4)(kg in a m^3 mixture)")
      (meta concrete-strength "concrete-compressive-strength" "original-name"
            "Concrete compressive strength(MPa, megapascals) ")
      (row energy-efficiency 0 ("relative-compactness" "surface-area" "orientation" "heating-load"
                                "cooling-load")
           (0.98 514.5 2 15.55 21.33))
      (meta energy-efficiency "heating-load" "original-name" "Y1")
      (info energy-efficiency "suggested-targets" ("heating-load" "cooling-load"))
      (info-match energy-efficiency "notes"
                  #rx"no documentation file; names X1-X8, Y1 and Y2 are replaced by the names on")
      (row student-performance 0 ("school" "sex" "age" "g1" "g3" "subject") ("GP" "F" 18 5 6 "math"))
      (row student-performance 1043 ("school" "absences" "g3" "subject") ("MS" 4 11 "portuguese"))
      (level-counts student-performance "subject" (395 649))
      (sum student-performance "g3" 11841)
      (sum student-performance "absences" 4630)
      (row bike-sharing-daily 0 ("instant" "dteday" "season" "temp" "cnt")
           (1 "2011-01-01" 1 0.344167 985))
      (total bike-sharing-daily ("casual" "registered") "cnt")
      (total bike-sharing-hourly ("casual" "registered") "cnt")
      (info-match bike-sharing-daily "notes" ,bike-note)
      (info-match bike-sharing-hourly "notes" ,bike-note)
      (sum bike-sharing-daily "cnt" 3292679)
      (row bike-sharing-hourly 17378 ("instant" "dteday" "hr" "cnt") (17379 "2012-12-31" 23 49))
      (sum bike-sharing-hourly "cnt" 3292679)
      (info bike-sharing-hourly "suggested-targets" ("cnt"))
      (citation abalone "Nash, W., Sellers, T., Talbot, S., Cawthorn, A. and Ford, W. (1994). "
                "C55C7W")
      (citation adult "Becker, B. and Kohavi, R. (1996). " "C5XW20")
      (citation auto-mpg "Quinlan, R. (1993). " "C5859H")
      (citation bank-marketing "Moro, S., Rita, P. and Cortez, P. (2014). " "C5K306")
      (citation banknote-authentication "Lohweg, V. (2012). " "C55P57")
      (citation bike-sharing-daily "Fanaee-T, H. (2013). " "C5W894")
      (citation bike-sharing-hourly "Fanaee-T, H. (2013). " "C5W894")
      (citation car-evaluation "Bohanec, M. (1988). " "C5JP48")
      (citation concrete-strength "Yeh, I.-C. (1998). " "C5PK67")
      (citation digits "Alpaydin, E. and Kaynak, C. (1998). " "C50P49")
      (citation energy-efficiency "Tsanas, A. and Xifara, A. (2012). " "C51307")
      (citation heart-disease "Janosi, A., Steinbrunn, W., Pfisterer, M. and Detrano, R. (1989). "
                "C52P4X")
      (citation mushroom "Mushroom [Dataset]. (1981). " "C5959T")
      (citation optdigits "Alpaydin, E. and Kaynak, C. (1998). " "C50P49")
      (citation seeds ,(string-append "Charytanowicz, M., Niewczas, J., Kulczycki, P., Kowalski, P. "
                                      "and Lukasik, S. (2010). ")
                "C5H30K")
      (citation spambase "Hopkins, M., Reeber, E., Forman, G. and Suermondt, J. (1999). " "C53G6X")
      (citation student-performance "Cortez, P. (2008). " "C5TG7T")
      (citation wine-quality "Cortez, P., Cerdeira, A., Almeida, F., Matos, T. and Reis, J. (2009). "
                "C56S3T")))

  (check-equal? (dataset-names #:package "datasets-uci") ids)
  (check-not-false (member "datasets-uci" (dataset-packages)))
  (for ([shape (in-list shapes)])
    (match-define (list id rows names) shape)
    (define label (symbol->string id))
    (define t (hash-ref tables id))
    (define info (dataset-info id))
    (check-equal? (dataset-table-row-count t) rows label)
    (check-equal? (dataset-table-names t) names label)
    (check-equal? (hash-ref info "rows") rows)
    (check-equal? (hash-ref info "column-count") (length names))
    (check-equal? (hash-ref info "tier") "bundled")
    (check-equal? (hash-ref info "package") "datasets-uci")
    (check-false (hash-ref info "fetch"))
    (check-equal? (hash-ref info "license") "CC-BY-4.0")
    (check-true (regexp-match? citation-pattern (hash-ref info "citation")) label)
    (check-equal? (~> id data-file gunzip-file sha256-bytes hex) (hash-ref info "sha256"))
    (define header (call-with-input-file (data-file id) (lambda (in) (read-bytes 8 in))))
    (check-equal? (subbytes header 0 4) (bytes #x1f #x8b 8 0))
    (check-equal? (subbytes header 4 8) (bytes 0 0 0 0))
    (check-true (immutable? info))
    (check-true (pair? (hash-ref info "suggested-targets")) label)
    (for ([name (in-list (hash-ref info "identifiers"))])
      (check-false (~> (column id name) vector->list check-duplicates) (format "~a ~a" label name)))
    (for ([target (in-list (hash-ref info "suggested-targets"))])
      (check-not-false (member target names) label))
    (for ([c (in-vector (dataset-table-columns t))] [meta (in-list (hash-ref info "columns"))])
      (define name (hash-ref meta "name"))
      (define present (filter-not dataset-missing? (vector->list c)))
      (check-true (immutable? c))
      (check-equal? (vector-length c) rows)
      (check-equal? (vector-count dataset-missing? c) (hash-ref meta "missing-count"))
      (check-equal? (hash-ref meta "type")
                    (column-type name (hash-ref meta "levels") (hash-ref info "identifiers") present)
                    name)
      (check-true (or (andmap string? present)
                      (not (member (hash-ref meta "type") '("categorical" "string"))))
                  name)
      (check-true (or (andmap exact-integer? present) (andmap flonum? present)
                      (not (member (hash-ref meta "type") '("integer" "real"))))
                  name))
    (for ([c (in-vector (dataset-table-columns t))] [meta (in-list (hash-ref info "columns"))]
          #:when (pair? (hash-ref meta "levels")))
      (check-equal? (~> c vector->list (filter-not dataset-missing? _) remove-duplicates
                        (sort string<?))
                    (sort (hash-ref meta "levels") string<?)
                    (hash-ref meta "name"))))
  (check-equal? (~> (build-path uci-data "data") directory-list (map path->string _) (sort string<?))
                (~> ids (map (lambda (id) (format "~a.rktd.gz" id)) _) (sort string<?)))
  (match-define (list _ 'AND data-license) ((get-info/full package-root) 'license))
  (check-equal? data-license 'CC-BY-4.0)
  (check-equal? (~> (build-path uci-data "licenses") directory-list (map path->string _))
                '("CC-BY-4.0.txt"))
  (check-regexp-match #rx"- mushroom: Mushroom \\(1981\\), doi:10\\.24432/C5959T, no creator named"
                      (file->string (build-path package-root "NOTICE")))

  (for ([spot (in-list spot-checks)])
    (match spot
      [(list 'row id row names expected)
       (check-equal? (map (lambda (name) (vector-ref (column id name) row)) names) expected
                     (format "~a row ~a" id row))]
      [(list 'missing id names expected)
       (check-equal? (map (lambda (name) (column-count id name dataset-missing?)) names) expected)]
      [(list 'total id parts whole)
       (check-equal? (apply vector-map + (map (lambda (name) (column id name)) parts))
                     (column id whole))]
      [(list 'sum id name expected) (check-equal? (column-sum id name) expected)]
      [(list 'sum id name expected epsilon) (check-= (column-sum id name) expected epsilon)]
      [(list 'level-counts id name expected)
       (check-equal? (map (lambda (level) (column-count id name (lambda (v) (equal? v level))))
                          (column-meta id name "levels"))
                     expected)]
      [(list 'categorical id expected) (check-equal? (typed-columns id "categorical") expected)]
      [(list 'identifiers id expected)
       (check-equal? (hash-ref (dataset-info id) "identifiers") expected)
       (check-equal? (typed-columns id "identifier") expected)]
      [(list 'meta id name key expected) (check-equal? (column-meta id name key) expected)]
      [(list 'info id key expected) (check-equal? (hash-ref (dataset-info id) key) expected)]
      [(list 'info-match id key pattern)
       (check-regexp-match pattern (hash-ref (dataset-info id) key))]
      [(list 'citation id authors-year doi-suffix)
       (define citation (hash-ref (dataset-info id) "citation"))
       (check-true (string-prefix? citation authors-year) (symbol->string id))
       (check-true (string-contains? citation (format "doi:10.24432/~a. " doi-suffix))
                   (symbol->string id))])))
