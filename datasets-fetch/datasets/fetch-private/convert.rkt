#lang racket/base
(require (only-in datasets/fetch-private/archive gunzip-bytes gunzip-file tar-gz-member tar-gz-members
                  zip-members)
         (only-in datasets/fetch-private/csv csv-rows info->column-specs)
         (only-in datasets/fetch-private/idx read-idx)
         (only-in racket/contract/base -> and/c contract-out flat-named-contract hash/c integer-in listof)
         (only-in racket/file file->bytes)
         (only-in racket/flonum fl/)
         (only-in racket/match match match* match-define)
         (only-in racket/sequence sequence-append sequence-map)
         (only-in racket/stream for/stream)
         (only-in racket/string string-split)
         (only-in threading ~>))
(provide (contract-out
          [covtype-rows converter/c]
          [california-housing-rows converter/c]
          [idx-file-names (listof string?)]
          [idx-image-rows converter/c]
          [cifar-10-rows converter/c]
          [cifar-100-rows converter/c]
          [emnist-balanced-rows converter/c]
          [emnist-letters-rows converter/c]
          [emnist-digits-rows converter/c]
          [csv-header-rows converter/c]
          [esl-zip-file-names (listof string?)]
          [esl-zip-rows converter/c]))

(define converter-files/c
  (flat-named-contract 'converter-files (hash/c string? complete-path? #:immutable #t)))
(define converter-info/c (flat-named-contract 'dataset-info (and/c hash? immutable?)))
(define converter/c (-> converter-files/c converter-info/c sequence?))

(define (covtype-rows files info)
  (~> (hash-ref files "covtype.data.gz")
      gunzip-file
      open-input-bytes
      (csv-rows (info->column-specs info) #:header? #f)))

(define cal-housing-specs
  (map (lambda (name) (list name 'real))
       '("longitude" "latitude" "housingMedianAge" "totalRooms" "totalBedrooms" "population" "households"
         "medianIncome" "medianHouseValue")))

(define (california-housing-rows files info)
  (~> (hash-ref files "cal_housing.tgz")
      (tar-gz-member "CaliforniaHousing/cal_housing.data")
      open-input-bytes
      (csv-rows cal-housing-specs #:header? #f)
      (sequence-map california-housing-row _)))

(define (california-housing-row row)
  (match-define (list longitude latitude age rooms bedrooms population households income value) row)
  (list income age (fl/ rooms households) (fl/ bedrooms households) population
        (fl/ population households) latitude longitude (fl/ value 100000.0)))

(define idx-file-names
  '("train-images-idx3-ubyte.gz" "train-labels-idx1-ubyte.gz" "t10k-images-idx3-ubyte.gz"
    "t10k-labels-idx1-ubyte.gz"))

(define (idx-image-rows files info)
  (idx-rows (lambda (name) (idx-file files name)) idx-file-names (- (hash-ref info "column-count") 2)))

(define (idx-rows read-part parts width)
  (match-define (list train-images train-labels test-images test-labels) parts)
  (sequence-append (idx-split-rows read-part train-images train-labels "train" width)
                   (idx-split-rows read-part test-images test-labels "test" width)))

(define (idx-split-rows read-part images-part labels-part split width)
  (define-values (image-dims images) (read-part images-part))
  (define-values (label-dims labels) (read-part labels-part))
  (sequence-map (lambda (i) (idx-row images labels i width split))
                (in-range (idx-image-count image-dims label-dims split width))))

(define (idx-image-count image-dims label-dims split width)
  (define (fail message details . args)
    (apply error 'idx-image-rows (string-append "~a\n  split: ~a" details) message split args))
  (match* (image-dims label-dims)
    [((list n _ _) (list m)) #:when (not (= n m))
     (fail "image and label counts differ" "\n  images: ~a\n  labels: ~a" n m)]
    [((list _ h w) (list _)) #:when (not (= (* h w) width))
     (fail "image size does not match the column count" "\n  expected pixels: ~a\n  given: ~ax~a" width h w)]
    [((list n _ _) (list _)) n]
    [(_ _)
     (fail "expected 3-dimensional images and 1-dimensional labels" "\n  images: ~a\n  labels: ~a"
           image-dims label-dims)]))

(define (idx-row images labels i width split)
  (~> images
      (subbytes (* i width) (* (add1 i) width))
      bytes->list
      (append _ (list (bytes-ref labels i) split))))

(define (idx-file files name)
  (~> (hash-ref files name) gunzip-file open-input-bytes read-idx))

(define (image-width who info extra-columns)
  (define shape (hash-ref info "image-shape"))
  (define width (apply * shape))
  (define count (hash-ref info "column-count"))
  (cond
    [(= count (+ width extra-columns)) width]
    [else (error who "image size does not match the column count\n  image-shape: ~a\n  column-count: ~a"
                 shape count)]))

(define ((emnist-rows split) files info)
  (define width (image-width 'idx-image-rows info 2))
  (define members
    (zip-members (hash-ref files "gzip.zip")
                 (map (lambda (part) (format "gzip/emnist-~a-~a-ubyte.gz" split part))
                      '("train-images-idx3" "train-labels-idx1" "test-images-idx3" "test-labels-idx1"))))
  (idx-rows (lambda (data) (~> data gunzip-bytes open-input-bytes read-idx)) members width))

(define emnist-balanced-rows (emnist-rows "balanced"))
(define emnist-letters-rows (emnist-rows "letters"))
(define emnist-digits-rows (emnist-rows "digits"))

(struct cifar-member (name split))
(struct cifar-label (offset classes name))

(define cifar-10-members
  (append (for/list ([k (in-range 1 6)])
            (cifar-member (format "cifar-10-batches-bin/data_batch_~a.bin" k) "train"))
          (list (cifar-member "cifar-10-batches-bin/test_batch.bin" "test"))))
(define cifar-100-members
  (list (cifar-member "cifar-100-binary/train.bin" "train") (cifar-member "cifar-100-binary/test.bin" "test")))

(define (cifar-10-rows files info)
  (cifar-rows 'cifar-10-rows (hash-ref files "cifar-10-binary.tar.gz") info
              #:members cifar-10-members
              #:labels (list (cifar-label 0 10 "label"))))

(define (cifar-100-rows files info)
  (cifar-rows 'cifar-100-rows (hash-ref files "cifar-100-binary.tar.gz") info
              #:members cifar-100-members
              #:labels (list (cifar-label 1 100 "label") (cifar-label 0 20 "coarse label"))))

(define (cifar-rows who archive info #:members members #:labels labels)
  (define width (image-width who info (add1 (length labels))))
  (define record-bytes (+ (length labels) width))
  (apply sequence-append
         (map (lambda (member data) (cifar-member-rows who member data record-bytes labels))
              members
              (tar-gz-members archive (map cifar-member-name members)))))

(define (cifar-member-rows who member data record-bytes labels)
  (match-define (cifar-member name split) member)
  (sequence-map (lambda (record) (cifar-row who name data record record-bytes labels split))
                (in-range (cifar-record-count who name data record-bytes))))

(define (cifar-record-count who member data record-bytes)
  (define size (bytes-length data))
  (cond
    [(and (positive? size) (zero? (remainder size record-bytes))) (quotient size record-bytes)]
    [else (error who "member is not a whole number of records\n  member: ~a\n  record bytes: ~a\n  given bytes: ~a"
                 member record-bytes size)]))

(define (cifar-row who member data record record-bytes labels split)
  (define start (* record record-bytes))
  (define label-values
    (for/list ([label (in-list labels)])
      (match-define (cifar-label offset classes name) label)
      (define value (bytes-ref data (+ start offset)))
      (cond
        [(< value classes) value]
        [else (error who "~a out of range\n  member: ~a\n  record: ~a\n  label: ~a\n  expected: 0 to ~a"
                     name member (add1 record) value (sub1 classes))])))
  (~> data
      (subbytes (+ start (length labels)) (+ start record-bytes))
      bytes->list
      (append _ label-values (list split))))

(define (csv-header-rows files info)
  (match (hash-values files)
    [(list path) (~> path file->bytes open-input-bytes (csv-rows (info->column-specs info)))]
    [paths (error 'csv-header-rows "expected one file\n  given: ~a" (length paths))]))

(define esl-zip-file-names '("zip.train.gz" "zip.test.gz"))

(define decimal-rx #px"^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$")

(define (esl-zip-rows files info)
  (match-define (list train test) esl-zip-file-names)
  (define width (sub1 (hash-ref info "column-count")))
  (sequence-append (esl-zip-split-rows files train "train" width)
                   (esl-zip-split-rows files test "test" width)))

(define (esl-zip-split-rows files name split width)
  (for/stream ([line (in-lines (~> (hash-ref files name) gunzip-file open-input-bytes))]
               [number (in-naturals 1)])
    (match (string-split line)
      [tokens #:when (not (= (length tokens) width))
       (error 'esl-zip-rows "wrong number of tokens\n  file: ~a\n  line: ~a\n  expected: ~a\n  given: ~a"
              name number width (length tokens))]
      [(list _ ... (and (not (regexp decimal-rx)) token) _ ...)
       (error 'esl-zip-rows "not a number\n  file: ~a\n  line: ~a\n  token: ~a" name number token)]
      [(cons (app (lambda (token) (string->number token 10 'number-or-false 'decimal-as-exact))
                  (? (integer-in 0 9) digit))
             pixels)
       (cons digit
             (append (map (lambda (pixel) (string->number (string-append "#i" pixel) 10)) pixels) (list split)))]
      [(cons token _)
       (error 'esl-zip-rows "digit is not a whole number from 0 to 9\n  file: ~a\n  line: ~a\n  token: ~a"
              name number token)])))
