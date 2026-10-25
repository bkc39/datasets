#lang racket/base
(require (only-in datasets/fetch-private/archive gunzip-file tar-gz-member)
         (only-in datasets/fetch-private/csv csv-rows info->column-specs)
         (only-in datasets/fetch-private/idx read-idx)
         (only-in racket/contract/base -> and/c contract-out flat-named-contract hash/c)
         (only-in racket/file file->bytes)
         (only-in racket/flonum fl/)
         (only-in racket/match match match* match-define)
         (only-in racket/sequence sequence-map)
         (only-in threading ~>))
(provide (contract-out
          [covtype-rows converter/c]
          [california-housing-rows converter/c]
          [idx-image-rows converter/c]
          [csv-header-rows converter/c]))

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

(define (idx-image-rows files info)
  (define width (- (hash-ref info "column-count") 2))
  (in-sequences (idx-split-rows files "train" "train" width)
                (idx-split-rows files "t10k" "test" width)))

(define (idx-split-rows files prefix split width)
  (define-values (image-dims images) (idx-file files (format "~a-images-idx3-ubyte.gz" prefix)))
  (define-values (label-dims labels) (idx-file files (format "~a-labels-idx1-ubyte.gz" prefix)))
  (match* (image-dims label-dims)
    [((list n h w) (list n)) #:when (= (* h w) width)
     (sequence-map (lambda (i)
                     (append (bytes->list (subbytes images (* i width) (* (add1 i) width)))
                             (list (bytes-ref labels i) split)))
                   (in-range n))]
    [((list n _ _) (list m)) #:when (not (= n m))
     (error 'idx-image-rows "image and label counts differ\n  split: ~a\n  images: ~a\n  labels: ~a"
            split n m)]
    [((list _ h w) (list _))
     (error 'idx-image-rows
            "image size does not match the column count\n  split: ~a\n  expected pixels: ~a\n  given: ~ax~a"
            split width h w)]
    [(_ _)
     (error 'idx-image-rows
            "expected 3-dimensional images and 1-dimensional labels\n  split: ~a\n  images: ~a\n  labels: ~a"
            split image-dims label-dims)]))

(define (idx-file files name)
  (~> (hash-ref files name) gunzip-file open-input-bytes read-idx))

(define (csv-header-rows files info)
  (match (hash-values files)
    [(list path) (~> path file->bytes open-input-bytes (csv-rows (info->column-specs info)))]
    [paths (error 'csv-header-rows "expected one file\n  given: ~a" (length paths))]))
