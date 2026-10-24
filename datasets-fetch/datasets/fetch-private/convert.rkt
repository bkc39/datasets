#lang racket/base
(require (only-in datasets/fetch-private/archive gunzip-file tar-gz-member)
         (only-in datasets/fetch-private/csv csv-rows info->column-specs)
         (only-in racket/contract/base -> and/c contract-out flat-named-contract hash/c)
         (only-in racket/file file->bytes)
         (only-in racket/flonum fl/)
         (only-in racket/match match match-define)
         (only-in racket/sequence sequence-map)
         (only-in threading ~>))
(provide (contract-out
          [covtype-rows converter/c]
          [california-housing-rows converter/c]
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

(define (csv-header-rows files info)
  (match (hash-values files)
    [(list path) (~> path file->bytes open-input-bytes (csv-rows (info->column-specs info)))]
    [paths (error 'csv-header-rows "expected one file\n  given: ~a" (length paths))]))
