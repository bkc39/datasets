#lang racket/base
(require (only-in datasets/fetch-private/archive gunzip-file tar-gz-member)
         (only-in datasets/fetch-private/csv csv-rows info->column-specs)
         (only-in racket/contract/base -> and/c contract-out flat-named-contract hash/c)
         (only-in racket/file file->bytes)
         (only-in racket/flonum fl/)
         (only-in racket/match match match-define)
         (only-in racket/sequence sequence-map))
(provide (contract-out
          [covtype-rows converter/c]
          [california-housing-rows converter/c]
          [csv-header-rows converter/c]))

(define converter-files/c
  (flat-named-contract 'converter-files (hash/c string? complete-path? #:immutable #t)))
(define converter-info/c (flat-named-contract 'dataset-info (and/c hash? immutable?)))
(define converter/c (-> converter-files/c converter-info/c sequence?))

(define (covtype-rows files info)
  (csv-rows (open-input-bytes (gunzip-file (hash-ref files "covtype.data.gz")))
            (info->column-specs info)
            #:header? #f))

(define cal-housing-specs
  (for/list ([name (in-list '("longitude" "latitude" "housingMedianAge" "totalRooms" "totalBedrooms"
                              "population" "households" "medianIncome" "medianHouseValue"))])
    (list name 'real)))

(define (california-housing-rows files info)
  (sequence-map california-housing-row
                (csv-rows (open-input-bytes (tar-gz-member (hash-ref files "cal_housing.tgz")
                                                           "CaliforniaHousing/cal_housing.data"))
                          cal-housing-specs
                          #:header? #f)))

(define (california-housing-row row)
  (match-define (list longitude latitude age rooms bedrooms population households income value) row)
  (list income age (fl/ rooms households) (fl/ bedrooms households) population
        (fl/ population households) latitude longitude (fl/ value 100000.0)))

(define (csv-header-rows files info)
  (match (hash-values files)
    [(list path) (csv-rows (open-input-bytes (file->bytes path)) (info->column-specs info))]
    [paths (raise (exn:fail (format "csv-header-rows: expected one file\n  given: ~a" (length paths))
                            (current-continuation-marks)))]))
