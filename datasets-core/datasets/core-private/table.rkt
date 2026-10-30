#lang racket/base
(require (only-in racket/contract/base flat-named-contract integer-in or/c)
         (only-in racket/match match))
(provide (struct-out dataset-table-struct)
         unchecked-dataset-table
         dataset-table
         dataset-missing dataset-missing?
         dataset-cell/c dataset-column/c dataset-info/c
         freeze column-info)

(struct missing-value ())
(define dataset-missing (missing-value))
(define (dataset-missing? v) (eq? v dataset-missing))
(struct dataset-table (names columns row-count info)
  #:transparent #:name dataset-table-struct #:constructor-name unchecked-dataset-table)

(define int64? (integer-in (- (expt 2 63)) (sub1 (expt 2 63))))

(define dataset-cell/c
  (flat-named-contract 'dataset-cell/c (or/c dataset-missing? int64? flonum? string?)))

(define (column->list v)
  (cond [(list? v) v]
        [(vector? v) (vector->list v)]
        [else #f]))

(define (present-cells cells)
  (for/list ([c (in-list cells)] #:unless (dataset-missing? c)) c))

(define (homogeneous? cells)
  (define present (present-cells cells))
  (or (andmap number? present) (andmap string? present)))

(define (dataset-column? v)
  (define cells (column->list v))
  (and cells (andmap dataset-cell/c cells) (homogeneous? cells)))

(define dataset-column/c (flat-named-contract 'dataset-column/c dataset-column?))

(define (column-description? v)
  (and (hash? v) (string? (hash-ref v "name" #f))))

(define (dataset-info? v)
  (and (hash? v)
       (for/and ([k (in-hash-keys v)]) (string? k))
       (match (hash-ref v "columns" '())
         [(list (? column-description?) ...) #t]
         [_ #f])))

(define dataset-info/c (flat-named-contract 'dataset-info/c dataset-info?))

(define (freeze v)
  (cond [(string? v) (string->immutable-string v)]
        [(pair? v) (cons (freeze (car v)) (freeze (cdr v)))]
        [(hash? v) (for/hash ([(k x) (in-hash v)]) (values (freeze k) (freeze x)))]
        [else v]))

(define (column-info name original-name type description missing-count)
  (hash "name" name "original-name" original-name "type" type "levels" '() "unit" #f
        "missing-count" missing-count "description" description))

(define (column-type cells)
  (define present (present-cells cells))
  (cond [(andmap exact-integer? present) "integer"]
        [(andmap string? present) "string"]
        [else "real"]))

(define (freeze-column cells)
  (vector->immutable-vector
   (for/vector #:length (length cells) ([c (in-list cells)])
     (if (string? c) (string->immutable-string c) c))))

(define (dataset-table names columns #:info [info (hash)])
  (define frozen-names (map string->immutable-string names))
  (define cell-lists (map column->list columns))
  (define rows (length (car cell-lists)))
  (define defaults
    (hash "name" #f "rows" rows "column-count" (length names)
          "columns" (for/list ([n (in-list frozen-names)] [cells (in-list cell-lists)])
                      (column-info n n (column-type cells) ""
                                   (for/sum ([c (in-list cells)]) (if (dataset-missing? c) 1 0))))
          "identifiers" '() "suggested-targets" '()
          "source-url" #f "source-version" #f "source-sha256" #f "sha256" #f
          "license" #f "citation" #f "notes" #f "time-series" #f))
  (unchecked-dataset-table
   frozen-names
   (vector->immutable-vector (list->vector (map freeze-column cell-lists)))
   rows
   (freeze (for/fold ([merged defaults]) ([(k v) (in-hash info)]) (hash-set merged k v)))))
