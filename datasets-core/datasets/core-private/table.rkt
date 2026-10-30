#lang racket/base
(require (for-syntax racket/base (only-in syntax/parse ...+ expr id))
         (only-in racket/contract/base and/c flat-named-contract non-empty-listof)
         (only-in racket/vector vector-map)
         (only-in syntax/parse/define define-syntax-parse-rule))
(provide (struct-out dataset-table-struct)
         unchecked-dataset-table
         dataset-table
         dataset-missing dataset-missing?
         columns/c dataset-info/c
         freeze column-info)

(struct missing-value ())
(define dataset-missing (missing-value))
(define (dataset-missing? v) (eq? v dataset-missing))
(struct dataset-table (names columns row-count info)
  #:transparent #:name dataset-table-struct #:constructor-name unchecked-dataset-table)

(define-syntax-parse-rule (for/fold-column (accumulator ...) ([cell:id column:expr]) body ...+)
  (let ([v column])
    (if (vector? v)
        (for/fold (accumulator ...) ([cell (in-vector v)]) body ...)
        (for/fold (accumulator ...) ([cell (in-list v)]) body ...))))

(define int64-min (- (expt 2 63)))
(define int64-max (sub1 (expt 2 63)))

(define (int64? v)
  (or (fixnum? v) (and (exact-integer? v) (<= int64-min v int64-max))))

(define (next-kind kind cell)
  (cond [(dataset-missing? cell) kind]
        [(int64? cell) (case kind [(empty integer) 'integer] [(real) 'real] [else #f])]
        [(flonum? cell) (case kind [(empty integer real) 'real] [else #f])]
        [(string? cell) (case kind [(empty string) 'string] [else #f])]
        [else #f]))

(define (column-summary column)
  (for/fold-column ([kind 'empty] [missing 0]) ([cell column])
    (values (next-kind kind cell) (if (dataset-missing? cell) (add1 missing) missing))))

(define (column-kind column)
  (define-values (kind _missing) (column-summary column))
  kind)

(define (dataset-column? v)
  (and (or (vector? v) (list? v)) (column-kind v) #t))

(define (column-length column)
  (if (vector? column) (vector-length column) (length column)))

(define (equal-lengths? columns)
  (define n (column-length (car columns)))
  (andmap (λ (c) (= (column-length c) n)) columns))

(define (columns/c names)
  (and/c (non-empty-listof (flat-named-contract 'dataset-column/c dataset-column?))
         (flat-named-contract "one column per name" (λ (cs) (= (length cs) (length names))))
         (flat-named-contract "every column the same length" equal-lengths?)))

(define (info-value? v ancestors)
  (cond [(or (string? v) (number? v) (boolean? v) (symbol? v) (null? v)) #t]
        [(hash-ref ancestors v #f) #f]
        [(list? v) (andmap (λ (x) (info-value? x (hash-set ancestors v #t))) v)]
        [(hash? v) (info-hash? v (hash-set ancestors v #t))]
        [else #f]))

(define (info-hash? v ancestors)
  (and (hash-equal? v)
       (for/and ([(k x) (in-hash v)]) (and (string? k) (info-value? x ancestors)))))

(define (column-description? v)
  (and (hash? v) (string? (hash-ref v "name" #f))))

(define (dataset-info? v)
  (and (hash? v)
       (info-hash? v (hasheq v #t))
       (let ([columns (hash-ref v "columns" '())])
         (and (list? columns) (andmap column-description? columns)))))

(define dataset-info/c (flat-named-contract 'dataset-info/c dataset-info?))

(define (freeze v)
  (cond [(string? v) (string->immutable-string v)]
        [(pair? v) (cons (freeze (car v)) (freeze (cdr v)))]
        [(hash? v) (for/hash ([(k x) (in-hash v)]) (values (freeze k) (freeze x)))]
        [else v]))

(define (column-info name original-name type description missing-count)
  (hash "name" name "original-name" original-name "type" type "levels" '() "unit" #f
        "missing-count" missing-count "description" description))

(define (freeze-cell cell)
  (if (string? cell) (string->immutable-string cell) cell))

(define (string-column? cells)
  (for/first ([cell (in-vector cells)] #:unless (dataset-missing? cell)) (string? cell)))

(define (freeze-column column)
  (define cells (if (vector? column) column (list->vector column)))
  (vector->immutable-vector (if (string-column? cells) (vector-map freeze-cell cells) cells)))

(define (describe name column)
  (define-values (kind missing) (column-summary column))
  (column-info name name (if (eq? kind 'empty) "integer" (symbol->string kind)) "" missing))

(define (dataset-table names columns #:info [info (hash)])
  (define frozen-names (map string->immutable-string names))
  (define frozen-columns (map freeze-column columns))
  (define rows (vector-length (car frozen-columns)))
  (define defaults
    (hash "name" #f "rows" rows "column-count" (length names)
          "columns" (map describe frozen-names frozen-columns)
          "identifiers" '() "suggested-targets" '()
          "source-url" #f "source-version" #f "source-sha256" #f "sha256" #f
          "license" #f "citation" #f "notes" #f "time-series" #f))
  (unchecked-dataset-table
   frozen-names
   (apply vector-immutable frozen-columns)
   rows
   (freeze (for/fold ([merged defaults]) ([(k v) (in-hash info)]) (hash-set merged k v)))))
