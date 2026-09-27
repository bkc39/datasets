#lang racket/base
(require racket/list racket/runtime-path racket/vector)
(provide dataset-names dataset-info load-dataset-table
         dataset-missing dataset-missing?
         dataset-table? dataset-table-names dataset-table-columns
         dataset-table-row-count dataset-table-info dataset-table-column)

;; Opaque, singleton sentinel: cannot be confused with any ordinary observation.
(struct missing-value ())
(define dataset-missing (missing-value))
(define (dataset-missing? v) (eq? v dataset-missing))
(struct dataset-table (names columns row-count info) #:transparent)
(define-runtime-path registry-path "private/registry.rktd")
(define-runtime-path data-path "private/data")
(define (freeze v)
  (cond [(string? v) (string->immutable-string v)]
        [(pair? v) (cons (freeze (car v)) (freeze (cdr v)))]
        [(hash? v) (for/hash ([(k x) (in-hash v)]) (values (freeze k) (freeze x)))]
        [else v]))
(define registry (freeze (call-with-input-file registry-path read)))
(define (dataset-names) (sort (map string->symbol (hash-keys registry)) symbol<?))
(define (dataset-info name)
  (unless (and (symbol? name) (hash-has-key? registry (symbol->string name)))
    (raise-arguments-error 'dataset-info "unknown dataset" "name" name "available" (dataset-names)))
  (hash-ref registry (symbol->string name)))
(define (dataset-table-column table name)
  (define i (index-of (dataset-table-names table) name))
  (unless i (raise-arguments-error 'dataset-table-column "unknown column" "column" name
                                   "available" (dataset-table-names table)))
  (vector-ref (dataset-table-columns table) i))
(define (load-dataset-table name #:columns [selected #f])
  (define info (dataset-info name))
  (define all-names (map (lambda (c) (hash-ref c "name")) (hash-ref info "columns")))
  (define names (or selected all-names))
  (unless (and (list? names) (pair? names) (andmap string? names))
    (raise-argument-error 'load-dataset-table "nonempty list of column-name strings or #f" selected))
  (when (check-duplicates names)
    (raise-arguments-error 'load-dataset-table "duplicate column" "columns" names))
  (define indices
    (for/list ([n (in-list names)])
      (or (index-of all-names n)
          (raise-arguments-error 'load-dataset-table "unknown column" "column" n "available" all-names))))
  (define rows (call-with-input-file (build-path data-path (format "~a.rktd" name)) read))
  (define columns
    (vector->immutable-vector
     (for/vector ([i (in-list indices)])
       (vector->immutable-vector
        (for/vector ([row (in-list rows)])
          (define v (list-ref row i))
          (if (eq? v 'missing) dataset-missing (freeze v)))))))
  (dataset-table (map string->immutable-string names) columns (length rows) info))
