#lang racket/base
(require (only-in datasets dataset-info dataset-missing? dataset-table-column
                  dataset-table-names load-dataset)
         (only-in datasets/polars table->polars)
         (prefix-in pl: (only-in polars dataframe-column polars-null? series-ref))
         (only-in racket/list take)
         (only-in racket/match match-let*))

(define (call-with-minor-collector thunk)
  (define collections (box 0))
  (define collector
    (thread (λ ()
              (let loop ()
                (define probe (make-weak-box (make-vector 1)))
                (collect-garbage 'minor)
                (set-box! collections (+ (unbox collections) (if (weak-box-value probe) 0 1)))
                (sleep 0)
                (loop)))))
  (define result (dynamic-wind void thunk (λ () (kill-thread collector))))
  (values result (unbox collections)))

(define (same-cell? cell value)
  (if (dataset-missing? cell)
      (pl:polars-null? value)
      (equal? value cell)))

(define (column-mismatches cells series)
  (for/list ([cell (in-vector cells)]
             [row (in-naturals)]
             #:unless (same-cell? cell (pl:series-ref series row)))
    row))

(define (mismatch-message label rows)
  (format "~a: ~a mismatching rows, first ~a" label (length rows) (take rows (min 5 (length rows)))))

(define (string-column? column)
  (member (hash-ref column "type") '("categorical" "string")))

(define (string-cells? cells)
  (define present (filter (λ (cell) (not (dataset-missing? cell))) (vector->list cells)))
  (and (pair? present) (andmap string? present)))

(module+ test
  (require (only-in rackunit check-equal? check-pred check-true))
  (define table (load-dataset 'adult #:format 'table))
  (define columns (hash-ref (dataset-info 'adult) "columns"))
  (define string-columns
    (map (λ (column) (hash-ref column "name")) (filter string-column? columns)))
  (check-pred pair? string-columns)
  (check-equal? string-columns
                (filter (λ (name) (string-cells? (dataset-table-column table name)))
                        (dataset-table-names table)))
  (define (check-column frame name label)
    (define rows (column-mismatches (dataset-table-column table name) (pl:dataframe-column frame name)))
    (check-equal? (length rows) 0 (mismatch-message label rows)))
  (match-let* ([(list first second _ ...) string-columns]
               [swapped (column-mismatches (dataset-table-column table first)
                                           (pl:dataframe-column (table->polars table) second))])
    (check-true (> (length swapped) 1000))
    (check-true (< (string-length (mismatch-message "swapped" swapped)) 120)))
  (for ([trial (in-range 4)])
    (define-values (frame collections)
      (call-with-minor-collector (λ () (table->polars table))))
    (check-true (>= collections 50) (format "trial ~a: ~a minor collections" trial collections))
    (for ([name (in-list string-columns)])
      (check-column frame name (format "trial ~a, column ~a" trial name)))))
