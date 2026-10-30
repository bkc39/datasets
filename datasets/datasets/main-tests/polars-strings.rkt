#lang racket/base
(require (only-in datasets dataset-info dataset-missing? dataset-table-column
                  dataset-table-row-count load-dataset)
         (only-in datasets/polars table->polars)
         (prefix-in pl: (only-in polars dataframe-column polars-null? series-ref))
         (only-in racket/list range))

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

(define (column-mismatches table frame name)
  (define column (dataset-table-column table name))
  (define series (pl:dataframe-column frame name))
  (filter (λ (row) (not (same-cell? (vector-ref column row) (pl:series-ref series row))))
          (range (dataset-table-row-count table))))

(define (string-column? column)
  (member (hash-ref column "type") '("categorical" "string")))

(module+ test
  (require (only-in rackunit check-equal? check-true))
  (define table (load-dataset 'adult #:format 'table))
  (define columns (hash-ref (dataset-info 'adult) "columns"))
  (define string-columns
    (map (λ (column) (hash-ref column "name")) (filter string-column? columns)))
  (for ([trial (in-range 4)])
    (define-values (frame collections)
      (call-with-minor-collector (λ () (table->polars table))))
    (check-true (>= collections 50) (format "trial ~a: ~a minor collections" trial collections))
    (for ([name (in-list string-columns)])
      (check-equal? (column-mismatches table frame name) '()
                    (format "trial ~a, column ~a" trial name)))))
