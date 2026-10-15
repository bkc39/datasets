#lang racket/base
(require racket/match datasets datasets/data-frame math/matrix (prefix-in pl: polars)
         (prefix-in s: datasets/synthetic))
(module+ test
  (require rackunit (only-in racket/contract/combinator exn:fail:contract:blame?))
  (define (call gen plist)
    (define pairs
      (sort (let loop ([p plist])
              (match p ['() '()] [(list* k v rest) (cons (cons k v) (loop rest))]))
            keyword<? #:key car))
    (keyword-apply gen (map car pairs) (map cdr pairs) '()))
  (define cases
    `((,make-blobs ,s:make-blobs (#:n-samples 12))
      (,make-moons ,s:make-moons (#:n-samples 12 #:noise 0.1))
      (,make-circles ,s:make-circles (#:n-samples 12 #:noise 0.1))
      (,make-classification ,s:make-classification (#:n-samples 12 #:n-features 5))
      (,make-regression ,s:make-regression (#:n-samples 12 #:n-features 4 #:n-targets 2))
      (,make-friedman1 ,s:make-friedman1 (#:n-samples 12))
      (,make-friedman2 ,s:make-friedman2 (#:n-samples 12))
      (,make-friedman3 ,s:make-friedman3 (#:n-samples 12))
      (,make-hastie-10-2 ,s:make-hastie-10-2 (#:n-samples 12))
      (,make-swiss-roll ,s:make-swiss-roll (#:n-samples 12 #:hole? #t))
      (,make-s-curve ,s:make-s-curve (#:n-samples 12))
      (,make-gaussian-quantiles ,s:make-gaussian-quantiles (#:n-samples 12))))

  (test-case "exact: 'table is the core table"
    (for ([c (in-list cases)])
      (match-define (list w core args) c)
      (check-equal? (call w (append args '(#:format table #:seed 5))) (call core (append args '(#:seed 5))))))

  (test-case "exact: keyword parity and names"
    (for ([c (in-list cases)])
      (match-define (list w core _) c)
      (define-values (w-required w-all) (procedure-keywords w))
      (define-values (core-required core-all) (procedure-keywords core))
      (check-equal? w-required '())
      (check-equal? w-all (sort (cons '#:format core-all) keyword<?))
      (check-equal? (object-name w) (object-name core))))

  (test-case "exact: default 'polars"
    (for ([c (in-list cases)])
      (match-define (list w core args) c)
      (define t (call core (append args '(#:seed 5))))
      (define p (call w (append args '(#:seed 5))))
      (check-true (pl:dataframe? p))
      (check-equal? (pl:dataframe-column-names p) (dataset-table-names t))
      (for ([n (in-list (dataset-table-names t))] [m (in-list (hash-ref (dataset-table-info t) "columns"))])
        (define series (pl:dataframe-column p n))
        (check-equal? (pl:series-dtype series) (match (hash-ref m "type") ["integer" 'int64] ["real" 'float64]))
        (for ([v (in-vector (dataset-table-column t n))] [i (in-naturals)])
          (check-equal? (pl:series-ref series i) v)))))

  (test-case "exact: 'matrix succeeds for every generator"
    (for ([c (in-list cases)])
      (match-define (list w core args) c)
      (define t (call core (append args '(#:seed 5))))
      (define m (call w (append args '(#:format matrix #:seed 5))))
      (for* ([i (in-range (dataset-table-row-count t))]
             [(column j) (in-parallel (dataset-table-columns t) (in-naturals))])
        (check-equal? (matrix-ref m i j) (vector-ref column i)))))

  (test-case "exact: 'data-frame keeps the column names"
    (for ([c (in-list cases)])
      (match-define (list w core args) c)
      (check-equal? (data-frame-column-names (call w (append args '(#:format data-frame #:seed 5))))
                    (dataset-table-names (call core (append args '(#:seed 5)))))))

  (test-case "exact: invalid arguments blame the caller"
    (define (violation form)
      (parameterize ([current-namespace (make-base-namespace)])
        (namespace-attach-module (variable-reference->empty-namespace (#%variable-reference)) 'datasets)
        (eval `(module client racket/base (require datasets) ,form))
        (with-handlers ([exn:fail:contract:blame? exn-message]) (eval '(require 'client)) "no violation")))
    (for ([form (in-list '((make-moons #:format 'csv) (make-blobs #:centers '((0 0) (1)))
                           (make-gaussian-quantiles #:n-samples 2 #:n-classes 3)))])
      (check-regexp-match #rx"blaming: [^\n]*client" (violation form)))))
