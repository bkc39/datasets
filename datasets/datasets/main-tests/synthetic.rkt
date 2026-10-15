#lang racket/base
(require (only-in racket/match match match-define)
         (only-in datasets
                  dataset-table-column dataset-table-columns dataset-table-info dataset-table-names
                  dataset-table-row-count make-blobs make-circles make-classification
                  make-friedman1 make-friedman2 make-friedman3 make-gaussian-quantiles
                  make-hastie-10-2 make-moons make-regression make-s-curve make-swiss-roll)
         (only-in datasets/core-tests/plist apply/plist)
         (only-in datasets/data-frame data-frame-column-names)
         (only-in math/matrix matrix-ref)
         (prefix-in pl: polars)
         (prefix-in s: datasets/synthetic))
(module+ test
  (require (only-in rackunit check-equal? check-regexp-match check-true test-case)
           (only-in racket/contract/combinator exn:fail:contract:blame?))
  (define cases
    `((,make-blobs ,s:make-blobs (#:n-samples 12)
       (#:center-box #:centers #:cluster-std #:n-features #:n-samples #:seed #:shuffle?))
      (,make-moons ,s:make-moons (#:n-samples 12 #:noise 0.1)
       (#:n-samples #:noise #:seed #:shuffle?))
      (,make-circles ,s:make-circles (#:n-samples 12 #:noise 0.1)
       (#:factor #:n-samples #:noise #:seed #:shuffle?))
      (,make-classification ,s:make-classification (#:n-samples 12 #:n-features 5)
       (#:class-sep #:flip-y #:hypercube? #:n-classes #:n-clusters-per-class #:n-features
        #:n-informative #:n-redundant #:n-repeated #:n-samples #:scale #:seed #:shift #:shuffle?
        #:weights))
      (,make-regression ,s:make-regression (#:n-samples 12 #:n-features 4 #:n-targets 2)
       (#:bias #:effective-rank #:n-features #:n-informative #:n-samples #:n-targets #:noise #:seed
        #:shuffle? #:tail-strength))
      (,make-friedman1 ,s:make-friedman1 (#:n-samples 12) (#:n-features #:n-samples #:noise #:seed))
      (,make-friedman2 ,s:make-friedman2 (#:n-samples 12) (#:n-samples #:noise #:seed))
      (,make-friedman3 ,s:make-friedman3 (#:n-samples 12) (#:n-samples #:noise #:seed))
      (,make-hastie-10-2 ,s:make-hastie-10-2 (#:n-samples 12) (#:n-samples #:seed))
      (,make-swiss-roll ,s:make-swiss-roll (#:n-samples 12 #:hole? #t)
       (#:hole? #:n-samples #:noise #:seed))
      (,make-s-curve ,s:make-s-curve (#:n-samples 12) (#:n-samples #:noise #:seed))
      (,make-gaussian-quantiles ,s:make-gaussian-quantiles (#:n-samples 12)
       (#:cov #:mean #:n-classes #:n-features #:n-samples #:seed #:shuffle?))))

  (test-case "exact: 'table is the core table"
    (for ([c (in-list cases)])
      (match-define (list w core args _) c)
      (check-equal? (apply/plist w (append args '(#:format table #:seed 5)))
                    (apply/plist core (append args '(#:seed 5))))))

  (test-case "exact: keywords follow the scikit-learn mapping; names match"
    (for ([c (in-list cases)])
      (match-define (list w core _ keywords) c)
      (define-values (w-required w-all) (procedure-keywords w))
      (define-values (core-required core-all) (procedure-keywords core))
      (check-equal? w-required '())
      (check-equal? core-required '())
      (check-equal? core-all keywords)
      (check-equal? w-all (sort (cons '#:format keywords) keyword<?))
      (check-equal? (object-name w) (object-name core))))

  (test-case "exact: default 'polars"
    (for ([c (in-list cases)])
      (match-define (list w core args _) c)
      (define t (apply/plist core (append args '(#:seed 5))))
      (define p (apply/plist w (append args '(#:seed 5))))
      (check-true (pl:dataframe? p))
      (check-equal? (pl:dataframe-column-names p) (dataset-table-names t))
      (for ([n (in-list (dataset-table-names t))]
            [m (in-list (hash-ref (dataset-table-info t) "columns"))])
        (define series (pl:dataframe-column p n))
        (check-equal? (pl:series-dtype series)
                      (match (hash-ref m "type") ["integer" 'int64] ["real" 'float64]))
        (for ([v (in-vector (dataset-table-column t n))] [i (in-naturals)])
          (check-equal? (pl:series-ref series i) v)))))

  (test-case "exact: 'matrix succeeds for every generator"
    (for ([c (in-list cases)])
      (match-define (list w core args _) c)
      (define t (apply/plist core (append args '(#:seed 5))))
      (define m (apply/plist w (append args '(#:format matrix #:seed 5))))
      (for* ([i (in-range (dataset-table-row-count t))]
             [(column j) (in-parallel (dataset-table-columns t) (in-naturals))])
        (check-equal? (matrix-ref m i j) (vector-ref column i)))))

  (test-case "exact: 'data-frame keeps the column names"
    (for ([c (in-list cases)])
      (match-define (list w core args _) c)
      (check-equal? (data-frame-column-names
                     (apply/plist w (append args '(#:format data-frame #:seed 5))))
                    (dataset-table-names (apply/plist core (append args '(#:seed 5)))))))

  (test-case "exact: invalid arguments blame the caller"
    (define (violation form)
      (parameterize ([current-namespace (make-base-namespace)])
        (namespace-attach-module (variable-reference->empty-namespace (#%variable-reference))
                                 'datasets)
        (eval `(module client racket/base (require datasets) ,form))
        (with-handlers ([exn:fail:contract:blame? exn-message])
          (eval '(require 'client))
          "no violation")))
    (for ([form (in-list '((make-moons #:format 'csv) (make-blobs #:centers '((0 0) (1)))
                           (make-gaussian-quantiles #:n-samples 2 #:n-classes 3)
                           (load-dataset 'iris #:format 'csv) (load-iris #:format 'csv)
                           (load-iris #:columns 5) (load-dataset 'iris #:columns '())))])
      (check-regexp-match #rx"blaming: [^\n]*client" (violation form)))))
