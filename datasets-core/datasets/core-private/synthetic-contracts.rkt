#lang racket/base
(require (for-syntax racket/base syntax/parse)
         (only-in racket/contract/base
                  ->i </c >/c >=/c and/c any/c flat-named-contract integer-in list/c listof
                  non-empty-listof or/c real-in unsupplied-arg?)
         (only-in syntax/parse/define define-syntax-parse-rule)
         (only-in datasets/core-private/table dataset-table?))
(provide synthetic/c dataset-format/c
         seed/c nonnegative/c positive/c probability/c factor/c
         sample-split/c sample-counts/c center-box/c points/c
         blobs-counts-match-centers? blobs-std-matches-centers? features-match-dimension?
         classification-features-fit? classification-clusters-fit? classification-weights-fit?
         length-matches-features? quantiles-samples-fit?)

(define seed/c
  (flat-named-contract 'seed/c
    (or/c #f (integer-in 0 2147483647) pseudo-random-generator? pseudo-random-generator-vector?)))
(define nonnegative/c (flat-named-contract 'nonnegative/c (and/c rational? (>=/c 0))))
(define positive/c (flat-named-contract 'positive/c (and/c rational? (>/c 0))))
(define probability/c (flat-named-contract 'probability/c (and/c rational? (real-in 0 1))))
(define factor/c (flat-named-contract 'factor/c (and/c rational? (>=/c 0) (</c 1))))
(define sample-split/c
  (flat-named-contract 'sample-split/c
    (list/c exact-nonnegative-integer? exact-nonnegative-integer?)))
(define sample-counts/c
  (flat-named-contract 'sample-counts/c
    (and/c (non-empty-listof exact-nonnegative-integer?) (λ (ns) (positive? (apply + ns))))))
(define center-box/c
  (flat-named-contract 'center-box/c
    (and/c (list/c rational? rational?) (λ (box) (<= (car box) (cadr box))))))
(define points/c
  (flat-named-contract 'points/c
    (and/c (non-empty-listof (non-empty-listof rational?))
           (λ (ps) (for/and ([p (in-list (cdr ps))]) (= (length p) (length (car ps))))))))
(define dataset-format/c
  (flat-named-contract 'dataset-format/c (or/c 'polars 'table 'matrix 'data-frame)))

;; Preconditions see `the-unsupplied-arg` for an omitted keyword; `given` maps it to the
;; generator's default, which must agree with the definition in datasets/synthetic.
(define (given v default) (if (unsupplied-arg? v) default v))

(define (blobs-counts-match-centers? n-samples centers)
  (define ns (given n-samples 100))
  (define cs (given centers #f))
  (or (exact-integer? ns) (not cs) (and (pair? cs) (= (length cs) (length ns)))))

(define (blobs-std-matches-centers? n-samples centers cluster-std)
  (define ns (given n-samples 100))
  (define cs (given centers #f))
  (define std (given cluster-std 1.0))
  (define count
    (cond [(exact-integer? cs) cs]
          [(pair? cs) (length cs)]
          [(pair? ns) (length ns)]
          [else 3]))
  (or (real? std) (= (length std) count)))

(define (features-match-dimension? n-features points)
  (define nf (given n-features #f))
  (define ps (given points #f))
  (or (not nf) (not (pair? ps)) (= nf (length (car ps)))))

(define (classification-features-fit? n-informative n-redundant n-repeated n-features)
  (<= (+ (given n-informative 2) (given n-redundant 2) (given n-repeated 0))
      (given n-features 20)))

(define (classification-clusters-fit? n-classes n-clusters-per-class n-informative)
  (define clusters (* (given n-classes 2) (given n-clusters-per-class 2)))
  (<= (integer-length (sub1 clusters)) (given n-informative 2)))

(define (classification-weights-fit? weights n-classes)
  (define ws (given weights #f))
  (define k (given n-classes 2))
  (or (not ws)
      (= (length ws) k)
      (and (= (length ws) (sub1 k)) (<= (apply + ws) 1))))

(define (length-matches-features? v n-features)
  (define x (given v #f))
  (or (not (list? x)) (= (length x) (given n-features 20))))

(define (quantiles-samples-fit? n-samples n-classes)
  (>= (given n-samples 100) (given n-classes 3)))

(begin-for-syntax
  (define-syntax-class generator
    #:description "a datasets/synthetic generator name"
    #:attributes ([domain 1] [pre 1])
    (pattern (~datum make-blobs)
      #:with (domain ...)
      #'(#:n-samples [n-samples (or/c exact-positive-integer? sample-counts/c)]
         #:n-features [n-features (or/c #f exact-positive-integer?)]
         #:centers [centers (or/c #f exact-positive-integer? points/c)]
         #:cluster-std [cluster-std (or/c nonnegative/c (non-empty-listof nonnegative/c))]
         #:center-box [center-box center-box/c]
         #:shuffle? [shuffle? boolean?])
      #:with (pre ...)
      #'(#:pre/name (n-samples centers)
         "per-centre counts need #:centers #f or one centre per count"
         (blobs-counts-match-centers? n-samples centers)
         #:pre/name (n-samples centers cluster-std) "#:cluster-std needs one entry per centre"
         (blobs-std-matches-centers? n-samples centers cluster-std)
         #:pre/name (n-features centers) "#:n-features must equal the dimension of #:centers"
         (features-match-dimension? n-features centers)))
    (pattern (~datum make-moons)
      #:with (domain ...)
      #'(#:n-samples [n-samples (or/c exact-positive-integer? sample-split/c)]
         #:shuffle? [shuffle? boolean?]
         #:noise [noise (or/c #f nonnegative/c)])
      #:with (pre ...) #'())
    (pattern (~datum make-circles)
      #:with (domain ...)
      #'(#:n-samples [n-samples (or/c exact-nonnegative-integer? sample-split/c)]
         #:shuffle? [shuffle? boolean?]
         #:noise [noise (or/c #f nonnegative/c)]
         #:factor [factor factor/c])
      #:with (pre ...) #'())
    (pattern (~datum make-classification)
      #:with (domain ...)
      #'(#:n-samples [n-samples exact-positive-integer?]
         #:n-features [n-features exact-positive-integer?]
         #:n-informative [n-informative exact-positive-integer?]
         #:n-redundant [n-redundant exact-nonnegative-integer?]
         #:n-repeated [n-repeated exact-nonnegative-integer?]
         #:n-classes [n-classes exact-positive-integer?]
         #:n-clusters-per-class [n-clusters-per-class exact-positive-integer?]
         #:weights [weights (or/c #f (listof probability/c))]
         #:flip-y [flip-y probability/c]
         #:class-sep [class-sep positive/c]
         #:hypercube? [hypercube? boolean?]
         #:shift [shift (or/c #f rational? (non-empty-listof rational?))]
         #:scale [scale (or/c #f positive/c (non-empty-listof positive/c))]
         #:shuffle? [shuffle? boolean?])
      #:with (pre ...)
      #'(#:pre/name (n-informative n-redundant n-repeated n-features)
         "informative + redundant + repeated features exceed #:n-features"
         (classification-features-fit? n-informative n-redundant n-repeated n-features)
         #:pre/name (n-classes n-clusters-per-class n-informative)
         "#:n-classes × #:n-clusters-per-class exceeds 2^#:n-informative"
         (classification-clusters-fit? n-classes n-clusters-per-class n-informative)
         #:pre/name (weights n-classes)
         "#:weights needs n-classes or n-classes − 1 entries (the latter summing ≤ 1)"
         (classification-weights-fit? weights n-classes)
         #:pre/name (shift n-features) "#:shift list needs one entry per feature"
         (length-matches-features? shift n-features)
         #:pre/name (scale n-features) "#:scale list needs one entry per feature"
         (length-matches-features? scale n-features)))
    (pattern (~datum make-regression)
      #:with (domain ...)
      #'(#:n-samples [n-samples exact-positive-integer?]
         #:n-features [n-features exact-positive-integer?]
         #:n-informative [n-informative exact-nonnegative-integer?]
         #:n-targets [n-targets exact-positive-integer?]
         #:bias [bias rational?]
         #:effective-rank [effective-rank (or/c #f exact-positive-integer?)]
         #:tail-strength [tail-strength probability/c]
         #:noise [noise nonnegative/c]
         #:shuffle? [shuffle? boolean?])
      #:with (pre ...) #'())
    (pattern (~datum make-friedman1)
      #:with (domain ...)
      #'(#:n-samples [n-samples exact-positive-integer?]
         #:n-features [n-features (integer-in 5 #f)]
         #:noise [noise nonnegative/c])
      #:with (pre ...) #'())
    (pattern (~or* (~datum make-friedman2) (~datum make-friedman3) (~datum make-s-curve))
      #:with (domain ...)
      #'(#:n-samples [n-samples exact-positive-integer?] #:noise [noise nonnegative/c])
      #:with (pre ...) #'())
    (pattern (~datum make-hastie-10-2)
      #:with (domain ...) #'(#:n-samples [n-samples exact-positive-integer?])
      #:with (pre ...) #'())
    (pattern (~datum make-swiss-roll)
      #:with (domain ...)
      #'(#:n-samples [n-samples exact-positive-integer?]
         #:noise [noise nonnegative/c]
         #:hole? [hole? boolean?])
      #:with (pre ...) #'())
    (pattern (~datum make-gaussian-quantiles)
      #:with (domain ...)
      #'(#:mean [mean (or/c #f (non-empty-listof rational?))]
         #:cov [cov nonnegative/c]
         #:n-samples [n-samples exact-positive-integer?]
         #:n-features [n-features (or/c #f exact-positive-integer?)]
         #:n-classes [n-classes exact-positive-integer?]
         #:shuffle? [shuffle? boolean?])
      #:with (pre ...)
      #'(#:pre/name (n-features mean) "#:n-features must equal the length of #:mean"
         (features-match-dimension? n-features (and (pair? mean) (list mean)))
         #:pre/name (n-samples n-classes) "#:n-samples must be at least #:n-classes"
         (quantiles-samples-fit? n-samples n-classes)))))

(define-syntax-parse-rule (synthetic/c g:generator (~optional (~and #:format with-format)))
  #:with (format-domain ...)
  (if (attribute with-format) #'(#:format [format dataset-format/c]) #'())
  #:with result/c (if (attribute with-format) #'any/c #'dataset-table?)
  (->i () (format-domain ... g.domain ... #:seed [seed seed/c]) g.pre ... [result result/c]))
