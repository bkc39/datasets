#lang racket/base
(require (for-syntax racket/base
                     (only-in syntax/parse
                              attribute define-syntax-class expr id pattern str
                              ~and ~datum ~optional ~or*))
         (only-in racket/contract/base
                  ->i </c >/c >=/c and/c any/c flat-named-contract integer-in list/c listof
                  non-empty-listof or/c real-in unsupplied-arg?)
         (only-in racket/match match)
         (only-in syntax/parse/define define-syntax-parse-rule)
         (only-in datasets/core-private/table dataset-table?))
(provide synthetic/c dataset-format/c (for-syntax generator) centre-count
         seed/c finite/c nonnegative/c positive/c probability/c factor/c
         sample-split/c sample-counts/c center-box/c points/c
         blobs-counts-match-centers? blobs-std-matches-centers? features-match-dimension?
         classification-features-fit? classification-clusters-fit? classification-weights-fit?
         length-matches-features? quantiles-samples-fit?)

(define seed/c
  (flat-named-contract 'seed/c
    (or/c #f (integer-in 0 2147483647) pseudo-random-generator? pseudo-random-generator-vector?)))
(define finite/c
  (flat-named-contract 'finite/c
    (λ (x) (and (rational? x) (rational? (real->double-flonum x))))))
(define nonnegative/c (flat-named-contract 'nonnegative/c (and/c finite/c (>=/c 0))))
(define positive/c (flat-named-contract 'positive/c (and/c finite/c (>/c 0))))
(define probability/c (flat-named-contract 'probability/c (and/c finite/c (real-in 0 1))))
(define factor/c (flat-named-contract 'factor/c (and/c finite/c (>=/c 0) (</c 1))))
(define sample-split/c
  (flat-named-contract 'sample-split/c
    (list/c exact-nonnegative-integer? exact-nonnegative-integer?)))
(define sample-counts/c
  (flat-named-contract 'sample-counts/c
    (and/c (non-empty-listof exact-nonnegative-integer?) (λ (ns) (positive? (apply + ns))))))
(define center-box/c
  (flat-named-contract 'center-box/c
    (and/c (list/c finite/c finite/c) (λ (box) (apply <= box)))))
(define points/c
  (flat-named-contract 'points/c
    (and/c (non-empty-listof (non-empty-listof finite/c))
           (λ (ps) (apply = (map length ps))))))
(define dataset-format/c
  (flat-named-contract 'dataset-format/c (or/c 'polars 'table 'matrix 'data-frame)))

(define (given v default) (if (unsupplied-arg? v) default v))

(define (centre-count n-samples centers)
  (cond [(exact-integer? centers) centers]
        [(pair? centers) (length centers)]
        [(pair? n-samples) (length n-samples)]
        [else 3]))

(define (blobs-counts-match-centers? n-samples centers)
  (or (exact-integer? n-samples)
      (not centers)
      (and (pair? centers) (= (length centers) (length n-samples)))))

(define (blobs-std-matches-centers? n-samples centers cluster-std)
  (or (real? cluster-std) (= (length cluster-std) (centre-count n-samples centers))))

(define (features-match-dimension? n-features points)
  (match points
    [(cons point _) #:when n-features (= n-features (length point))]
    [_ #t]))

(define (classification-features-fit? n-informative n-redundant n-repeated n-features)
  (<= (+ n-informative n-redundant n-repeated) n-features))

(define (classification-clusters-fit? n-classes n-clusters-per-class n-informative)
  (<= (integer-length (sub1 (* n-classes n-clusters-per-class))) n-informative))

(define (classification-weights-fit? weights n-classes)
  (or (not weights)
      (= (length weights) n-classes)
      (and (= (length weights) (sub1 n-classes)) (<= (apply + weights) 1))))

(define (length-matches-features? v n-features)
  (or (not (list? v)) (= (length v) n-features)))

(define (quantiles-samples-fit? n-samples n-classes) (>= n-samples n-classes))

(begin-for-syntax
  (define-syntax-class argument
    #:description "a generator argument [name default contract]"
    (pattern [name:id default:expr contract:expr]
      #:with keyword (datum->syntax #'name (string->keyword (symbol->string (syntax-e #'name))))))

  (define-syntax-class precondition
    #:description "a precondition [(argument ...) message check]"
    (pattern [(id:id ...) message:str check:expr]))

  (define-syntax-class generator
    #:description "a datasets/synthetic generator name"
    #:attributes ([arg.name 1] [arg.default 1] [arg.contract 1] [arg.keyword 1]
                  [pre.id 2] [pre.message 1] [pre.check 1])
    (pattern (~datum make-blobs)
      #:with (arg:argument ...)
      #'([n-samples 100 (or/c exact-positive-integer? sample-counts/c)]
         [n-features #f (or/c #f exact-positive-integer?)]
         [centers #f (or/c #f exact-positive-integer? points/c)]
         [cluster-std 1.0 (or/c nonnegative/c (non-empty-listof nonnegative/c))]
         [center-box '(-10.0 10.0) center-box/c]
         [shuffle? #t boolean?])
      #:with (pre:precondition ...)
      #'([(n-samples centers) "per-centre counts need #:centers #f or one centre per count"
          (blobs-counts-match-centers? n-samples centers)]
         [(n-samples centers cluster-std) "#:cluster-std needs one entry per centre"
          (blobs-std-matches-centers? n-samples centers cluster-std)]
         [(n-features centers) "#:n-features must equal the dimension of #:centers"
          (features-match-dimension? n-features centers)]))
    (pattern (~datum make-moons)
      #:with (arg:argument ...)
      #'([n-samples 100 (or/c exact-positive-integer? sample-split/c)]
         [shuffle? #t boolean?]
         [noise #f (or/c #f nonnegative/c)])
      #:with (pre:precondition ...) #'())
    (pattern (~datum make-circles)
      #:with (arg:argument ...)
      #'([n-samples 100 (or/c exact-nonnegative-integer? sample-split/c)]
         [shuffle? #t boolean?]
         [noise #f (or/c #f nonnegative/c)]
         [factor 0.8 factor/c])
      #:with (pre:precondition ...) #'())
    (pattern (~datum make-classification)
      #:with (arg:argument ...)
      #'([n-samples 100 exact-positive-integer?]
         [n-features 20 exact-positive-integer?]
         [n-informative 2 exact-positive-integer?]
         [n-redundant 2 exact-nonnegative-integer?]
         [n-repeated 0 exact-nonnegative-integer?]
         [n-classes 2 exact-positive-integer?]
         [n-clusters-per-class 2 exact-positive-integer?]
         [weights #f (or/c #f (listof probability/c))]
         [flip-y 0.01 probability/c]
         [class-sep 1.0 positive/c]
         [hypercube? #t boolean?]
         [shift 0.0 (or/c #f finite/c (non-empty-listof finite/c))]
         [scale 1.0 (or/c #f positive/c (non-empty-listof positive/c))]
         [shuffle? #t boolean?])
      #:with (pre:precondition ...)
      #'([(n-informative n-redundant n-repeated n-features)
          "informative + redundant + repeated features exceed #:n-features"
          (classification-features-fit? n-informative n-redundant n-repeated n-features)]
         [(n-classes n-clusters-per-class n-informative)
          "#:n-classes × #:n-clusters-per-class exceeds 2^#:n-informative"
          (classification-clusters-fit? n-classes n-clusters-per-class n-informative)]
         [(weights n-classes)
          "#:weights needs n-classes or n-classes − 1 entries (the latter summing ≤ 1)"
          (classification-weights-fit? weights n-classes)]
         [(shift n-features) "#:shift list needs one entry per feature"
          (length-matches-features? shift n-features)]
         [(scale n-features) "#:scale list needs one entry per feature"
          (length-matches-features? scale n-features)]))
    (pattern (~datum make-regression)
      #:with (arg:argument ...)
      #'([n-samples 100 exact-positive-integer?]
         [n-features 100 exact-positive-integer?]
         [n-informative 10 exact-nonnegative-integer?]
         [n-targets 1 exact-positive-integer?]
         [bias 0.0 finite/c]
         [effective-rank #f (or/c #f exact-positive-integer?)]
         [tail-strength 0.5 probability/c]
         [noise 0.0 nonnegative/c]
         [shuffle? #t boolean?])
      #:with (pre:precondition ...) #'())
    (pattern (~datum make-friedman1)
      #:with (arg:argument ...)
      #'([n-samples 100 exact-positive-integer?]
         [n-features 10 (integer-in 5 #f)]
         [noise 0.0 nonnegative/c])
      #:with (pre:precondition ...) #'())
    (pattern (~or* (~datum make-friedman2) (~datum make-friedman3) (~datum make-s-curve))
      #:with (arg:argument ...)
      #'([n-samples 100 exact-positive-integer?] [noise 0.0 nonnegative/c])
      #:with (pre:precondition ...) #'())
    (pattern (~datum make-hastie-10-2)
      #:with (arg:argument ...) #'([n-samples 12000 exact-positive-integer?])
      #:with (pre:precondition ...) #'())
    (pattern (~datum make-swiss-roll)
      #:with (arg:argument ...)
      #'([n-samples 100 exact-positive-integer?]
         [noise 0.0 nonnegative/c]
         [hole? #f boolean?])
      #:with (pre:precondition ...) #'())
    (pattern (~datum make-gaussian-quantiles)
      #:with (arg:argument ...)
      #'([mean #f (or/c #f (non-empty-listof finite/c))]
         [cov 1.0 nonnegative/c]
         [n-samples 100 exact-positive-integer?]
         [n-features #f (or/c #f exact-positive-integer?)]
         [n-classes 3 exact-positive-integer?]
         [shuffle? #t boolean?])
      #:with (pre:precondition ...)
      #'([(n-features mean) "#:n-features must equal the length of #:mean"
          (features-match-dimension? n-features (and (pair? mean) (list mean)))]
         [(n-samples n-classes) "#:n-samples must be at least #:n-classes"
          (quantiles-samples-fit? n-samples n-classes)]))))

(define-syntax-parse-rule (synthetic/c g:generator (~optional (~and #:format with-format)))
  #:do [(define defaults
          (for/hasheq ([name (in-list (attribute g.arg.name))]
                       [default (in-list (attribute g.arg.default))])
            (values (syntax-e name) default)))]
  #:with ((pre-default ...) ...)
  (for/list ([ids (in-list (attribute g.pre.id))])
    (for/list ([id (in-list ids)]) (hash-ref defaults (syntax-e id))))
  #:with (format-domain ...)
  (if (attribute with-format) #'(#:format [format dataset-format/c]) #'())
  #:with result/c (if (attribute with-format) #'any/c #'dataset-table?)
  (->i ()
       (format-domain ... (~@ g.arg.keyword [g.arg.name g.arg.contract]) ... #:seed [seed seed/c])
       (~@ #:pre/name (g.pre.id ...) g.pre.message
           (let ([g.pre.id (given g.pre.id pre-default)] ...) g.pre.check)) ...
       [result result/c]))
