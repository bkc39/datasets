#lang racket/base
(require (for-syntax racket/base (only-in syntax/parse ...+ attribute expr id))
         (only-in racket/match match)
         (only-in syntax/parse/define define-syntax-parse-rule)
         (only-in datasets/core-private/random seed->generator)
         (only-in datasets/core-private/synthetic-contracts generator)
         (only-in datasets/core-private/table dataset-table))
(provide define-generator (struct-out generated) (struct-out target))

(struct generated (features descriptions targets derived))
(struct target (name original-name type description values))

(define (text . parts) (string->immutable-string (apply string-append parts)))

(define pedregosa
  (text "F. Pedregosa et al., \"Scikit-learn: Machine Learning in Python\", "
        "Journal of Machine Learning Research 12, pages 2825-2830, 2011."))
(define friedman
  (text "J. Friedman, \"Multivariate adaptive regression splines\", "
        "The Annals of Statistics 19 (1), pages 1-67, 1991."))

(define citations
  (hash "make-classification"
        "I. Guyon, \"Design of experiments for the NIPS 2003 variable selection benchmark\", 2003."
        "make-friedman1" friedman "make-friedman2" friedman "make-friedman3" friedman
        "make-hastie-10-2"
        (text "T. Hastie, R. Tibshirani and J. Friedman, "
              "\"Elements of Statistical Learning Ed. 2\", Springer, 2009, Example 10.2.")
        "make-swiss-roll"
        (text "S. Marsland, \"Machine Learning: An Algorithmic Perspective\", "
              "2nd edition, Chapter 6, 2014.")
        "make-gaussian-quantiles"
        (text "J. Zhu, H. Zou, S. Rosset, T. Hastie, \"Multi-class AdaBoost\", "
              "Statistics and its Interface 2 (3), pages 349-360, 2009.")))

(define notes
  (hash "make-blobs"
        "Isotropic Gaussian clusters; the centres are recorded under source.derived.centers."
        "make-moons" "Two interleaving half circles."
        "make-circles" "A large circle containing a smaller one."
        "make-classification"
        (text "Gaussian clusters at hypercube vertices with redundant, repeated and random "
              "features; column descriptions give each feature's role.")
        "make-regression"
        "A random linear model; the coefficients are recorded under source.derived.coef."
        "make-friedman1" "Friedman's first regression problem."
        "make-friedman2" "Friedman's second regression problem."
        "make-friedman3" "Friedman's third regression problem."
        "make-hastie-10-2"
        "Hastie, Tibshirani and Friedman's binary problem; labels are -1 and 1."
        "make-swiss-roll"
        (text "A two-dimensional manifold rolled up in three dimensions; "
              "t is the position along the roll.")
        "make-s-curve"
        (text "A two-dimensional manifold bent into an S in three dimensions; "
              "t is the position along the curve.")
        "make-gaussian-quantiles"
        "Nested Gaussian shells cut at quantiles of the distance from the mean."))

(define (column-info name original-name type description)
  (hash "name" name "original-name" original-name "type" type "levels" '() "unit" #f
        "missing-count" 0 "description" description))

(define (generated->table name arguments result)
  (define reference (string-append "make_" (regexp-replace* #rx"-" (substring name 5) "_")))
  (define feature-names
    (for/list ([j (in-range (length (generated-features result)))])
      (string->immutable-string (format "x~a" j))))
  (define targets (generated-targets result))
  (define target-names (map (λ (t) (string->immutable-string (target-name t))) targets))
  (define names (append feature-names target-names))
  (define columns
    (append (for/list ([j (in-naturals)]
                       [n (in-list feature-names)]
                       [d (in-list (generated-descriptions result))])
              (column-info n (string->immutable-string (format "X[:, ~a]" j)) "real" d))
            (for/list ([t (in-list targets)] [n (in-list target-names)])
              (column-info n (string->immutable-string (target-original-name t)) (target-type t)
                           (target-description t)))))
  (define data
    (append (map vector->immutable-vector (generated-features result))
            (map (λ (t) (vector->immutable-vector (target-values t))) targets)))
  (define rows (match data ['() 0] [(cons column _) (vector-length column)]))
  (define info
    (hash "name" name "rows" rows "column-count" (length names) "columns" columns
          "identifiers" '() "suggested-targets" target-names
          "source-url"
          (string->immutable-string
           (string-append "https://scikit-learn.org/1.8/modules/generated/sklearn.datasets."
                          reference ".html"))
          "source-version" "scikit-learn 1.8.0 semantics; stream 1, not NumPy-compatible"
          "source-sha256" #f "sha256" #f "license" #f "time-series" #f
          "citation" (hash-ref citations name pedregosa) "notes" (hash-ref notes name)
          "source" (hash "generator" name
                         "reference"
                         (string->immutable-string (string-append "sklearn.datasets." reference))
                         "reference-version" "1.8.0" "stream" 1 "arguments" arguments
                         "derived" (generated-derived result))))
  (dataset-table names (vector->immutable-vector (list->vector data)) rows info))

(define-syntax-parse-rule (define-generator name:generator #:stream stream:id body:expr ...+)
  #:with label (datum->syntax #'name (symbol->string (syntax-e #'name)))
  #:with ((arg key) ...)
  (for/list ([a (in-list (attribute name.arg.name))])
    (define datum (syntax-e a))
    (list (datum->syntax #'name datum) (datum->syntax #'name (symbol->string datum))))
  (define (name (~@ name.arg.keyword [arg name.arg.default]) ... #:seed [seed #f])
    (define-values (stream seed-value) (seed->generator seed))
    (define arguments (hash (~@ key arg) ... "seed" seed-value))
    (generated->table label arguments (let () body ...))))
