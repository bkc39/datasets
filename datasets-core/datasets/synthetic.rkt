#lang racket/base
(require (only-in racket/contract/base contract-out)
         (only-in racket/flonum fl* fl+ fl- fl/ fl< fl> flatan flcos flexp flsin flsqrt)
         (only-in racket/list make-list)
         (only-in racket/math exact-truncate pi sgn)
         (only-in racket/vector vector-append vector-map vector-sort)
         (only-in datasets/core-private/dense
                  draw-rows linspace map-rows orthonormal-columns permute row-times rows->columns
                  rows-times)
         (only-in datasets/core-private/random
                  normal-source permutation random-index uniform uniform-in)
         (only-in datasets/core-private/synthetic-contracts synthetic/c)
         (only-in datasets/core-private/synthetic-table define-generator generated target))
(provide
 (contract-out
  [make-blobs (synthetic/c make-blobs)]
  [make-moons (synthetic/c make-moons)]
  [make-circles (synthetic/c make-circles)]
  [make-classification (synthetic/c make-classification)]
  [make-regression (synthetic/c make-regression)]
  [make-friedman1 (synthetic/c make-friedman1)]
  [make-friedman2 (synthetic/c make-friedman2)]
  [make-friedman3 (synthetic/c make-friedman3)]
  [make-hastie-10-2 (synthetic/c make-hastie-10-2)]
  [make-swiss-roll (synthetic/c make-swiss-roll)]
  [make-s-curve (synthetic/c make-s-curve)]
  [make-gaussian-quantiles (synthetic/c make-gaussian-quantiles)]))

;; Every generator mirrors the draw order of scikit-learn 1.8's NumPy calls, including
;; draws whose values are then multiplied by zero (stream 1).

(define (label-target labels description) (target "y" "y" "integer" description labels))
(define (real-target name original-name description values)
  (target name original-name "real" description values))

(define (shuffle-rows g rows labels)
  (define order (permutation g (vector-length rows)))
  (values (permute rows order) (permute labels order)))

(define (maybe-shuffle g shuffle? rows labels)
  (if shuffle? (shuffle-rows g rows labels) (values rows labels)))

(define (add-noise! rows scale normal)
  (for* ([row (in-vector rows)] [j (in-range (vector-length row))])
    (vector-set! row j (fl+ (vector-ref row j) (fl* scale (normal))))))

(define (points angles x y)
  (for/vector #:length (vector-length angles) ([θ (in-vector angles)]) (vector (x θ) (y θ))))

;; Noise is drawn after the shuffle, over the shuffled rows.
(define (two-curves g n-samples shuffle? noise outer inner)
  (define n-out (if (pair? n-samples) (car n-samples) (quotient n-samples 2)))
  (define n-in (if (pair? n-samples) (cadr n-samples) (- n-samples n-out)))
  (define-values (rows labels)
    (maybe-shuffle g shuffle? (vector-append (outer n-out) (inner n-in))
                   (vector-append (make-vector n-out 0) (make-vector n-in 1))))
  (when noise (add-noise! rows (real->double-flonum noise) (normal-source g)))
  (generated (rows->columns rows 2) '("coordinate" "coordinate") (list (label-target labels "curve label"))
             (hash)))

(define-generator (make-moons [n-samples 100] [shuffle? #t] [noise #f])
  #:stream g
  (two-curves g n-samples shuffle? noise
              (λ (n) (points (linspace 0.0 pi n #:endpoint? #t) flcos flsin))
              (λ (n) (points (linspace 0.0 pi n #:endpoint? #t)
                             (λ (θ) (fl- 1.0 (flcos θ)))
                             (λ (θ) (fl- (fl- 1.0 (flsin θ)) 0.5))))))

(define-generator (make-circles [n-samples 100] [shuffle? #t] [noise #f] [factor 0.8])
  #:stream g
  (define r (real->double-flonum factor))
  (define (angles n) (linspace 0.0 (fl* 2.0 pi) n #:endpoint? #f))
  (two-curves g n-samples shuffle? noise
              (λ (n) (points (angles n) flcos flsin))
              (λ (n) (points (angles n) (λ (θ) (fl* (flcos θ) r)) (λ (θ) (fl* (flsin θ) r))))))

(define-generator (make-blobs [n-samples 100] [n-features #f] [centers #f] [cluster-std 1.0]
                              [center-box '(-10.0 10.0)] [shuffle? #t])
  #:stream g
  (define n-centres
    (cond [(pair? centers) (length centers)]
          [(exact-integer? centers) centers]
          [(pair? n-samples) (length n-samples)]
          [else 3]))
  (define dimension (if (pair? centers) (length (car centers)) (or n-features 2)))
  (define centres
    (if (pair? centers)
        (for/vector ([c (in-list centers)]) (vector-map real->double-flonum (list->vector c)))
        (draw-rows n-centres dimension
                   (λ () (uniform-in g (real->double-flonum (car center-box)) (real->double-flonum (cadr center-box)))))))
  (define counts
    (if (pair? n-samples)
        n-samples
        (for/list ([i (in-range n-centres)])
          (+ (quotient n-samples n-centres) (if (< i (remainder n-samples n-centres)) 1 0)))))
  (define stds (if (pair? cluster-std) (map real->double-flonum cluster-std) (make-list n-centres (real->double-flonum cluster-std))))
  (define normal (normal-source g))
  (define-values (rows labels)
    (maybe-shuffle
     g shuffle?
     (apply vector-append
            (for/list ([centre (in-vector centres)] [n (in-list counts)] [std (in-list stds)])
              (for/vector #:length n ([_ (in-range n)])
                (for/vector #:length dimension ([c (in-vector centre)]) (fl+ c (fl* std (normal)))))))
     (apply vector-append (for/list ([n (in-list counts)] [k (in-naturals)]) (make-vector n k)))))
  (generated (rows->columns rows dimension)
             (make-list dimension "cluster coordinate")
             (list (label-target labels "cluster label"))
             (hash "centers" (for/list ([c (in-vector centres)]) (vector->list c)))))

;; `count` distinct vertices of the unit hypercube in `dimension` dimensions, each a vector
;; of 0.0/1.0 with the most significant bit first. Past 30 dimensions the leading
;; coordinates are free random bits, as in scikit-learn.
(define (hypercube-vertices g count dimension)
  (cond
    [(> dimension 30)
     (define free (draw-rows count (- dimension 30) (λ () (real->double-flonum (random-index g 2)))))
     (for/vector ([bits (in-vector free)] [vertex (in-vector (hypercube-vertices g count 30))])
       (vector-append bits vertex))]
    [else
     (define population (expt 2 dimension))
     (define (unseen seen)
       (define v (random-index g population))
       (if (hash-ref seen v #f) (unseen seen) v))
     (define chosen
       (for/fold ([seen (hash)] [order '()] #:result (reverse order)) ([_ (in-range count)])
         (define v (unseen seen))
         (values (hash-set seen v #t) (cons v order))))
     (for/vector ([v (in-list chosen)])
       (for/vector ([bit (in-range (sub1 dimension) -1 -1)]) (if (bitwise-bit-set? v bit) 1.0 0.0)))]))

(define (class-weights weights n-classes)
  (cond [(not weights) (make-list n-classes (fl/ 1.0 (real->double-flonum n-classes)))]
        [(= (length weights) n-classes) (map real->double-flonum weights)]
        [else (define ws (map real->double-flonum weights))
              (append ws (list (fl- 1.0 (for/fold ([s 0.0]) ([w (in-list ws)]) (fl+ s w)))))]))

(define (cluster-sizes n-samples weights n-clusters-per-class n-clusters)
  (define base
    (for/list ([k (in-range n-clusters)])
      (exact-truncate (fl/ (fl* (real->double-flonum n-samples) (list-ref weights (modulo k (length weights))))
                           (real->double-flonum n-clusters-per-class)))))
  (define extra (max 0 (- n-samples (apply + base))))
  (for/list ([size (in-list base)] [k (in-naturals)])
    (+ size (quotient extra n-clusters) (if (< k (remainder extra n-clusters)) 1 0))))

(define (per-feature v n-features draw)
  (cond [(pair? v) (list->vector (map real->double-flonum v))]
        [v (make-vector n-features (real->double-flonum v))]
        [else (for/vector #:length n-features ([_ (in-range n-features)]) (draw))]))

(define-generator (make-classification [n-samples 100] [n-features 20] [n-informative 2]
                                       [n-redundant 2] [n-repeated 0] [n-classes 2]
                                       [n-clusters-per-class 2] [weights #f] [flip-y 0.01]
                                       [class-sep 1.0] [hypercube? #t] [shift 0.0] [scale 1.0]
                                       [shuffle? #t])
  #:stream g
  (define sep (real->double-flonum class-sep))
  (define n-clusters (* n-classes n-clusters-per-class))
  (define n-useful (+ n-informative n-redundant))
  (define n-random (- n-features n-useful n-repeated))
  (define sizes (cluster-sizes n-samples (class-weights weights n-classes) n-clusters-per-class n-clusters))
  (define vertices (hypercube-vertices g n-clusters n-informative))
  (define by-cluster (and (not hypercube?) (draw-rows 1 n-clusters (λ () (uniform g)))))
  (define by-feature (and (not hypercube?) (draw-rows 1 n-informative (λ () (uniform g)))))
  (define centroids
    (for/vector #:length n-clusters ([vertex (in-vector vertices)] [k (in-naturals)])
      (for/vector #:length n-informative ([b (in-vector vertex)] [j (in-naturals)])
        (define c (fl- (fl* b (fl* 2.0 sep)) sep))
        (if hypercube?
            c
            (fl* (fl* c (vector-ref (vector-ref by-cluster 0) k)) (vector-ref (vector-ref by-feature 0) j))))))
  (define normal (normal-source g))
  (define standard (draw-rows n-samples n-informative normal))
  (define bounds
    (for/fold ([start 0] [acc '()] #:result (reverse acc)) ([size (in-list sizes)])
      (define stop (min n-samples (+ start size)))
      (values stop (cons (cons (min start n-samples) stop) acc))))
  (define informative
    (apply vector-append
           (for/list ([bound (in-list bounds)] [centroid (in-vector centroids)])
             (define a (draw-rows n-informative n-informative (λ () (fl- (fl* 2.0 (uniform g)) 1.0))))
             (for/vector ([x (in-vector standard (car bound) (cdr bound))])
               (vector-map fl+ (row-times x a n-informative) centroid)))))
  (define labels
    (apply vector-append
           (for/list ([bound (in-list bounds)] [k (in-naturals)])
             (make-vector (- (cdr bound) (car bound)) (modulo k n-classes)))))
  (define redundant
    (if (positive? n-redundant)
        (rows-times informative (draw-rows n-informative n-redundant (λ () (fl- (fl* 2.0 (uniform g)) 1.0)))
                    n-redundant)
        (make-vector n-samples (vector))))
  (define useful (for/vector ([x (in-vector informative)] [r (in-vector redundant)]) (vector-append x r)))
  (define repeated
    (for/list ([_ (in-range n-repeated)])
      (exact-truncate (fl+ (fl* (real->double-flonum (sub1 n-useful)) (uniform g)) 0.5))))
  (define random-part (if (positive? n-random) (draw-rows n-samples n-random normal) (make-vector n-samples (vector))))
  (define features
    (for/vector ([u (in-vector useful)] [r (in-vector random-part)])
      (vector-append u (for/vector ([j (in-list repeated)]) (vector-ref u j)) r)))
  (define flips (for/vector #:length n-samples ([_ (in-range n-samples)]) (fl< (uniform g) (real->double-flonum flip-y))))
  (define flipped
    (for/vector #:length n-samples ([label (in-vector labels)] [flip? (in-vector flips)])
      (if flip? (random-index g n-classes) label)))
  (define shifts (per-feature shift n-features (λ () (fl* (fl- (fl* 2.0 (uniform g)) 1.0) sep))))
  (define scales (per-feature scale n-features (λ () (fl+ 1.0 (fl* 100.0 (uniform g))))))
  (define shifted
    (map-rows (λ (row) (for/vector ([x (in-vector row)] [s (in-vector shifts)] [c (in-vector scales)])
                         (fl* (fl+ x s) c)))
              features))
  (define-values (rows shuffled-labels) (maybe-shuffle g shuffle? shifted flipped))
  (define order (if shuffle? (permutation g n-features) (build-vector n-features values)))
  (generated (for/list ([j (in-vector order)]) (for/vector ([row (in-vector rows)]) (vector-ref row j)))
             (for/list ([j (in-vector order)])
               (cond [(< j n-informative) "informative"]
                     [(< j n-useful) "redundant"]
                     [(< j (+ n-useful n-repeated)) "repeated"]
                     [else "random"]))
             (list (label-target shuffled-labels "class label"))
             (hash)))

(define (low-rank-rows g normal n-samples n-features effective-rank tail-strength)
  (define n (min n-samples n-features))
  (define (orthonormal m) (orthonormal-columns (rows->columns (draw-rows m n normal) n)))
  (define u (orthonormal n-samples))
  (define v (orthonormal n-features))
  (define tail (real->double-flonum tail-strength))
  (define rank (real->double-flonum effective-rank))
  (define s
    (for/list ([k (in-range n)])
      (define x (fl/ (real->double-flonum k) rank))
      (fl+ (fl* (fl- 1.0 tail) (flexp (fl* -1.0 (fl* x x)))) (fl* tail (flexp (fl* -0.1 x))))))
  (for/vector #:length n-samples ([i (in-range n-samples)])
    (for/vector #:length n-features ([j (in-range n-features)])
      (for/fold ([acc 0.0]) ([uk (in-list u)] [vk (in-list v)] [sk (in-list s)])
        (fl+ acc (fl* (fl* (vector-ref uk i) sk) (vector-ref vk j)))))))

(define-generator (make-regression [n-samples 100] [n-features 100] [n-informative 10] [n-targets 1]
                                   [bias 0.0] [effective-rank #f] [tail-strength 0.5] [noise 0.0]
                                   [shuffle? #t])
  #:stream g
  (define informative (min n-features n-informative))
  (define normal (normal-source g))
  (define x
    (if effective-rank
        (low-rank-rows g normal n-samples n-features effective-rank tail-strength)
        (draw-rows n-samples n-features normal)))
  (define coef
    (vector-append (draw-rows informative n-targets (λ () (fl* 100.0 (uniform g))))
                   (draw-rows (- n-features informative) n-targets (λ () 0.0))))
  (define y (map-rows (λ (row) (vector-map (λ (v) (fl+ v (real->double-flonum bias))) row)) (rows-times x coef n-targets)))
  (when (positive? noise) (add-noise! y (real->double-flonum noise) normal))
  (define-values (rows targets) (maybe-shuffle g shuffle? x y))
  (define order (if shuffle? (permutation g n-features) (build-vector n-features values)))
  (define coef-rows (permute coef order))
  (generated (for/list ([j (in-vector order)]) (for/vector ([row (in-vector rows)]) (vector-ref row j)))
             (for/list ([c (in-vector coef-rows)])
               (if (for/and ([v (in-vector c)]) (zero? v)) "random" "informative"))
             (for/list ([column (in-list (rows->columns targets n-targets))] [k (in-naturals)])
               (if (= n-targets 1)
                   (real-target "y" "y" "linear response" column)
                   (real-target (format "y~a" k) (format "y[:, ~a]" k) "linear response" column)))
             (hash "coef" (for/list ([c (in-vector coef-rows)]) (vector->list c)))))

(define (with-noise g noise n-samples response)
  (define normal (normal-source g))
  (define scale (real->double-flonum noise))
  (for/vector #:length n-samples ([r (in-vector response)]) (fl+ r (fl* scale (normal)))))

(define-generator (make-friedman1 [n-samples 100] [n-features 10] [noise 0.0])
  #:stream g
  (define x (draw-rows n-samples n-features (λ () (uniform g))))
  (define response
    (map-rows (λ (row)
                (define (v j) (vector-ref row j))
                (fl+ (fl+ (fl+ (fl* 10.0 (flsin (fl* (fl* pi (v 0)) (v 1))))
                               (fl* 20.0 (let ([d (fl- (v 2) 0.5)]) (fl* d d))))
                          (fl* 10.0 (v 3)))
                     (fl* 5.0 (v 4))))
              x))
  (generated (rows->columns x n-features) (make-list n-features "uniform on [0, 1)")
             (list (real-target "y" "y" "Friedman #1 response" (with-noise g noise n-samples response)))
             (hash)))

(define (friedman-inputs g n-samples)
  (map-rows (λ (row)
              (vector (fl* (vector-ref row 0) 100.0)
                      (fl+ (fl* (vector-ref row 1) (fl* 520.0 pi)) (fl* 40.0 pi))
                      (vector-ref row 2)
                      (fl+ (fl* (vector-ref row 3) 10.0) 1.0)))
            (draw-rows n-samples 4 (λ () (uniform g)))))

(define friedman-descriptions
  '("uniform on [0, 100)" "uniform on [40π, 560π)" "uniform on [0, 1)" "uniform on [1, 11)"))

(define (friedman-term row)
  (define x1 (vector-ref row 1))
  (fl- (fl* x1 (vector-ref row 2)) (fl/ 1.0 (fl* x1 (vector-ref row 3)))))

(define-generator (make-friedman2 [n-samples 100] [noise 0.0])
  #:stream g
  (define x (friedman-inputs g n-samples))
  (define response
    (map-rows (λ (row)
                (define x0 (vector-ref row 0))
                (define t (friedman-term row))
                (flsqrt (fl+ (fl* x0 x0) (fl* t t))))
              x))
  (generated (rows->columns x 4) friedman-descriptions
             (list (real-target "y" "y" "Friedman #2 response" (with-noise g noise n-samples response)))
             (hash)))

(define-generator (make-friedman3 [n-samples 100] [noise 0.0])
  #:stream g
  (define x (friedman-inputs g n-samples))
  (define response (map-rows (λ (row) (flatan (fl/ (friedman-term row) (vector-ref row 0)))) x))
  (generated (rows->columns x 4) friedman-descriptions
             (list (real-target "y" "y" "Friedman #3 response" (with-noise g noise n-samples response)))
             (hash)))

(define-generator (make-hastie-10-2 [n-samples 12000])
  #:stream g
  (define x (draw-rows n-samples 10 (normal-source g)))
  (define labels
    (map-rows (λ (row) (if (fl> (for/fold ([s 0.0]) ([v (in-vector row)]) (fl+ s (fl* v v))) 9.34) 1 -1)) x))
  (generated (rows->columns x 10) (make-list 10 "standard normal")
             (list (label-target labels "1 when the sum of squares exceeds 9.34, else -1"))
             (hash)))

;; Noise for the manifolds is drawn coordinate-major, as NumPy fills a (3, n) array.
(define (manifold g noise t columns)
  (define normal (normal-source g))
  (define scale (real->double-flonum noise))
  (generated (for/list ([column (in-list columns)]) (vector-map (λ (v) (fl+ v (fl* scale (normal)))) column))
             (make-list 3 "manifold coordinate")
             (list (real-target "t" "t" "position along the manifold" t))
             (hash)))

(define-generator (make-swiss-roll [n-samples 100] [noise 0.0] [hole? #f])
  #:stream g
  (define (uniforms) (for/vector #:length n-samples ([_ (in-range n-samples)]) (uniform g)))
  (define-values (t y)
    (cond
      [hole?
       (define corners
         (for*/vector ([i (in-range 3)] [j (in-range 3)] #:unless (= i j 1))
           (vector (fl* pi (fl+ 1.5 (real->double-flonum i))) (fl* (real->double-flonum j) 7.0))))
       (define index (for/vector #:length n-samples ([_ (in-range n-samples)]) (random-index g 8)))
       (define along (uniforms))
       (define across (uniforms))
       (values (for/vector ([k (in-vector index)] [u (in-vector along)]) (fl+ (vector-ref (vector-ref corners k) 0) (fl* u pi)))
               (for/vector ([k (in-vector index)] [u (in-vector across)]) (fl+ (vector-ref (vector-ref corners k) 1) (fl* u 7.0))))]
      [else
       (define t (vector-map (λ (u) (fl* (fl* 1.5 pi) (fl+ 1.0 (fl* 2.0 u)))) (uniforms)))
       (values t (vector-map (λ (u) (fl* 21.0 u)) (uniforms)))]))
  (define x0 (vector-map (λ (s) (fl* s (flcos s))) t))
  (define x2 (vector-map (λ (s) (fl* s (flsin s))) t))
  (manifold g noise t (list x0 y x2)))

(define-generator (make-s-curve [n-samples 100] [noise 0.0])
  #:stream g
  (define t (for/vector #:length n-samples ([_ (in-range n-samples)]) (fl* (fl* 3.0 pi) (fl- (uniform g) 0.5))))
  (define x1 (for/vector #:length n-samples ([_ (in-range n-samples)]) (fl* 2.0 (uniform g))))
  (manifold g noise t (list (vector-map flsin t) x1 (vector-map (λ (s) (fl* (sgn s) (fl- (flcos s) 1.0))) t))))

(define-generator (make-gaussian-quantiles [mean #f] [cov 1.0] [n-samples 100] [n-features #f]
                                           [n-classes 3] [shuffle? #t])
  #:stream g
  (define dimension (cond [mean (length mean)] [else (or n-features 2)]))
  (define centre (if mean (list->vector (map real->double-flonum mean)) (make-vector dimension 0.0)))
  (define spread (flsqrt (real->double-flonum cov)))
  (define x
    (map-rows (λ (z) (for/vector #:length dimension ([zj (in-vector z)] [c (in-vector centre)]) (fl+ c (fl* spread zj))))
              (draw-rows n-samples dimension (normal-source g))))
  (define (distance row)
    (for/fold ([s 0.0]) ([v (in-vector row)] [c (in-vector centre)]) (fl+ s (fl* (fl- v c) (fl- v c)))))
  (define sorted (vector-sort x fl< #:key distance #:cache-keys? #t))
  (define step (quotient n-samples n-classes))
  (define labels (for/vector #:length n-samples ([i (in-range n-samples)]) (min (quotient i step) (sub1 n-classes))))
  (define-values (rows shuffled-labels) (maybe-shuffle g shuffle? sorted labels))
  (generated (rows->columns rows dimension) (make-list dimension "Gaussian coordinate")
             (list (label-target shuffled-labels "quantile shell, innermost first"))
             (hash)))
