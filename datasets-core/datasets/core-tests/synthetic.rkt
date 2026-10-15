#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/list count remove-duplicates)
         (only-in racket/match match match-define match-lambda)
         (only-in racket/math exact-floor pi sgn sqr)
         (only-in racket/runtime-path define-runtime-path)
         (only-in datasets/core
                  dataset-table-column dataset-table-columns dataset-table-info dataset-table-names
                  dataset-table-row-count)
         (only-in datasets/core-private/random seeded)
         (only-in datasets/core-tests/plist apply/plist)
         (only-in datasets/synthetic
                  make-blobs make-circles make-classification make-friedman1 make-friedman2
                  make-friedman3 make-gaussian-quantiles make-hastie-10-2 make-moons make-regression
                  make-s-curve make-swiss-roll))
(define-runtime-path fixtures "fixtures")
(module+ test
  (require (only-in rackunit
                    check-= check-equal? check-not-equal? check-not-false check-regexp-match
                    check-true test-case)
           (only-in racket/contract/combinator exn:fail:contract:blame?))
  (define (col t n) (dataset-table-column t n))
  (define (cell t n i) (vector-ref (col t n) i))
  (define (info t) (dataset-table-info t))
  (define (source t) (hash-ref (info t) "source"))
  (define (derived t k) (hash-ref (hash-ref (source t) "derived") k))
  (define (features t) (filter (λ (n) (regexp-match? #rx"^x[0-9]+$" n)) (dataset-table-names t)))
  (define (roles t)
    (for/list ([m (hash-ref (info t) "columns")] #:when (regexp-match? #rx"^x" (hash-ref m "name")))
      (hash-ref m "description")))
  (define (label-counts t)
    (for/fold ([h (hash)]) ([v (in-vector (col t "y"))]) (hash-update h v add1 0)))
  (define (state g) (pseudo-random-generator->vector g))
  (define (dot u v) (for/sum ([a (in-vector u)] [b (in-vector v)]) (* a b)))
  (define (mean xs) (/ (apply + xs) (length xs)))
  (define (variance xs)
    (define m (mean xs))
    (/ (for/sum ([x xs]) (sqr (- x m))) (sub1 (length xs))))
  (define (deeply-immutable? v)
    (match v
      [(? hash?)
       (and (immutable? v)
            (for/and ([(k x) (in-hash v)]) (and (deeply-immutable? k) (deeply-immutable? x))))]
      [(cons a d) (and (deeply-immutable? a) (deeply-immutable? d))]
      [(? string?) (immutable? v)]
      [(? vector?) (and (immutable? v) (for/and ([x (in-vector v)]) (deeply-immutable? x)))]
      [_ #t]))
  (define (violation form)
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-attach-module (variable-reference->empty-namespace (#%variable-reference))
                               'datasets/synthetic)
      (eval `(module client racket/base (require datasets/synthetic) ,form))
      (with-handlers ([exn:fail:contract:blame? exn-message])
        (eval '(require 'client))
        "no violation")))
  (define small
    `((,make-blobs (#:n-samples 40)) (,make-moons (#:n-samples 40 #:noise 0.1))
      (,make-circles (#:n-samples 40 #:noise 0.1)) (,make-classification (#:n-samples 40))
      (,make-regression (#:n-samples 40 #:n-features 6 #:n-informative 3))
      (,make-regression (#:n-samples 40 #:n-features 6 #:effective-rank 2))
      (,make-friedman1 (#:n-samples 40 #:noise 1.0)) (,make-friedman2 (#:n-samples 40 #:noise 1.0))
      (,make-friedman3 (#:n-samples 40 #:noise 0.1)) (,make-hastie-10-2 (#:n-samples 40))
      (,make-swiss-roll (#:n-samples 40 #:noise 0.1 #:hole? #t))
      (,make-s-curve (#:n-samples 40 #:noise 0.1))
      (,make-gaussian-quantiles (#:n-samples 40))))

  (test-case "exact: names and row counts follow the arguments"
    (for ([c (in-list
              `((,make-blobs (#:n-samples 10 #:n-features 3 #:seed 0) 10 ("x0" "x1" "x2" "y"))
                (,make-blobs (#:n-samples (3 0 2) #:centers ((0 0) (1 1) (2 2)) #:seed 0)
                             5 ("x0" "x1" "y"))
                (,make-moons (#:n-samples (2 5) #:seed 0) 7 ("x0" "x1" "y"))
                (,make-circles (#:n-samples 0 #:seed 0) 0 ("x0" "x1" "y"))
                (,make-classification (#:n-samples 9 #:n-features 4 #:seed 0)
                                      9 ("x0" "x1" "x2" "x3" "y"))
                (,make-regression (#:n-samples 4 #:n-features 2 #:n-targets 3 #:seed 0)
                                  4 ("x0" "x1" "y0" "y1" "y2"))
                (,make-regression (#:n-samples 4 #:n-features 2 #:seed 0) 4 ("x0" "x1" "y"))
                (,make-friedman1 (#:n-samples 3 #:n-features 6 #:seed 0)
                                 3 ("x0" "x1" "x2" "x3" "x4" "x5" "y"))
                (,make-friedman2 (#:n-samples 3 #:seed 0) 3 ("x0" "x1" "x2" "x3" "y"))
                (,make-friedman3 (#:n-samples 3 #:seed 0) 3 ("x0" "x1" "x2" "x3" "y"))
                (,make-hastie-10-2 (#:n-samples 3 #:seed 0)
                                   3 ("x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "y"))
                (,make-swiss-roll (#:n-samples 3 #:seed 0) 3 ("x0" "x1" "x2" "t"))
                (,make-s-curve (#:n-samples 3 #:seed 0) 3 ("x0" "x1" "x2" "t"))
                (,make-gaussian-quantiles (#:n-samples 5 #:mean (1.0 2.0 3.0) #:seed 0)
                                          5 ("x0" "x1" "x2" "y"))))])
      (match-define (list gen args rows names) c)
      (define t (apply/plist gen args))
      (check-equal? (dataset-table-row-count t) rows)
      (check-equal? (dataset-table-names t) names)
      (for ([v (in-vector (dataset-table-columns t))]) (check-equal? (vector-length v) rows))))

  (test-case "exact: value types, immutability, metadata key set"
    (for ([c (in-list small)])
      (match-define (list gen args) c)
      (define t (apply/plist gen (append args '(#:seed 1))))
      (define i (info t))
      (check-equal? (sort (hash-keys i) string<?)
                    (sort '("name" "rows" "column-count" "columns" "identifiers" "suggested-targets"
                            "source-url" "source-version" "source-sha256" "sha256" "license"
                            "citation" "notes" "time-series" "source")
                          string<?))
      (check-true (deeply-immutable? i))
      (check-equal? (hash-ref i "name") (symbol->string (object-name gen)))
      (check-equal? (hash-ref (source t) "generator") (hash-ref i "name"))
      (check-equal? (hash-ref (source t) "stream") 1)
      (check-equal? (hash-ref i "rows") (dataset-table-row-count t))
      (check-equal? (hash-ref i "column-count") (length (dataset-table-names t)))
      (check-equal? (hash-ref i "identifiers") '())
      (check-equal? (hash-ref i "suggested-targets") (remove* (features t) (dataset-table-names t)))
      (for ([n (in-list (dataset-table-names t))] [m (in-list (hash-ref i "columns"))])
        (check-equal? (hash-ref m "name") n)
        (check-equal? (hash-ref m "missing-count") 0)
        (check-true (immutable? (col t n)))
        (match (hash-ref m "type")
          ["real" (check-true (for/and ([x (in-vector (col t n))]) (flonum? x)))]
          ["integer" (check-true (for/and ([x (in-vector (col t n))]) (exact-integer? x)))]))))

  (test-case "exact: seed forms agree, distinct seeds differ, metadata replays"
    (for ([c (in-list small)])
      (match-define (list gen args) c)
      (define (with seed) (apply/plist gen (append args (list '#:seed seed))))
      (define a (with 7))
      (check-equal? (with 7) a)
      (check-equal? (dataset-table-columns (with (seeded 7))) (dataset-table-columns a))
      (check-equal? (dataset-table-columns (with (state (seeded 7)))) (dataset-table-columns a))
      (check-not-equal? (dataset-table-columns (with 8)) (dataset-table-columns a))
      (for ([t (list a (apply/plist gen args) (with (seeded 9)))])
        (define replay
          (apply/plist gen (for*/list ([(k v) (in-hash (hash-ref (source t) "arguments"))]
                                       [x (list (string->keyword k) v)])
                             x)))
        (check-equal? replay t))))

  (test-case "exact: the current generator is never read or advanced"
    (for ([c (in-list small)])
      (match-define (list gen args) c)
      (define before (state (current-pseudo-random-generator)))
      (for ([seed (list #f 3 (seeded 3) (state (seeded 3)))])
        (apply/plist gen (append args (list '#:seed seed))))
      (check-equal? (state (current-pseudo-random-generator)) before))
    (define a (make-moons #:noise 0.1 #:seed 5))
    (parameterize ([current-pseudo-random-generator (seeded 2)])
      (check-equal? (make-moons #:noise 0.1 #:seed 5) a)))

  (test-case "exact: a supplied generator advances deterministically; draw-free calls leave it"
    (define g1 (seeded 4))
    (define g2 (seeded 4))
    (make-blobs #:seed g1)
    (make-blobs #:seed g2)
    (check-equal? (state g1) (state g2))
    (check-not-equal? (state g1) (state (seeded 4)))
    (define g3 (seeded 4))
    (make-moons #:shuffle? #f #:seed g3)
    (make-circles #:shuffle? #f #:seed g3)
    (check-equal? (state g3) (state (seeded 4))))

  (test-case "exact: zero noise and noise-only draws"
    (check-equal? (dataset-table-columns (make-moons #:noise 0.0 #:seed 1))
                  (dataset-table-columns (make-moons #:seed 1)))
    (define (pair gen) (values (gen #:noise 0.0 #:seed 2) (gen #:noise 1.0 #:seed 2)))
    (let-values ([(quiet loud) (pair make-friedman1)])
      (for ([n (features quiet)]) (check-equal? (col quiet n) (col loud n))))
    (for ([gen (list make-swiss-roll make-s-curve)])
      (define-values (quiet loud) (pair gen))
      (check-equal? (col quiet "t") (col loud "t")))
    (check-equal? (dataset-table-columns
                   (make-regression #:n-samples 20 #:n-features 3 #:noise (/ 1 (expt 10 400)) #:seed 0))
                  (dataset-table-columns (make-regression #:n-samples 20 #:n-features 3 #:seed 0))))

  (test-case "exact: class sizes"
    (check-equal? (label-counts (make-blobs #:n-samples 10 #:centers 3 #:seed 0))
                  (hash 0 4 1 3 2 3))
    (check-equal? (label-counts (make-blobs #:n-samples '(3 0 2) #:centers '((0 0) (1 1) (2 2))
                                            #:seed 0))
                  (hash 0 3 2 2))
    (check-equal? (label-counts (make-moons #:n-samples 7 #:seed 0)) (hash 0 3 1 4))
    (check-equal? (label-counts (make-circles #:n-samples '(4 1) #:seed 0)) (hash 0 4 1 1))
    (check-equal? (label-counts (make-gaussian-quantiles #:n-samples 10 #:seed 0))
                  (hash 0 3 1 3 2 4))
    (check-equal? (label-counts (make-classification #:n-samples 101 #:n-classes 3 #:n-informative 3
                                                     #:flip-y 0.0 #:seed 0))
                  (hash 0 34 1 34 2 33))
    (check-equal? (label-counts (make-classification #:n-samples 10 #:n-classes 3
                                                     #:n-clusters-per-class 1
                                                     #:weights '(0.5 0.25 0.25) #:flip-y 0.0
                                                     #:seed 0))
                  (hash 0 6 1 2 2 2)))

  (test-case "exact: shuffling permutes rows of the unshuffled table"
    (define (rows t)
      (sort (for/list ([i (dataset-table-row-count t)])
              (for/list ([v (in-vector (dataset-table-columns t))]) (vector-ref v i)))
            string<? #:key (λ (r) (format "~s" r))))
    (for ([gen (list make-blobs make-moons make-circles make-gaussian-quantiles)])
      (define s (gen #:n-samples 30 #:seed 3))
      (define u (gen #:n-samples 30 #:shuffle? #f #:seed 3))
      (check-equal? (rows s) (rows u))
      (check-not-equal? (col s "y") (col u "y"))))

  (test-case "exact: blobs sit on their centres at zero spread; drawn centres in the box"
    (define t (make-blobs #:n-samples 7 #:centers '((1 2) (-3 4.5)) #:cluster-std 0 #:seed 0))
    (for ([i 7])
      (check-equal? (list (cell t "x0" i) (cell t "x1" i))
                    (if (= 0 (cell t "y" i)) '(1.0 2.0) '(-3.0 4.5))))
    (define d (make-blobs #:centers 4 #:n-features 3 #:center-box '(-2.0 5.0) #:seed 0))
    (check-equal? (map length (derived d "centers")) '(3 3 3 3))
    (for* ([c (derived d "centers")] [x c]) (check-true (< -2.0 x 5.0)))
    (check-equal? (derived (make-blobs #:centers '((0 1)) #:seed 0) "centers") '((0.0 1.0))))

  (test-case "exact: classification roles, repeated columns, hypercube separation"
    (define all-roles
      '("informative" "informative" "informative" "redundant" "redundant" "repeated" "repeated"
        "random" "random"))
    (define (gen shuffle?)
      (make-classification #:n-samples 60 #:n-features 9 #:n-informative 3 #:n-redundant 2
                           #:n-repeated 2 #:shuffle? shuffle? #:seed 0))
    (check-equal? (roles (gen #f)) all-roles)
    (define t (gen #t))
    (check-equal? (sort (roles t) string<?) (sort all-roles string<?))
    (define (by role) (for/list ([n (features t)] [r (roles t)] #:when (equal? r role)) (col t n)))
    (for ([v (by "repeated")])
      (check-not-false (member v (append (by "informative") (by "redundant")))))
    (define s (make-classification #:n-samples 200 #:n-features 2 #:n-informative 2 #:n-redundant 0
                                   #:n-classes 4 #:n-clusters-per-class 1 #:class-sep 100.0
                                   #:flip-y 0.0 #:seed 0))
    (define patterns
      (for/list ([k 4])
        (remove-duplicates
         (for/list ([i 200] #:when (= k (cell s "y" i)))
           (map (λ (n) (sgn (cell s n i))) (features s))))))
    (check-true (andmap (λ (p) (= 1 (length p))) patterns))
    (check-equal? (length (remove-duplicates (apply append patterns))) 4))

  (test-case "tolerance 2% (fixed seed): without the hypercube, class means are rank one"
    (define t (make-classification #:n-samples 4000 #:n-features 2 #:n-informative 2
                                   #:n-redundant 0 #:n-classes 2 #:n-clusters-per-class 1
                                   #:class-sep 100.0 #:flip-y 0.0 #:hypercube? #f #:shuffle? #f
                                   #:seed 0))
    (match-define (list (list m00 m01) (list m10 m11))
      (for/list ([k 2])
        (for/list ([n (features t)])
          (mean (for/list ([x (in-vector (col t n))] [y (in-vector (col t "y"))] #:when (= y k))
                  x)))))
    (check-= (abs (* m00 m11)) (abs (* m01 m10)) (* 0.02 (abs (* m01 m10))))
    (check-true (for/or ([m (list m00 m01 m10 m11)]) (< (abs m) 90.0)))
    (check-true (> (abs (- 1 (abs (/ m00 m01)))) 0.1))
    (check-true (> (abs (- 1 (abs (/ m00 m10)))) 0.1)))

  (test-case "formula (1e-12): list #:shift and #:scale apply per feature"
    (define shift '(1.0 -2.0 0.5))
    (define scale '(2.0 0.5 3.0))
    (define (gen shift scale)
      (make-classification #:n-samples 20 #:n-features 3 #:n-informative 2 #:n-redundant 1
                           #:shift shift #:scale scale #:shuffle? #f #:seed 0))
    (define base (gen 0.0 1.0))
    (define moved (gen shift scale))
    (for ([n (features base)] [s shift] [c scale])
      (for ([a (in-vector (col moved n))] [b (in-vector (col base n))])
        (check-= a (* (+ b s) c) 1e-12))))

  (test-case "exact: Hastie labels follow the threshold; Gaussian quantiles are nested shells"
    (define h (make-hastie-10-2 #:n-samples 500 #:seed 0))
    (for ([i 500])
      (define sum-of-squares (for/sum ([n (features h)]) (sqr (cell h n i))))
      (check-equal? (cell h "y" i) (if (> sum-of-squares 9.34) 1 -1)))
    (define m '(1.0 -2.0 0.5))
    (define g (make-gaussian-quantiles #:mean m #:cov 2.0 #:n-samples 301 #:n-classes 4 #:seed 0))
    (define shells
      (for/list ([k 4])
        (for/list ([i 301] #:when (= k (cell g "y" i)))
          (for/sum ([n (features g)] [c m]) (sqr (- (cell g n i) c))))))
    (check-equal? (map length shells) '(75 75 75 76))
    (match-define (cons _ outer-shells) shells)
    (for ([inner shells] [outer outer-shells])
      (check-true (<= (apply max inner) (apply min outer)))))

  (test-case "exact: swiss-roll hole leaves the centre cell empty and fills the other eight"
    (define h (make-swiss-roll #:n-samples 2000 #:hole? #t #:seed 0))
    (define cells
      (for/list ([i 2000])
        (list (exact-floor (- (/ (cell h "t" i) pi) 1.5)) (exact-floor (/ (cell h "x1" i) 7.0)))))
    (check-equal? (sort (remove-duplicates cells) < #:key (match-lambda [(list i j) (+ (* 3 i) j)]))
                  '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2))))

  (test-case "formula (1e-12): curves without noise"
    (define m (make-moons #:n-samples 9 #:seed 0))
    (for ([i 9])
      (define x (cell m "x0" i))
      (define y (cell m "x1" i))
      (match (cell m "y" i)
        [0 (check-= (+ (sqr x) (sqr y)) 1.0 1e-12) (check-true (>= y 0.0))]
        [1 (check-= (+ (sqr (- x 1.0)) (sqr (- y 0.5))) 1.0 1e-12) (check-true (<= y 0.5))]))
    (define c (make-circles #:n-samples 11 #:factor 0.3 #:seed 0))
    (for ([i 11])
      (check-= (+ (sqr (cell c "x0" i)) (sqr (cell c "x1" i)))
               (if (= 0 (cell c "y" i)) 1.0 0.09)
               1e-12))
    (define r (make-swiss-roll #:n-samples 200 #:seed 0))
    (for ([i 200])
      (define t (cell r "t" i))
      (check-= (cell r "x0" i) (* t (cos t)) 1e-12)
      (check-= (cell r "x2" i) (* t (sin t)) 1e-12)
      (check-true (< (* 1.5 pi) t (* 4.5 pi)))
      (check-true (< 0.0 (cell r "x1" i) 21.0)))
    (define s (make-s-curve #:n-samples 200 #:seed 0))
    (for ([i 200])
      (define t (cell s "t" i))
      (check-= (cell s "x0" i) (sin t) 1e-12)
      (check-= (cell s "x2" i) (* (sgn t) (- (cos t) 1.0)) 1e-12)
      (check-true (< (* -1.5 pi) t (* 1.5 pi)))
      (check-true (< 0.0 (cell s "x1" i) 2.0))))

  (test-case "formula: Friedman responses and input ranges"
    (define f1 (make-friedman1 #:n-samples 50 #:n-features 7 #:seed 0))
    (for ([i 50])
      (define (x j) (cell f1 (format "x~a" j) i))
      (check-= (cell f1 "y" i)
               (+ (* 10 (sin (* pi (x 0) (x 1))))
                  (* 20 (sqr (- (x 2) 0.5)))
                  (* 10 (x 3))
                  (* 5 (x 4)))
               1e-12)
      (for ([j 7]) (check-true (< 0.0 (x j) 1.0))))
    (for ([gen (list make-friedman2 make-friedman3)] [tol '(1e-9 1e-12)])
      (define f (gen #:n-samples 50 #:seed 0))
      (for ([i 50])
        (match-define (list a b c d) (map (λ (n) (cell f n i)) (features f)))
        (check-true (and (< 0.0 a 100.0) (< (* 40 pi) b (* 560 pi)) (< 0.0 c 1.0) (< 1.0 d 11.0)))
        (define inner (- (* b c) (/ 1.0 (* b d))))
        (check-= (cell f "y" i)
                 (if (eq? gen make-friedman2) (sqrt (+ (sqr a) (sqr inner))) (atan (/ inner a)))
                 tol))))

  (test-case "formula: regression targets are X·coef + bias; coefficient support"
    (define t (make-regression #:n-samples 30 #:n-features 5 #:n-informative 3 #:n-targets 2
                               #:bias 4.5 #:seed 0))
    (define coef (derived t "coef"))
    (check-equal? (map length coef) '(2 2 2 2 2))
    (check-equal? (count (λ (row) (andmap positive? row)) coef) 3)
    (check-equal? (count (λ (row) (andmap zero? row)) coef) 2)
    (check-equal? (roles t) (map (λ (row) (if (andmap zero? row) "random" "informative")) coef))
    (for* ([i 30] [k 2])
      (check-= (cell t (format "y~a" k) i)
               (+ 4.5 (for/sum ([n (features t)] [row coef]) (* (cell t n i) (list-ref row k))))
               1e-9))
    (define clamped (make-regression #:n-features 3 #:n-informative 10 #:seed 0))
    (check-equal? (count (λ (row) (andmap positive? row)) (derived clamped "coef")) 3))

  (test-case "formula (1e-10): low-rank inputs carry the singular profile (Σs², Σs⁴)"
    (define t (make-regression #:n-samples 20 #:n-features 10 #:effective-rank 3 #:tail-strength 0.5
                               #:seed 0))
    (define s
      (for/list ([k 10]) (+ (* 0.5 (exp (- (sqr (/ k 3.0))))) (* 0.5 (exp (* -0.1 (/ k 3.0)))))))
    (define cols (map (λ (n) (col t n)) (features t)))
    (check-= (for/sum ([u cols]) (dot u u)) (for/sum ([x s]) (sqr x)) 1e-10)
    (check-= (for*/sum ([u cols] [v cols]) (sqr (dot u v))) (for/sum ([x s]) (expt x 4)) 1e-10))

  (test-case "tolerance 4 SE (fixed seeds): normal sampler, uniforms, noise, balance, centres"
    (define h (make-hastie-10-2 #:n-samples 20000 #:seed 0))
    (define zs (for*/list ([n (features h)] [x (in-vector (col h n))]) x))
    (define N (length zs))
    (check-= (mean zs) 0.0 (/ 4 (sqrt N)))
    (check-= (variance zs) 1.0 (* 4 (sqrt (/ 2 N))))
    (check-= (mean (map (λ (z) (expt z 4)) zs)) 3.0 (* 4 (sqrt (/ 96 N))))
    (check-= (/ (count (λ (z) (> (abs z) 1.959964)) zs) N) 0.05 (* 4 (sqrt (/ (* 0.05 0.95) N))))
    (match-define (cons _ later) zs)
    (check-= (/ (for/sum ([a zs] [b later]) (* a b)) (sub1 N)) 0.0 (/ 4 (sqrt N)))
    (check-= (mean (vector->list (col h "y"))) 0.0 (/ 4 (sqrt 20000)))
    (define f (make-friedman1 #:n-samples 20000 #:noise 2.0 #:seed 0))
    (define quiet (make-friedman1 #:n-samples 20000 #:seed 0))
    (check-= (mean (vector->list (col f "x0"))) 0.5 (* 4 (sqrt (/ 1/12 20000))))
    (check-= (sqrt (variance (for/list ([a (in-vector (col f "y"))] [b (in-vector (col quiet "y"))])
                               (- a b))))
             2.0
             (* 4 (/ 2.0 (sqrt 40000))))
    (define flipped (make-classification #:n-samples 4000 #:flip-y 1.0 #:seed 0))
    (check-= (/ (hash-ref (label-counts flipped) 1) 4000) 0.5 (* 4 (sqrt (/ 0.25 4000))))
    (define b (make-blobs #:n-samples 3000 #:centers 3 #:cluster-std '(0.5 1.0 2.0) #:seed 0))
    (for ([centre (derived b "centers")] [std '(0.5 1.0 2.0)] [k 3])
      (for ([n '("x0" "x1")] [c centre])
        (define xs
          (for/list ([x (in-vector (col b n))] [y (in-vector (col b "y"))] #:when (= y k)) x))
        (check-= (mean xs) c (* 4 (/ std (sqrt 1000))))
        (check-= (sqrt (variance xs)) std (* 4 (/ std (sqrt 2000)))))))

  (test-case "exact: invalid arguments are contract violations blaming the caller"
    (for ([form (in-list
                 '((make-blobs #:centers '((0 0) (1))) (make-blobs #:n-samples '(5 5) #:centers 3)
                   (make-blobs #:n-samples '(5 5) #:centers '((0 0)))
                   (make-blobs #:centers 2 #:cluster-std '(1.0))
                   (make-blobs #:n-features 3 #:centers '((0 0)))
                   (make-blobs #:center-box '(1.0 -1.0)) (make-blobs #:center-box '(-1e308 1e308))
                   (make-blobs #:cluster-std -1.0) (make-moons #:n-samples 0)
                   (make-moons #:noise -0.1)
                   (make-moons #:noise (expt 10 400))
                   (make-moons #:noise (- (/ 1 (expt 10 400))))
                   (make-classification #:flip-y (+ 1 (/ 1 (expt 10 400))))
                   (make-moons #:n-samples '(1 2 3)) (make-circles #:factor 1.0)
                   (make-circles #:factor (- 1 (/ 1 (expt 10 400))))
                   (make-classification #:scale (/ 1 (expt 10 400)))
                   (make-classification #:class-sep (/ 1 (expt 10 400)))
                   (make-classification #:n-features 3) (make-classification #:n-classes 3)
                   (make-classification #:n-classes 3 #:weights '(0.5))
                   (make-classification #:n-classes 3 #:weights '(0.7 0.6))
                   (make-classification #:weights '(1.5 0.5)) (make-classification #:class-sep 0.0)
                   (make-classification #:flip-y 1.5) (make-classification #:shift '(1.0 2.0))
                   (make-classification #:scale 0.0)
                   (make-classification #:n-informative 16 #:n-repeated 3)
                   (make-regression #:tail-strength 1.5) (make-regression #:effective-rank 0)
                   (make-regression #:noise +nan.0) (make-regression #:bias (expt 10 400))
                   (make-friedman1 #:n-features 4) (make-friedman2 #:noise +inf.0)
                   (make-friedman3 #:n-samples 1.5)
                   (make-hastie-10-2 #:n-samples 0) (make-swiss-roll #:hole? 'yes)
                   (make-s-curve #:seed -1)
                   (make-s-curve #:seed 2147483648) (make-moons #:seed (vector 1 2 3))
                   (make-gaussian-quantiles #:n-samples 2 #:n-classes 3)
                   (make-gaussian-quantiles #:n-samples 2)
                   (make-gaussian-quantiles #:mean '(0.0) #:n-features 2)
                   (make-gaussian-quantiles #:cov -1.0)))])
      (check-regexp-match #rx"blaming: [^\n]*client" (violation form))))

  (test-case "exact: preconditions see the generators' defaults at the boundary"
    (define widest (make-classification #:n-informative 16 #:n-repeated 2 #:seed 0))
    (check-equal? (length (features widest)) 20)
    (check-equal? (dataset-table-row-count (make-gaussian-quantiles #:n-samples 3 #:seed 0)) 3))

  (test-case "golden (stream 1; integers exact, flonums 1e-12 relative; runs on Linux and macOS CI)"
    (for ([entry (call-with-input-file (build-path fixtures "synthetic-golden.rktd") read)])
      (match-define (list name args columns) entry)
      (define t (apply/plist (dynamic-require 'datasets/synthetic name) args))
      (check-equal? (sort (dataset-table-names t) string<?) (sort (hash-keys columns) string<?))
      (for ([(n expected) (in-hash columns)])
        (check-equal? (vector-length (col t n)) (length expected) (format "~a ~a" name n))
        (for ([a (in-vector (col t n))] [b (in-list expected)])
          (if (exact-integer? b) (check-equal? a b) (check-= a b (* 1e-12 (max 1.0 (abs b)))))))))

  (test-case "parity: deterministic parts match scikit-learn 1.8.0"
    (define fx (call-with-input-file (build-path fixtures "sklearn-synthetic.rktd") read))
    (check-equal? (hash-ref fx "scikit-learn") "1.8.0")
    (for ([c (hash-ref fx "cases")])
      (match c
        [(list id 'low-rank-profile args (hash-table ("singular-values" s)))
         (define t (apply/plist make-regression (append args '(#:seed 0))))
         (define cols (map (λ (n) (col t n)) (features t)))
         (check-= (for/sum ([u cols]) (dot u u)) (for/sum ([x s]) (sqr x)) 1e-10 (format "~a Σs²" id))
         (check-= (for*/sum ([u cols] [v cols]) (sqr (dot u v)))
                  (for/sum ([x s]) (expt x 4))
                  1e-10
                  (format "~a Σs⁴" id))]
        [(list id generator args expected)
         (define t (apply/plist (dynamic-require 'datasets/synthetic generator)
                                (append args '(#:seed 0))))
         (for ([(n vs) (in-hash expected)])
           (check-equal? (vector-length (col t n)) (length vs) (format "~a ~a" id n))
           (for ([a (in-vector (col t n))] [b (in-list vs)])
             (if (exact-integer? b) (check-equal? a b) (check-= a b 1e-14))))]))))
