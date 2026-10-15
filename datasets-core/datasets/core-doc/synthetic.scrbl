#lang scribble/manual
@(require scribble/eval
          (for-label racket/base racket/contract/base racket/random datasets/core datasets/synthetic))
@(define ev (make-base-eval))
@(ev '(require datasets/core datasets/synthetic))

@title[#:tag "datasets-synthetic-reference"]{Synthetic Data}
@defmodule[datasets/synthetic]

Each generator mirrors the scikit-learn 1.8 function of the same name and returns
a @tech{dataset table}. Features are named @racket["x0"], @racket["x1"], …, and
hold flonums. The target is @racket["y"]; a regression with several targets has
@racket["y0"], @racket["y1"], …, and the two manifolds have @racket["t"]. Class
labels are exact integers.

Keywords follow scikit-learn's parameters: underscores become hyphens, boolean
parameters gain a @litchar{?}, and @tt{random_state} becomes @racket[#:seed].

@tabular[#:sep @hspace[2]
 (list (list @bold{Procedure} @bold{scikit-learn} @bold{Differences})
       (list @racket[make-blobs] @tt{make_blobs}
             @elem{@racket[#:n-features] defaults to @racket[#f]; centres in metadata, not @tt{return_centers}})
       (list @racket[make-moons] @tt{make_moons} "none")
       (list @racket[make-circles] @tt{make_circles} "none")
       (list @racket[make-classification] @tt{make_classification}
             @elem{feature roles in column descriptions, not @tt{return_X_y}})
       (list @racket[make-regression] @tt{make_regression}
             @elem{coefficients in metadata, not @tt{coef}})
       (list @racket[make-friedman1] @tt{make_friedman1} "none")
       (list @racket[make-friedman2] @tt{make_friedman2} "none")
       (list @racket[make-friedman3] @tt{make_friedman3} "none")
       (list @racket[make-hastie-10-2] @tt{make_hastie_10_2} @elem{labels are exact @racket[-1] and @racket[1]})
       (list @racket[make-swiss-roll] @tt{make_swiss_roll} "none")
       (list @racket[make-s-curve] @tt{make_s_curve} "none")
       (list @racket[make-gaussian-quantiles] @tt{make_gaussian_quantiles}
             @elem{@racket[#:n-features] defaults to @racket[#f]}))]

Arguments scikit-learn accepts but ignores, such as an @racket[#:n-features] that
disagrees with the given centres, are contract violations here. Numbers must be
finite.

@section[#:tag "datasets-synthetic-seeds"]{Seeds and reproducibility}

@racket[#:seed] takes one of four forms:

@itemlist[
 @item{@racket[#f]: a fresh seed in [0, 2@superscript{31}) is drawn from
       @racket[crypto-random-bytes] and recorded in the metadata.}
 @item{an integer @racket[_k]: a new generator in the state @racket[(random-seed _k)] produces.}
 @item{a state vector, as from @racket[pseudo-random-generator->vector]: a new generator in that state.}
 @item{a pseudo-random generator: used and advanced in place.}]

@racket[current-pseudo-random-generator] is never read or advanced. On one
platform, the same arguments and seed give @racket[equal?] tables. Across
platforms, labels and uniform-only columns are bit-identical; values passed
through @racket[log], @racket[exp], @racket[sin], @racket[cos] or @racket[atan]
agree to a relative 10@superscript{-12}. The stream is not NumPy's: a seed
does not reproduce scikit-learn's values.

The @tt{source} metadata's @tt{arguments} hash records every keyword with the
seed that reproduces the table, so passing it back replays the table.

@examples[#:eval ev
 (define a (make-moons #:n-samples 4 #:noise 0.1 #:seed 7))
 (equal? a (make-moons #:n-samples 4 #:noise 0.1 #:seed 7))
 (hash-ref (hash-ref (dataset-table-info a) "source") "arguments")]

@section[#:tag "datasets-synthetic-metadata"]{Metadata}

Columns are the features, then the targets. Features have type @tt{real} and
original name @tt{X[:, j]}. Class labels have type @tt{integer}; other targets
have type @tt{real} and original name @tt{y}, @tt{y[:, k]} or @tt{t}. Every
column has no levels, no unit and no missing values. Descriptions of
@racket[make-classification] features are scikit-learn's @tt{feature_info}
roles: @tt{informative}, @tt{redundant}, @tt{repeated} or @tt{random};
@racket[make-regression] features are @tt{informative} when their coefficient
is nonzero and @tt{random} otherwise.

The info hash is deeply immutable and has exactly these keys:

@tabular[#:sep @hspace[2]
 (list (list @bold{Key} @bold{Value})
       (list @tt{name} @elem{the generator's name, such as @racket["make-blobs"]})
       (list @elem{@tt{rows}, @tt{column-count}, @tt{columns}} "as for registry datasets")
       (list @tt{identifiers} @elem{@racket['()]})
       (list @tt{suggested-targets} "every target column, in order")
       (list @tt{source-url} "the scikit-learn 1.8 page for the function")
       (list @tt{source-version} @elem{@racket["scikit-learn 1.8.0 semantics; stream 1, not NumPy-compatible"]})
       (list @elem{@tt{source-sha256}, @tt{sha256}, @tt{license}, @tt{time-series}} @elem{@racket[#f]})
       (list @tt{citation} "the reference scikit-learn cites for the generator")
       (list @tt{notes} "one sentence")
       (list @tt{source} "the hash below"))]

The @tt{source} hash has @tt{generator} (the name), @tt{reference} (such as
@racket["sklearn.datasets.make_blobs"]), @tt{reference-version}
(@racket["1.8.0"]), @tt{stream} (@racket[1]), @tt{arguments} and @tt{derived}.
@tt{arguments} maps each keyword name, without @litchar{#:}, to its value as
given or defaulted; the seed entry is the integer that reproduces the table, or
for a generator or vector its state before the first draw. @tt{derived} holds
@tt{centers} for @racket[make-blobs] and @tt{coef} for @racket[make-regression],
and is empty otherwise. Generated tables are not registry entries.

@section[#:tag "datasets-synthetic-generators"]{Generators}

@defproc[(make-blobs [#:n-samples n-samples (or/c exact-positive-integer?
                                                  (and/c (non-empty-listof exact-nonnegative-integer?)
                                                         (λ (ns) (positive? (apply + ns)))))
                                  100]
                     [#:n-features n-features (or/c #f exact-positive-integer?) #f]
                     [#:centers centers (or/c #f exact-positive-integer?
                                              (non-empty-listof (non-empty-listof rational?)))
                                #f]
                     [#:cluster-std cluster-std (or/c (and/c rational? (>=/c 0))
                                                      (non-empty-listof (and/c rational? (>=/c 0))))
                                    1.0]
                     [#:center-box center-box (list/c rational? rational?) '(-10.0 10.0)]
                     [#:shuffle? shuffle? boolean? #t]
                     [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                        pseudo-random-generator? pseudo-random-generator-vector?)
                             #f])
         dataset-table?]{
Isotropic Gaussian clusters with standard deviation @racket[cluster-std] (one
per centre when a list). An integer @racket[n-samples] is split as evenly as
possible, earlier centres taking the remainder; a list gives per-centre counts
and needs @racket[centers] to be @racket[#f] or one centre per count. An integer
or @racket[#f] @racket[centers] (3 centres) draws the centres uniformly from
@racket[center-box], whose lower bound must not exceed its upper; the centres
are recorded under @tt{derived} @racket["centers"]. Explicit centres must have
the same dimension, which @racket[n-features], when given, must equal;
otherwise there are 2 features.

@examples[#:eval ev
 (define b (make-blobs #:n-samples 6 #:centers 2 #:seed 0))
 (dataset-table-names b)
 (hash-ref (hash-ref (hash-ref (dataset-table-info b) "source") "derived") "centers")]}

@defproc[(make-moons [#:n-samples n-samples (or/c exact-positive-integer?
                                                  (list/c exact-nonnegative-integer? exact-nonnegative-integer?))
                                  100]
                     [#:shuffle? shuffle? boolean? #t]
                     [#:noise noise (or/c #f (and/c rational? (>=/c 0))) #f]
                     [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                        pseudo-random-generator? pseudo-random-generator-vector?)
                             #f])
         dataset-table?]{
Two interleaving half circles. Label @racket[0] gets ⌊@racket[n-samples]/2⌋
points and label @racket[1] the rest; a pair gives the (outer, inner) counts.
Gaussian noise of standard deviation @racket[noise] is added to both features;
@racket[#f] draws nothing.

@examples[#:eval ev
 (dataset-table-column (make-moons #:n-samples 5 #:shuffle? #f) "y")]}

@defproc[(make-circles [#:n-samples n-samples (or/c exact-nonnegative-integer?
                                                    (list/c exact-nonnegative-integer? exact-nonnegative-integer?))
                                    100]
                       [#:shuffle? shuffle? boolean? #t]
                       [#:noise noise (or/c #f (and/c rational? (>=/c 0))) #f]
                       [#:factor factor (and/c rational? (>=/c 0) (</c 1)) 0.8]
                       [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                          pseudo-random-generator? pseudo-random-generator-vector?)
                               #f])
         dataset-table?]{
A unit circle (label @racket[0]) containing a circle of radius @racket[factor]
(label @racket[1]). Angles are evenly spaced with the endpoint excluded. Counts
and noise are as for @racket[make-moons].}

@defproc[(make-classification [#:n-samples n-samples exact-positive-integer? 100]
                              [#:n-features n-features exact-positive-integer? 20]
                              [#:n-informative n-informative exact-positive-integer? 2]
                              [#:n-redundant n-redundant exact-nonnegative-integer? 2]
                              [#:n-repeated n-repeated exact-nonnegative-integer? 0]
                              [#:n-classes n-classes exact-positive-integer? 2]
                              [#:n-clusters-per-class n-clusters-per-class exact-positive-integer? 2]
                              [#:weights weights (or/c #f (listof (and/c rational? (real-in 0 1)))) #f]
                              [#:flip-y flip-y (and/c rational? (real-in 0 1)) 0.01]
                              [#:class-sep class-sep (and/c rational? (>/c 0)) 1.0]
                              [#:hypercube? hypercube? boolean? #t]
                              [#:shift shift (or/c #f rational? (non-empty-listof rational?)) 0.0]
                              [#:scale scale (or/c #f (and/c rational? (>/c 0))
                                                   (non-empty-listof (and/c rational? (>/c 0))))
                                       1.0]
                              [#:shuffle? shuffle? boolean? #t]
                              [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                                 pseudo-random-generator? pseudo-random-generator-vector?)
                                      #f])
         dataset-table?]{
Guyon's design: Gaussian clusters at vertices of a hypercube of side
2·@racket[class-sep] in the informative features, then redundant features
(random linear combinations of the informative ones), repeated features (copies),
random features, label flips (each label is redrawn with probability
@racket[flip-y]), @racket[shift], @racket[scale], and shuffling. A @racket[#f]
@racket[shift] or @racket[scale] is drawn per feature. Informative, redundant
and repeated features must fit in @racket[n-features], and
@racket[n-classes]·@racket[n-clusters-per-class] vertices in
2@superscript{@racket[n-informative]}. @racket[weights] gives the class
proportions, with @racket[n-classes] entries, or @racket[n-classes]−1 entries
summing to at most 1 with the last class taking the rest; a list
@racket[shift] or @racket[scale] has one entry per feature. Column descriptions
name each feature's role.

@examples[#:eval ev
 (for/list ([c (hash-ref (dataset-table-info (make-classification #:n-features 5 #:seed 0)) "columns")])
   (hash-ref c "description"))]}

@defproc[(make-regression [#:n-samples n-samples exact-positive-integer? 100]
                          [#:n-features n-features exact-positive-integer? 100]
                          [#:n-informative n-informative exact-nonnegative-integer? 10]
                          [#:n-targets n-targets exact-positive-integer? 1]
                          [#:bias bias rational? 0.0]
                          [#:effective-rank effective-rank (or/c #f exact-positive-integer?) #f]
                          [#:tail-strength tail-strength (and/c rational? (real-in 0 1)) 0.5]
                          [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                          [#:shuffle? shuffle? boolean? #t]
                          [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                             pseudo-random-generator? pseudo-random-generator-vector?)
                                  #f])
         dataset-table?]{
A linear model: targets are the features times a coefficient matrix plus
@racket[bias] plus Gaussian noise of standard deviation @racket[noise].
min(@racket[n-informative], @racket[n-features]) features have coefficients
uniform in [0, 100), the rest zero; the coefficients are recorded under
@tt{derived} @racket["coef"], one list of @racket[n-targets] per feature in
column order. Features are standard normal, or, when @racket[effective-rank] is
given, a matrix whose singular values follow scikit-learn's
@tt{make_low_rank_matrix} profile with that rank and @racket[tail-strength].

@examples[#:eval ev
 (hash-ref (hash-ref (hash-ref (dataset-table-info (make-regression #:n-samples 3 #:n-features 3
                                                                    #:n-informative 1 #:seed 0))
                               "source")
                     "derived")
           "coef")]}

@defproc[(make-friedman1 [#:n-samples n-samples exact-positive-integer? 100]
                         [#:n-features n-features (integer-in 5 #f) 10]
                         [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                         [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                            pseudo-random-generator? pseudo-random-generator-vector?)
                                 #f])
         dataset-table?]{
Features uniform on [0, 1); y = 10 sin(π x0 x1) + 20 (x2 − 0.5)@superscript{2}
+ 10 x3 + 5 x4 + @racket[noise]·N(0, 1). Features beyond x4 are unused.}

@defproc[(make-friedman2 [#:n-samples n-samples exact-positive-integer? 100]
                         [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                         [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                            pseudo-random-generator? pseudo-random-generator-vector?)
                                 #f])
         dataset-table?]{
x0 uniform on [0, 100), x1 on [40π, 560π), x2 on [0, 1), x3 on [1, 11);
y = (x0@superscript{2} + (x1 x2 − 1/(x1 x3))@superscript{2})@superscript{1/2}
+ @racket[noise]·N(0, 1).}

@defproc[(make-friedman3 [#:n-samples n-samples exact-positive-integer? 100]
                         [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                         [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                            pseudo-random-generator? pseudo-random-generator-vector?)
                                 #f])
         dataset-table?]{
Inputs as for @racket[make-friedman2];
y = atan((x1 x2 − 1/(x1 x3))/x0) + @racket[noise]·N(0, 1).}

@defproc[(make-hastie-10-2 [#:n-samples n-samples exact-positive-integer? 12000]
                           [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                              pseudo-random-generator? pseudo-random-generator-vector?)
                                   #f])
         dataset-table?]{
Ten standard normal features; y is @racket[1] when the sum of their squares
exceeds 9.34 and @racket[-1] otherwise.}

@defproc[(make-swiss-roll [#:n-samples n-samples exact-positive-integer? 100]
                          [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                          [#:hole? hole? boolean? #f]
                          [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                             pseudo-random-generator? pseudo-random-generator-vector?)
                                  #f])
         dataset-table?]{
t uniform on [1.5π, 4.5π), x0 = t cos t, x1 uniform on [0, 21), x2 = t sin t,
each feature plus @racket[noise]·N(0, 1). With @racket[hole?], (t, x1) is drawn
from the eight outer cells of a 3×3 grid on [1.5π, 4.5π) × [0, 21), leaving the
centre cell empty.}

@defproc[(make-s-curve [#:n-samples n-samples exact-positive-integer? 100]
                       [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                       [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                          pseudo-random-generator? pseudo-random-generator-vector?)
                               #f])
         dataset-table?]{
t uniform on [−1.5π, 1.5π), x0 = sin t, x1 uniform on [0, 2), x2 = sgn(t)(cos t − 1),
each feature plus @racket[noise]·N(0, 1).}

@defproc[(make-gaussian-quantiles [#:mean mean (or/c #f (non-empty-listof rational?)) #f]
                                  [#:cov cov (and/c rational? (>=/c 0)) 1.0]
                                  [#:n-samples n-samples exact-positive-integer? 100]
                                  [#:n-features n-features (or/c #f exact-positive-integer?) #f]
                                  [#:n-classes n-classes exact-positive-integer? 3]
                                  [#:shuffle? shuffle? boolean? #t]
                                  [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                                     pseudo-random-generator? pseudo-random-generator-vector?)
                                          #f])
         dataset-table?]{
Gaussian points with mean @racket[mean] (zeros when @racket[#f]) and covariance
@racket[cov] times the identity, sorted by squared distance from the mean and cut
into @racket[n-classes] nested shells of ⌊@racket[n-samples]/@racket[n-classes]⌋
points, the last class taking the remainder. @racket[n-features], when given,
must equal the length of @racket[mean]; otherwise there are 2 features.
@racket[n-samples] must be at least @racket[n-classes].

@examples[#:eval ev
 (dataset-table-column (make-gaussian-quantiles #:n-samples 7 #:n-classes 3 #:shuffle? #f #:seed 0) "y")]}

@(close-eval ev)
