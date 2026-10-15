#lang scribble/manual
@(require (for-label racket/base racket/contract/base datasets datasets/polars
                     datasets/matrix datasets/data-frame (prefix-in s: datasets/synthetic)))

@title[#:tag "datasets-reference"]{Reference}

This section defines the loaders and conversion procedures. For worked examples,
see the @secref["datasets-guide"]. Contracts use the notation described in
@other-doc['(lib "scribblings/reference/reference.scrbl")].

@section[#:tag "datasets-loaders-reference"]{Dataset loaders}
@defmodule[datasets]

All bindings from @racketmodname[datasets/core] are re-exported. Their definitions
are in @other-doc['(lib "datasets/core-doc/datasets-core.scrbl")].

@defproc[(load-dataset [name symbol?]
                       [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                       [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{
Loads @racket[name], selects columns, and converts the result to @racket[format].
The return value is a Polars dataframe, an immutable dataset table, a
@tt{math/matrix} matrix, or a @tt{data-frame} object as specified by the format.

When @racket[columns] is @racket[#f], all columns are selected in source order.
Otherwise it must be a nonempty list of distinct known column names. Selection
preserves the requested order and all source rows, and takes place before
conversion. An unknown dataset, format, or column, an empty selection, or a
duplicate column raises @racket[exn:fail:contract].

Each dataframe load allocates independent storage. Mutating a returned dataframe
cannot change later loads or the bundled observations. Table values are immutable.
Matrix conversion raises an error for any selected nonnumeric or missing value;
see @secref["datasets-matrix-reference"].}

@subsection[#:tag "datasets-named-loaders-reference"]{Named loaders}

Each named loader delegates to @racket[load-dataset] with its corresponding
dataset identifier. The keywords, defaults, selection rules, errors, and result
types are the same. Variable descriptions and provenance appear in the
@secref["datasets-catalog" #:doc '(lib "datasets/core-doc/datasets-core.scrbl")].
@defproc[(load-mtcars [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads mtcars.}
@defproc[(load-iris [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads corrected UCI Iris.}
@defproc[(load-faithful [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads faithful.}
@defproc[(load-anscombe [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads the wide Anscombe quartet.}
@defproc[(load-airquality [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads airquality, preserving missing values.}
@defproc[(load-us-arrests [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads US arrests with state identifiers.}
@defproc[(load-plant-growth [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads plant growth.}
@defproc[(load-tooth-growth [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads tooth growth.}
@defproc[(load-titanic [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads the contingency cells, including zero counts.}
@defproc[(load-air-passengers [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads monthly counts in chronological order.}
@defproc[(load-diabetes [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads the standardized lars representation.}
@defproc[(load-breast-cancer [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads UCI diagnostic measurements, M/B labels, and identifiers.}
@defproc[(load-wine [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars] [#:columns columns (or/c #f (non-empty-listof string?)) #f]) any/c]{Loads wine measurements and cultivar labels.}

@section[#:tag "datasets-synthetic-formats-reference"]{Synthetic generators}
@declare-exporting[datasets]

These procedures take the arguments of the same-named generators of
@racketmodname[datasets/synthetic], documented in
@secref["datasets-synthetic-reference" #:doc '(lib "datasets/core-doc/datasets-core.scrbl")],
plus @racket[#:format], and convert the generated table as @racket[load-dataset]
does, under the same argument bounds, which must hold for each value as a flonum.
Every generated table is numeric, so all four formats apply.
@racketmodname[datasets] does not re-export @racketmodname[datasets/synthetic]:
the names are the same, so a module that requires both gets an import conflict
and should use @racket[prefix-in] or @racket[only-in] for one of them.

@defproc[(make-blobs [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                     [#:n-samples n-samples (or/c exact-positive-integer?
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
         any/c]{
Generates as @racket[s:make-blobs] and converts the table as @racket[load-dataset] does.}

@defproc[(make-moons [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                     [#:n-samples n-samples (or/c exact-positive-integer?
                                                  (list/c exact-nonnegative-integer? exact-nonnegative-integer?))
                                  100]
                     [#:shuffle? shuffle? boolean? #t]
                     [#:noise noise (or/c #f (and/c rational? (>=/c 0))) #f]
                     [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                        pseudo-random-generator? pseudo-random-generator-vector?)
                             #f])
         any/c]{
Generates as @racket[s:make-moons] and converts the table as @racket[load-dataset] does.}

@defproc[(make-circles [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                       [#:n-samples n-samples (or/c exact-nonnegative-integer?
                                                    (list/c exact-nonnegative-integer? exact-nonnegative-integer?))
                                    100]
                       [#:shuffle? shuffle? boolean? #t]
                       [#:noise noise (or/c #f (and/c rational? (>=/c 0))) #f]
                       [#:factor factor (and/c rational? (>=/c 0) (</c 1)) 0.8]
                       [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                          pseudo-random-generator? pseudo-random-generator-vector?)
                               #f])
         any/c]{
Generates as @racket[s:make-circles] and converts the table as @racket[load-dataset] does.}

@defproc[(make-classification [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                              [#:n-samples n-samples exact-positive-integer? 100]
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
         any/c]{
Generates as @racket[s:make-classification] and converts the table as @racket[load-dataset] does.}

@defproc[(make-regression [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                          [#:n-samples n-samples exact-positive-integer? 100]
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
         any/c]{
Generates as @racket[s:make-regression] and converts the table as @racket[load-dataset] does.}

@defproc[(make-friedman1 [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                         [#:n-samples n-samples exact-positive-integer? 100]
                         [#:n-features n-features (integer-in 5 #f) 10]
                         [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                         [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                            pseudo-random-generator? pseudo-random-generator-vector?)
                                 #f])
         any/c]{
Generates as @racket[s:make-friedman1] and converts the table as @racket[load-dataset] does.}

@defproc[(make-friedman2 [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                         [#:n-samples n-samples exact-positive-integer? 100]
                         [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                         [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                            pseudo-random-generator? pseudo-random-generator-vector?)
                                 #f])
         any/c]{
Generates as @racket[s:make-friedman2] and converts the table as @racket[load-dataset] does.}

@defproc[(make-friedman3 [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                         [#:n-samples n-samples exact-positive-integer? 100]
                         [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                         [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                            pseudo-random-generator? pseudo-random-generator-vector?)
                                 #f])
         any/c]{
Generates as @racket[s:make-friedman3] and converts the table as @racket[load-dataset] does.}

@defproc[(make-hastie-10-2 [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                           [#:n-samples n-samples exact-positive-integer? 12000]
                           [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                              pseudo-random-generator? pseudo-random-generator-vector?)
                                   #f])
         any/c]{
Generates as @racket[s:make-hastie-10-2] and converts the table as @racket[load-dataset] does.}

@defproc[(make-swiss-roll [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                          [#:n-samples n-samples exact-positive-integer? 100]
                          [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                          [#:hole? hole? boolean? #f]
                          [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                             pseudo-random-generator? pseudo-random-generator-vector?)
                                  #f])
         any/c]{
Generates as @racket[s:make-swiss-roll] and converts the table as @racket[load-dataset] does.}

@defproc[(make-s-curve [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                       [#:n-samples n-samples exact-positive-integer? 100]
                       [#:noise noise (and/c rational? (>=/c 0)) 0.0]
                       [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                          pseudo-random-generator? pseudo-random-generator-vector?)
                               #f])
         any/c]{
Generates as @racket[s:make-s-curve] and converts the table as @racket[load-dataset] does.}

@defproc[(make-gaussian-quantiles [#:format format (or/c 'polars 'table 'matrix 'data-frame) 'polars]
                                  [#:mean mean (or/c #f (non-empty-listof rational?)) #f]
                                  [#:cov cov (and/c rational? (>=/c 0)) 1.0]
                                  [#:n-samples n-samples exact-positive-integer? 100]
                                  [#:n-features n-features (or/c #f exact-positive-integer?) #f]
                                  [#:n-classes n-classes exact-positive-integer? 3]
                                  [#:shuffle? shuffle? boolean? #t]
                                  [#:seed seed (or/c #f (integer-in 0 (sub1 (expt 2 31)))
                                                     pseudo-random-generator? pseudo-random-generator-vector?)
                                          #f])
         any/c]{
Generates as @racket[s:make-gaussian-quantiles] and converts the table as @racket[load-dataset] does.}

@section[#:tag "datasets-formats-reference"]{Value representations}

@tabular[#:sep @hspace[2]
 (list (list @bold{Value} @bold{Table} @bold{Polars} @bold{Matrix} @bold{data-frame})
       (list "Integer" "Exact integer" "Int64, or Float64 in a mixed numeric column" "Number" "Number")
       (list "Real" "Inexact real" "Float64" "Number" "Number")
       (list "Category / identifier" "String" "String" "Error" "String")
       (list "Missing" "dataset-missing" "polars-null" "Error" "Explicit series NA"))]

Category levels and their order remain in the source metadata; the Polars adapter
creates string columns rather than categorical or enum columns. No adapter
encodes labels, imputes missing observations, or drops selected columns. The
@tt{data-frame} adapter records initial column order in a property because its
underlying library does not define an iteration order for column names.

@section[#:tag "datasets-polars-reference"]{Polars conversion}
@defmodule[datasets/polars]

@defproc[(table->polars [table dataset-table?]) any/c]{
Constructs a Polars dataframe from the selected columns of @racket[table]. Each
column is newly allocated. String columns, including labels and identifiers,
remain strings. A column of exact integers uses Int64; other numeric columns use
Float64, with exact values converted to inexact values for that constructor.
@racket[dataset-missing] becomes @tt{polars-null}. Column and row order are
preserved. Source metadata is not attached to the dataframe; use
@racket[dataset-info] or @racket[dataset-table-info] to retain it.}

@section[#:tag "datasets-matrix-reference"]{Matrix conversion}
@defmodule[datasets/matrix]

@defproc[(table->matrix [table dataset-table?]) any/c]{
Constructs a @tt{math/matrix} matrix with one row per observation and one column
per selected variable, in table order. Every value must be numeric and nonmissing.
A nonnumeric or missing value raises @racket[exn:fail:contract] identifying the
column, zero-based row, and offending value. Labels and identifiers are not
encoded. The matrix does not carry column names or source metadata.}

@section[#:tag "datasets-data-frame-reference"]{Data-frame conversion}
@defmodule[datasets/data-frame]

@defproc[(table->data-frame [table dataset-table?]) any/c]{
Creates a @tt{data-frame} object with a copied vector for each selected column.
Each series uses @racket[dataset-missing] as its NA value. The property
@racket['datasets:columns] stores the table's ordered column names.
Changing values in this frame does not change @racket[table] or another load.}

@defproc[(data-frame-column-names [frame any/c]) (listof string?)]{
Returns the initial selection order stored in @racket[frame]'s
@racket['datasets:columns] property. The argument must be a @tt{data-frame}
object created by @racket[table->data-frame] or a loader using
@racket[#:format] @racket['data-frame]. A frame without the property raises
@racket[exn:fail:contract].

This is a snapshot of the selection at construction time. Callers that add,
remove, or rename columns must update the property themselves if they continue
to use it. The underlying @tt{df-series-names} procedure has unspecified order.}
