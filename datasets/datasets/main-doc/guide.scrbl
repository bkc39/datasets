#lang scribble/manual
@(require scribble/eval
          (for-label racket/base datasets datasets/data-frame
                     math/matrix (prefix-in pl: (except-in polars series-ref ~>)) (prefix-in df: data-frame)))
@(define ev (make-base-eval))

@title[#:tag "datasets-guide"]{Guide}

This guide starts with a dataframe and then shows how to select columns, choose
another representation, and interpret missing values and metadata. If you already
know which procedure you need, turn to the @secref["datasets-reference"].

@section[#:tag "datasets-install"]{Installation and a first dataset}

Use Racket 9.3 or later. Install from the Racket catalog:

@commandline{raco pkg install --auto datasets}

This also installs @tt{datasets-core} and the adapter dependencies.
From a checkout of the repository, install both packages from its root:

@verbatim{raco pkg install ./datasets-core
raco pkg install --auto ./datasets}

The two top-level directories are independently installable multi-collection
packages. @tt{datasets-core} provides @racketmodname[datasets/core];
@tt{datasets} adds the convenient loaders and adapters to the same collection.
The Polars package supplies its native library. With Nix, @tt{nix develop}
prepares an isolated environment containing both packages and that library.

@examples[#:eval ev
 (require datasets (prefix-in pl: polars))
 (define iris (load-iris))
 (pl:dataframe-height iris)
 (pl:dataframe-column-names iris)]

The prefix keeps Polars operations distinct from Racket's arithmetic and other
similarly named procedures. @racket[load-iris] returns 150 rows: four flower
measurements and a species label. Each call creates an independent dataframe.
Data is bundled with the package, so loading does not download anything.

@section[#:tag "datasets-discovery-guide"]{Discovering datasets and selecting columns}

@racket[dataset-names] lists the available identifiers. A named loader, such as
@racket[load-mtcars], is shorthand for @racket[(load-dataset 'mtcars)]. Both accept
@racket[#:columns] and @racket[#:format].

@examples[#:eval ev
 (dataset-names)
 (define cars (load-dataset 'mtcars #:columns '("model" "mpg")))
 (pl:dataframe-column-names cars)
 (hash-ref (dataset-info 'mtcars) "identifiers")]

Column names are strings. A selection preserves the order you give and retains
all source rows. Identifiers such as @racket["model"] are real columns, so they
can be kept for display or left out of a numeric matrix. Duplicate names, empty
selections, and unknown columns are reported as errors.

@section[#:tag "datasets-categories-guide"]{Grouping categorical data}

Category labels remain strings in Polars. For example, Iris species labels can
be used directly in a group operation:

@examples[#:eval ev
 (define by-species
   (pl:~> iris
          (pl:group-by "species")
          (pl:agg (pl:mean (pl:col "sepal-length")))))
 (pl:dataframe-height by-species)
 (pl:dataframe-column-names by-species)]

The result has one row for each species. Grouped output need not follow category
order; the intended level order is recorded separately in metadata:

@examples[#:eval ev
 (define species-info
   (list-ref (hash-ref (dataset-info 'iris) "columns") 4))
 (hash-ref species-info "levels")]

Keeping labels avoids introducing arbitrary numeric codes. See
@secref["datasets-formats-reference"] for the representation of each value type.

@section[#:tag "datasets-missing-guide"]{Handling missing observations}

Airquality has missing measurements. The table format uses
@racket[dataset-missing]; the Polars adapter translates that singleton to
@racket[pl:polars-null].

@examples[#:eval ev
 (define air (load-airquality))
 (pl:polars-null? (pl:series-ref (pl:dataframe-column air "ozone") 4))
 (define air-table (load-airquality #:format 'table))
 (define ozone (dataset-table-column air-table "ozone"))
 (for/sum ([value (in-vector ozone)])
   (if (dataset-missing? value) 1 0))]

Missing values are distinct from zero and @racket[#f]. Decide whether to exclude,
replace, or otherwise account for them in your analysis. The loaders preserve
them; matrix conversion reports a missing value instead of silently removing it.

@section[#:tag "datasets-matrices-guide"]{Preparing a numeric matrix}

Choose @racket['matrix] for a @racketmodname[math/matrix] matrix. Rows are
observations, and the requested column order determines the matrix's columns.
For Iris, select the four measurements explicitly to leave out the species label:

@examples[#:eval ev
 (require math/matrix)
 (define measurements
   (load-iris #:format 'matrix
              #:columns '("sepal-length" "sepal-width"
                          "petal-length" "petal-width")))
 (matrix-num-rows measurements)
 (matrix-num-cols measurements)
 (matrix-ref measurements 0 0)]

Trying to include a label or a missing observation produces an error naming the
column and the zero-based row:

@examples[#:eval ev
 (eval:error (load-iris #:format 'matrix #:columns '("species")))
 (eval:error (load-airquality #:format 'matrix #:columns '("ozone")))]

Diabetes already has ten standardized predictors in the @tt{lars}
representation. See its entry in the
@secref["datasets-catalog" #:doc '(lib "datasets/core-doc/datasets-core.scrbl")]
before comparing it with another library's dataset; raw measurements and exact
scikit-learn parity are not promised.

@section[#:tag "datasets-data-frame-guide"]{Using mutable data-frame objects}

The @racket['data-frame] format works with the Racket @racketmodname[data-frame]
library. Its NA value is explicitly set to @racket[dataset-missing].

@examples[#:eval ev
 (require (prefix-in df: data-frame) datasets/data-frame)
 (define frame (load-airquality #:format 'data-frame
                                #:columns '("ozone" "temp")))
 (data-frame-column-names frame)
 (df:df-is-na? frame "ozone" (df:df-ref frame 4 "ozone"))]

The underlying library stores columns by name and does not promise an order from
@racket[df:df-series-names]. @racket[data-frame-column-names] retrieves the initial
selection order recorded by this adapter.

Changes to one frame do not affect a later load:

@examples[#:eval ev
 (define first (load-iris #:format 'data-frame))
 (df:df-set! first 0 -100 "sepal-length")
 (df:df-ref first 0 "sepal-length")
 (df:df-ref (load-iris #:format 'data-frame) 0 "sepal-length")]

@section[#:tag "datasets-observations-guide"]{Interpreting counts and time series}

Titanic's rows are contingency cells, not individual passengers. Sum the
@racket["frequency"] column to count people, and retain zero-frequency rows
when you need the complete table of combinations:

@examples[#:eval ev
 (define counts (load-titanic #:format 'table))
 (dataset-table-row-count counts)
 (for/sum ([count (in-vector (dataset-table-column counts "frequency"))])
   count)]

AirPassengers contains monthly international airline passenger counts, measured
in thousands. Its year and month columns make the chronology explicit:

@examples[#:eval ev
 (define monthly (load-air-passengers #:format 'matrix))
 (for/list ([column (in-range 3)]) (matrix-ref monthly 0 column))
 (for/list ([column (in-range 3)]) (matrix-ref monthly 143 column))
 (hash-ref (dataset-info 'air-passengers) "time-series")]

The same metadata interface records source citations, licenses, units, identifiers,
and file checksums for every dataset. The catalog is generated from that registry
rather than maintained as a second independent list.

@section[#:tag "datasets-downstream-guide"]{Using core data in another package}

A package that constructs its own dataframes can depend on @tt{datasets-core}
and use @racketmodname[datasets/core] directly. This is particularly useful for
Polars documentation: depending only on the core avoids a dependency cycle
through @tt{datasets}.

The following example uses core data with Polars' constructors:

@examples[#:eval ev
 (require datasets/core)
 (define table (load-dataset-table 'iris #:columns '("sepal-length")))
 (define own-frame
   (pl:dataframe-new
    (list (pl:series-new-f64
           "sepal-length"
           (vector->list (dataset-table-column table "sepal-length"))))))
 (pl:dataframe-height own-frame)]

The repository's @tt{examples/core-with-polars.rkt} demonstrates the same pattern
and is tested before the adapter package is installed. For downstream Nix,
fetch this repository with @tt{flake = false} and install @tt{datasets-core/}
from that source. This keeps the complete flake and its native dependencies out
of the downstream dependency graph.

@(close-eval ev)
