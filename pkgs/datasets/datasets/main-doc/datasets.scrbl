#lang scribble/manual
@(require scribble/eval datasets/core
          (for-label racket/base datasets datasets/polars datasets/matrix datasets/data-frame))
@(define ev (make-base-eval))
@(ev '(require datasets (prefix-in pl: polars) math/matrix (prefix-in df: data-frame)))
@title{Datasets: Dataframes and Matrices}
@author{datasets contributors}
See @other-doc['(lib "scribblings/reference/reference.scrbl")] for the Racket language.
@defmodule[datasets]
Install Racket 9.3 or later, then @tt{raco pkg install ./pkgs/datasets-core}
and @tt{raco pkg install --auto ./pkgs/datasets}. The latter depends on the
catalog package @tt{polars}; its native library must be available.
The repository's Nix build reuses Polars' native derivation.

@defproc[(load-dataset [name symbol?] [#:format format symbol? 'polars]
                       [#:columns columns (or/c #f (listof string?)) #f]) any/c]{
Formats are @racket['polars] (default), @racket['table], @racket['matrix], and
@racket['data-frame]. Selection is applied before conversion and preserves order.
Each dataframe is independently allocated. Matrix conversion rejects nonnumeric
or missing values and identifies the column and zero-based row. It does not encode
labels, drop columns, or impute missing observations. All core exports are re-exported.}

@section{Named loaders}
Every named loader accepts the same @tt{#:format} and @tt{#:columns} keywords
and delegates to @racket[load-dataset].
@defproc[(load-mtcars [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads mtcars.}
@defproc[(load-iris [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads corrected UCI Iris.}
@defproc[(load-faithful [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads faithful.}
@defproc[(load-anscombe [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads the wide Anscombe quartet.}
@defproc[(load-airquality [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads airquality, preserving missing values.}
@defproc[(load-us-arrests [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads US arrests with state identifiers.}
@defproc[(load-plant-growth [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads plant growth.}
@defproc[(load-tooth-growth [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads tooth growth.}
@defproc[(load-titanic [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads the contingency cells, including zero counts.}
@defproc[(load-air-passengers [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads monthly counts in chronological order.}
@defproc[(load-diabetes [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads the standardized lars representation.}
@defproc[(load-breast-cancer [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads UCI diagnostic measurements, M/B labels, and identifiers.}
@defproc[(load-wine [#:format format symbol? 'polars] [#:columns columns (or/c #f (listof string?)) #f]) any/c]{Loads wine measurements and cultivar labels.}

@section{Examples}
Categories remain strings in Polars; ordered levels remain in metadata. Missing
observations map to @tt{polars-null}. The data-frame adapter configures its series
NA value explicitly as @racket[dataset-missing].
@examples[#:eval ev
 (define iris (load-iris))
 (pl:dataframe-height iris)
 (pl:~> iris (pl:group-by "species") (pl:agg (pl:mean (pl:col "sepal-length"))))
 (matrix-num-cols (load-iris #:format 'matrix
                            #:columns '("sepal-length" "sepal-width" "petal-length" "petal-width")))
 (define air (load-airquality))
 (pl:polars-null? (pl:series-ref (pl:dataframe-column air "ozone") 4))
 (define counts (load-titanic #:format 'table))
 (for/sum ([n (in-vector (dataset-table-column counts "frequency"))]) n)
 (hash-ref (dataset-info 'air-passengers) "time-series")]

@section{Adapters}
@subsection{Polars conversion}
@defmodule[datasets/polars]
@defproc[(table->polars [table dataset-table?]) any/c]{Creates independent Polars columns. Integer data uses Int64, real data Float64, and labels and identifiers strings.}
@subsection{Matrix conversion}
@defmodule[datasets/matrix]
@defproc[(table->matrix [table dataset-table?]) any/c]{Creates a math/matrix matrix with rows as observations. Requires numeric, nonmissing values.}
@subsection{Data-frame conversion}
@defmodule[datasets/data-frame]
@defproc[(table->data-frame [table dataset-table?]) any/c]{Creates a data-frame with copied column vectors and an explicit NA sentinel.}

@defproc[(data-frame-column-names [frame any/c]) (listof string?)]{Returns the
requested order retained in the @tt{datasets:columns} frame property.
The underlying data-frame library's @tt{df-series-names} has unspecified order.
This property records the initial selection; callers that add or remove columns
must update it if they use it afterwards.}

@section{Downstream integration}
Polars documentation can depend on @tt{datasets-core} only and construct frames
through its own constructors. The repository's @tt{examples/core-with-polars.rkt}
exercises this pattern without requiring @tt{datasets}. For Nix, fetch this
repository as a source input with @tt{flake = false}, and install only
@tt{pkgs/datasets-core}; do not import this repository's complete flake into
Polars' dependency graph. See the core manual for the registry-generated catalog,
variable descriptions, citations, license basis, and checksums.
@(close-eval ev)
