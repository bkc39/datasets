#lang scribble/manual
@(require (for-label racket/base racket/contract/base datasets datasets/polars
                     datasets/matrix datasets/data-frame))

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
                       [#:columns columns (or/c #f (listof string?)) #f]) any/c]{
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
