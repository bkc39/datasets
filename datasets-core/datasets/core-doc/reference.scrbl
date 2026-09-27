#lang scribble/manual
@(require scribble/eval
          (for-label racket/base racket/contract/base datasets/core))
@(define ev (make-base-eval))
@(ev '(require datasets/core))

@title[#:tag "datasets-core-reference"]{Reference}
@defmodule[datasets/core]

This section specifies the core API. For a step-by-step introduction, see the
@secref["datasets-core-guide"]. Contracts below describe accepted arguments and
results using the notation of
@other-doc['(lib "scribblings/reference/reference.scrbl")].

@section[#:tag "datasets-core-loading-reference"]{Registry and loading}

@defproc[(dataset-names) (listof symbol?)]{
Returns all available dataset identifiers, sorted alphabetically. The result
is stable for the bundled catalog.}

@defproc[(dataset-info [name symbol?]) (and/c hash? immutable?)]{
Returns deeply immutable metadata for @racket[name]. An unknown identifier
raises @racket[exn:fail:contract]. Metadata describes the complete source dataset;
it is independent of any column selection. See
@secref["datasets-metadata-reference"] for its fields.}

@defproc[(load-dataset-table [name symbol?]
                             [#:columns columns (or/c #f (listof string?)) #f])
         dataset-table?]{
Reads the bundled observations for @racket[name] and returns a table. Data files
are resolved relative to the installed module and read when this procedure is
called; the current directory does not affect loading.

If @racket[columns] is @racket[#f], all columns are returned in source order.
Otherwise, it must be a nonempty list of distinct known column names. Its order
determines the table's column order. Row order and row count are preserved.
Unknown datasets, unknown columns, empty selections, duplicate columns, and
invalid argument types raise @racket[exn:fail:contract].

The result contains immutable column vectors. Strings inside those vectors are
also immutable. Missing observations are @racket[dataset-missing]. Repeated
loads are equal; callers should not rely on object identity.}

@examples[#:eval ev
 (dataset-table-names
  (load-dataset-table 'mtcars #:columns '("mpg" "model")))
 (eval:error (load-dataset-table 'iris #:columns '("unknown")))]

@section[#:tag "datasets-core-table-reference"]{Table inspection}

@defproc[(dataset-table? [value any/c]) boolean?]{
Returns @racket[#t] if @racket[value] is a dataset table, @racket[#f] otherwise.
There is no public table constructor or mutator.}

@defproc[(dataset-table-names [table dataset-table?]) (listof string?)]{
Returns the selected column names in order. Each string is immutable.}

@defproc[(dataset-table-columns [table dataset-table?]) (and/c vector? immutable?)]{
Returns an immutable vector of immutable column vectors, in the same order as
@racket[dataset-table-names]. Each column has
@racket[(dataset-table-row-count table)] elements.}

@defproc[(dataset-table-column [table dataset-table?] [name string?])
         (and/c vector? immutable?)]{
Returns the column named @racket[name] from @racket[table]. A name that is not
among the selected columns raises @racket[exn:fail:contract].}

@defproc[(dataset-table-row-count [table dataset-table?]) exact-nonnegative-integer?]{
Returns the number of source observations, including rows with missing values.}

@defproc[(dataset-table-info [table dataset-table?]) (and/c hash? immutable?)]{
Returns the full source metadata. Its @racket["column-count"] may exceed the
number of selected columns in @racket[table].}

@section[#:tag "datasets-core-missing-reference"]{Missing values}

@deftogether[(@defthing[dataset-missing any/c]
              @defproc[(dataset-missing? [value any/c]) boolean?])]{
@racket[dataset-missing] is the missing-value singleton. The predicate recognizes
that value by identity. It returns @racket[#f] for @racket[#f], zero, NaN,
strings, and symbols, including @racket['missing]. The singleton is not a number
and must be handled explicitly before arithmetic.}

@section[#:tag "datasets-metadata-reference"]{Metadata fields}

All metadata hashes, lists, and strings are immutable. Dataset hashes have these
string keys:

@tabular[#:sep @hspace[2]
 (list (list @bold{Key} @bold{Meaning})
       (list @tt{name} "Dataset identifier as a string.")
       (list @tt{rows} "Number of source rows.")
       (list @tt{column-count} "Number of source columns, including identifiers.")
       (list @tt{columns} "Ordered list of column-description hashes.")
       (list @tt{identifiers} "Column names identifying observations; an empty list if none.")
       (list @tt{suggested-targets} "Optional response or label column names; an empty list if unspecified.")
       (list @tt{source-url} "URL of the pinned upstream archive.")
       (list @tt{source-version} "Upstream version or hash-pinned snapshot description.")
       (list @tt{source-sha256} "Hexadecimal SHA-256 of the upstream archive.")
       (list @tt{sha256} "Hexadecimal SHA-256 of the normalized .rktd file's UTF-8 bytes.")
       (list @tt{license} "Accepted upstream redistribution terms.")
       (list @tt{citation} "Source attribution and bibliographic citation.")
       (list @tt{notes} "Transformations, representation choices, and historical quirks.")
       (list @tt{time-series} "Time-series description hash, or #f."))]

Each column-description hash has @tt{name}, @tt{original-name}, @tt{type},
@tt{levels}, @tt{unit}, @tt{description}, and @tt{missing-count} keys.
Semantic types are strings: @tt{integer}, @tt{real}, @tt{categorical},
@tt{identifier}, or @tt{string}. Category levels are ordered strings; a noncategory
has an empty level list. A unit of @racket[#f] means unspecified. The missing count
refers to that column in the complete source dataset.

For AirPassengers, the time-series hash has @tt{frequency} (12 observations per
year), @tt{start} and @tt{end} (year/month lists), and @tt{index} (the year and
month column names). Other v0.1 datasets have @racket[#f] for @tt{time-series}.

@(close-eval ev)
