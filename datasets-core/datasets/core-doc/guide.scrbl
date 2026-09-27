#lang scribble/manual
@(require scribble/eval
          (for-label racket/base datasets/core))
@(define ev (make-base-eval))

@title[#:tag "datasets-core-guide"]{Guide}

A dataset is a named collection of observations. In this package, each
observation occupies a row, and each variable occupies a named column. A
@deftech{dataset table} holds those columns as immutable vectors, together with
metadata describing their source and meaning.

@section[#:tag "datasets-core-install"]{Installing and loading data}

Use Racket 9.3 or later. From a checkout of the repository, install the core
package with:

@verbatim{raco pkg install ./datasets-core}

The repository contains two independently installable packages at its root:
@tt{datasets-core/} and @tt{datasets/}. Each has an @tt{info.rkt} declaring a
multi-collection package. They contribute different modules to the same
@tt{datasets} collection; package names and module paths need not match.
The core package provides @racketmodname[datasets/core].

@examples[#:eval ev
 (require datasets/core)
 (dataset-names)
 (define iris (load-dataset-table 'iris))
 (dataset-table-row-count iris)
 (dataset-table-names iris)]

Dataset identifiers are symbols, such as @racket['iris]; column names are
strings, such as @racket["sepal-length"]. Loading uses bundled files and does
not contact a server. There is no need to install R or run an import script.

@section[#:tag "datasets-core-columns"]{Working with columns}

Use @racket[dataset-table-column] to obtain a column, then ordinary vector
operations to inspect its values. A vector index is zero-based.

@examples[#:eval ev
 (define lengths (dataset-table-column iris "sepal-length"))
 (vector-ref lengths 0)
 (for/sum ([length (in-vector lengths)]) length)]

A selection keeps the source rows and places columns in the order you request:

@examples[#:eval ev
 (define selected
   (load-dataset-table 'iris #:columns '("species" "sepal-length")))
 (dataset-table-names selected)
 (dataset-table-row-count selected)
 (immutable? (dataset-table-column selected "species"))]

The column vectors and their strings are immutable. Keep derived results in
new values rather than changing the bundled observations. Empty selections,
duplicate names, and unknown names are errors; see
@secref["datasets-core-loading-reference"] for the selection rules.

@section[#:tag "datasets-core-missing-guide"]{Recognizing missing observations}

The airquality dataset contains missing ozone and solar-radiation measurements.
A missing observation is represented by @racket[dataset-missing], a dedicated
value recognized by @racket[dataset-missing?]. It differs from @racket[#f],
zero, and @racket[+nan.0].

@examples[#:eval ev
 (define air (load-dataset-table 'airquality))
 (define ozone (dataset-table-column air "ozone"))
 (dataset-missing? (vector-ref ozone 4))
 (for/sum ([value (in-vector ozone)])
   (if (dataset-missing? value) 1 0))]

Make any filtering decision explicit. For example, the following collects only
observed ozone values; it does not modify the original table:

@examples[#:eval ev
 (define observed
   (for/list ([value (in-vector ozone)]
              #:unless (dataset-missing? value))
     value))
 (length observed)]

@section[#:tag "datasets-core-metadata-guide"]{Reading metadata and category levels}

@racket[dataset-info] returns an immutable hash whose keys are strings. Its
@racket["columns"] entry is a list in source column order. Each entry describes
one variable, including its normalized name, original spelling, semantic type,
units, missing-value count, and category levels.

@examples[#:eval ev
 (define info (dataset-info 'iris))
 (hash-ref info "rows")
 (define species-info (list-ref (hash-ref info "columns") 4))
 (hash-ref species-info "levels")
 (hash-ref species-info "original-name")
 (hash-ref info "license")]

Labels remain strings in the table. The level list records their intended order;
it does not assert that a categorical variable is ordinal. Metadata on a selected
table still describes the complete source dataset. See
@secref["datasets-metadata-reference"] for the field definitions.

@section[#:tag "datasets-core-counts-guide"]{Counting people and indexing months}

A row does not always represent one person. Titanic contains 32 contingency
cells, each combining a class, sex, age group, and survival outcome. Its
@racket["frequency"] column counts people, including zero for empty cells:

@examples[#:eval ev
 (define titanic (load-dataset-table 'titanic))
 (dataset-table-row-count titanic)
 (for/sum ([count (in-vector (dataset-table-column titanic "frequency"))])
   count)]

AirPassengers instead has one row per calendar month. Year and month are
explicit columns, and passenger counts are in thousands:

@examples[#:eval ev
 (define passengers (load-dataset-table 'air-passengers))
 (for/list ([name (in-list '("year" "month" "passengers"))])
   (vector-ref (dataset-table-column passengers name) 0))
 (hash-ref (dataset-info 'air-passengers) "time-series")]

The @secref["datasets-catalog"] records these conventions alongside source
citations and transformation notes. Consult it before interpreting a variable
or comparing a dataset with another library's version.

@(close-eval ev)
