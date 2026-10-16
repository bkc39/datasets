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

Use Racket 9.3 or later. Install the core package from the Racket catalog:

@commandline{raco pkg install --auto datasets-core}

From a checkout of the repository, use
@tt{raco pkg install ./datasets-core} instead.

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
strings, such as @racket["sepal-length"]. Bundled datasets load from installed
files and never contact a server; see @secref["datasets-core-fetch-guide"] for
datasets that are downloaded. There is no need to install R or run an import
script.

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

@section[#:tag "datasets-core-other-packages"]{Datasets from other packages}

Other packages add datasets to the same catalog. Such a package contributes a
directory to the @tt{datasets} collection, conventionally
@tt{datasets/@italic{suffix}-data} for a package named
@tt{datasets-@italic{suffix}}, containing an @tt{info.rkt}, a registry file, and
a @tt{data} directory with one gzip-compressed @tt{.rktd} file per dataset:

@codeblock{#lang info
(define datasets-registries '("registry.rktd"))}

Once @exec{raco setup} has indexed the package, @racket[dataset-names],
@racket[dataset-info] and @racket[load-dataset-table] include its datasets, and
@racket[dataset-packages] lists it. Each entry carries the fields in
@secref["datasets-metadata-reference"], with @tt{package} naming the package and
@tt{tier} set to @tt{bundled}. Names are shared across all installed packages, so
two packages declaring the same name is an error naming both. The maintainer
importer writes this layout with
@tt{scripts/import-data.py --package @italic{name} --target @italic{dir}}.

@section[#:tag "datasets-core-fetch-guide"]{Downloaded datasets}

Some datasets are too large to ship in a package. Their metadata is installed
like any other, so @racket[dataset-names], @racket[dataset-info] and the catalog
describe them without a network connection, but their data is downloaded the
first time it is loaded. Such a dataset has @tt{tier} @tt{fetch}; its @tt{fetch}
metadata pins the SHA-256 of the file to download.

The first @racket[load-dataset-table] of a fetch-tier dataset downloads the file,
checks its SHA-256, and stores it in the cache; later loads read the cache and
contact no server. A failed or mismatched download leaves nothing behind.

The cache is @racket[(dataset-cache-directory)]: @filepath{racket-datasets} in
Racket's cache directory, or the directory named by @envvar{DATASETS_HOME}.

@examples[#:eval ev
 (parameterize ([current-environment-variables
                 (make-environment-variables #"DATASETS_HOME" #"/srv/racket-datasets")])
   (dataset-cache-directory))]

@racket[fetch-dataset] downloads without loading, and @racket[dataset-cached?]
says whether a load would need the network. To use datasets on a machine without
network access, fetch them elsewhere, copy the cache directory, and point
@envvar{DATASETS_HOME} at the copy. To make sure a load never downloads, pass
@racket[#:download? #f]; a dataset that is not cached is then an error that says
how to fetch it.
@racket[clear-dataset-cache] deletes downloaded files, for one dataset or all,
and nothing else.

@subsection[#:tag "datasets-core-fetch-licences"]{Licences of downloaded data}

A package that declares a fetch-tier dataset does not contain its data. The
@tt{license} and @tt{citation} metadata record the terms under which the source
distributes the data and how to cite it, and the catalog lists them for each
dataset. Those terms, not the package licence, govern the cached files and
anything derived from them; check them before sharing a copy of the cache.

@(close-eval ev)
