#lang scribble/manual
@(require (for-label racket/base datasets))

@title[#:tag '("top" "datasets-manual") #:tag-prefix "(lib datasets/main-doc/datasets.scrbl)"]{Datasets: Dataframes and Matrices}
@author{datasets contributors}

@bold{Work in progress.} This manual covers basic dataset loading and format
conversion. The explanations and worked examples are still being expanded.

@racketmodname[datasets] brings small, attributed datasets into Racket programs.
Start with @racket[load-iris] or choose a dataset with @racket[load-dataset]. The
default result is a Polars dataframe; tables, matrices, and @tt{data-frame}
objects are available through the same loading interface.

The @secref["datasets-guide"] is a practical introduction with evaluated
examples. The @secref["datasets-reference"] defines the loaders, selection rules,
and conversion procedures. For the lightweight table API and the catalog of
@tt{datasets-core}, see @other-doc['(lib "datasets/core-doc/datasets-core.scrbl")].
Each other package catalogs its own datasets:
@other-doc['(lib "datasets/r-doc/datasets-r.scrbl")],
@other-doc['(lib "datasets/uci-doc/datasets-uci.scrbl")],
@other-doc['(lib "datasets/islr-doc/datasets-islr.scrbl")] and
@other-doc['(lib "datasets/fetch-doc/datasets-fetch.scrbl")].

@table-of-contents[]
@include-section["guide.scrbl"]
@include-section["r-datasets.scrbl"]
@include-section["reference.scrbl"]
@index-section[]
