#lang scribble/manual
@(require (for-label racket/base datasets))

@title[#:tag "datasets-manual"]{Datasets: Dataframes and Matrices}
@author{datasets contributors}

@racketmodname[datasets] brings small, attributed datasets into Racket programs.
Start with @racket[load-iris] or choose a dataset with @racket[load-dataset]. The
default result is a Polars dataframe; tables, matrices, and @tt{data-frame}
objects are available through the same loading interface.

The @secref["datasets-guide"] is a practical introduction with evaluated
examples. The @secref["datasets-reference"] defines the loaders, selection rules,
and conversion procedures. For the complete dataset catalog and the lightweight
table API, see @other-doc['(lib "datasets/core-doc/datasets-core.scrbl")].

@table-of-contents[]
@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
@index-section[]
