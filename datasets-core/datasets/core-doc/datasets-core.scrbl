#lang scribble/manual
@(require (for-label racket/base datasets/core))

@title[#:tag '("top" "datasets-core") #:tag-prefix "(lib datasets/core-doc/datasets-core.scrbl)"]{Datasets Core}
@author{datasets contributors}

@bold{Work in progress.} This manual covers the basic loading and table APIs.
The explanations and worked examples are still being expanded.

@racketmodname[datasets/core] provides bundled datasets as immutable Racket
tables. Use it when you want ordinary Racket values, want to choose your own
analysis library, or need data for another package's examples.

The @secref["datasets-core-guide"] introduces tables, missing observations,
and metadata through examples. The @secref["datasets-core-reference"] specifies
each exported binding. The @secref["datasets-catalog"] describes every dataset,
its variables, and its provenance.

This manual and package work without Polars, @tt{math-lib}, or @tt{data-frame}.
For general Racket programming, see
@other-doc['(lib "scribblings/guide/guide.scrbl")].

@table-of-contents[]
@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
@include-section["catalog.scrbl"]
@index-section[]
