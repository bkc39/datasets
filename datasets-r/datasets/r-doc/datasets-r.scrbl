#lang scribble/manual
@(require (only-in (lib "datasets/core-doc/package-catalog.scrbl") package-catalog))

@title[#:tag '("top" "datasets-r") #:tag-prefix "(lib datasets/r-doc/datasets-r.scrbl)"]{R Datasets Catalog}
@author{datasets contributors}

@tt{datasets-r} bundles the objects of R's @tt{datasets} package and teaching datasets from CRAN.
Load them with @tt{datasets/core} or @tt{datasets}; see
@other-doc['(lib "datasets/core-doc/datasets-core.scrbl")] for the loading API,
the column types and the provenance conventions this catalog follows.

@table-of-contents[]
@(package-catalog "datasets-r")
