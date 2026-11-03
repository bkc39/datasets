#lang scribble/manual
@(require (only-in (lib "datasets/core-doc/package-catalog.scrbl") package-catalog))

@title[#:tag '("top" "datasets-uci") #:tag-prefix "(lib datasets/uci-doc/datasets-uci.scrbl)"]{UCI Datasets Catalog}
@author{datasets contributors}

@tt{datasets-uci} bundles classic machine-learning datasets from the UCI Machine Learning Repository.
Load them with @tt{datasets/core} or @tt{datasets}; see
@other-doc['(lib "datasets/core-doc/datasets-core.scrbl")] for the loading API,
the column types and the provenance conventions this catalog follows.

@table-of-contents[]
@(package-catalog "datasets-uci")
