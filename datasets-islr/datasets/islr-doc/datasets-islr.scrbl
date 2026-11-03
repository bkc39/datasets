#lang scribble/manual
@(require (only-in (lib "datasets/core-doc/package-catalog.scrbl") package-catalog))

@title[#:tag '("top" "datasets-islr") #:tag-prefix "(lib datasets/islr-doc/datasets-islr.scrbl)"]{ISLR2 and ESL Datasets Catalog}
@author{datasets contributors}

@tt{datasets-islr} bundles the datasets of ISLR2 and of The Elements of Statistical Learning.
Load them with @tt{datasets/core} or @tt{datasets}; see
@other-doc['(lib "datasets/core-doc/datasets-core.scrbl")] for the loading API,
the column types and the provenance conventions this catalog follows.

@table-of-contents[]
@(package-catalog "datasets-islr")
