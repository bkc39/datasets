#lang scribble/manual
@(require (only-in (lib "datasets/core-doc/package-catalog.scrbl") package-catalog))

@title[#:tag '("top" "datasets-fetch") #:tag-prefix "(lib datasets/fetch-doc/datasets-fetch.scrbl)"]{Fetched Datasets Catalog}
@author{datasets contributors}

@tt{datasets-fetch} catalogues datasets that are downloaded and converted locally on first use; it ships no data, so its entries name upstream terms of use.
Load them with @tt{datasets/core} or @tt{datasets}; see
@other-doc['(lib "datasets/core-doc/datasets-core.scrbl")] for the loading API,
the column types and the provenance conventions this catalog follows.

@table-of-contents[]
@(package-catalog "datasets-fetch")
