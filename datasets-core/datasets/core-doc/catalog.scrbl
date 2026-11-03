#lang scribble/manual
@(require "package-catalog.scrbl" (for-label racket/base datasets/core))
@title[#:tag "datasets-catalog"]{Dataset Catalog and Provenance}

The following catalog is generated from the same registry used by
@racket[dataset-info]. Column order matches the full table returned by
@racket[load-dataset-table]; a run of three or more numbered columns of one type
and description, such as @tt{gene-1} to @tt{gene-2308}, is one row. It lists
the datasets of @tt{datasets-core}. The datasets of @tt{datasets-r},
@tt{datasets-uci}, @tt{datasets-islr} and @tt{datasets-fetch} are catalogued in
the same form in each package's own manual.

Redistribution follows upstream distribution terms. Each package section lists
its data licenses and each entry names its own: in @tt{datasets-core}, R data
under GPL-2 or GPL-3, @tt{lars} diabetes under GPL-2, and UCI data under CC BY
4.0. This records the redistribution basis, without asserting independent
original-owner permission. Code is Apache-2.0 OR MIT. Full license texts and
upstream variable descriptions and citations are bundled in
@tt{datasets/licenses} and @tt{datasets/provenance} for @tt{datasets-core}, and
in @tt{licenses} and @tt{provenance} beside the registry of any other package.
@tt{datasets-fetch} ships no data, so its entries name upstream terms of use.
The maintainer importer pins source archives by SHA-256. Normalized checksums cover
the exact UTF-8 bytes of each dataset's uncompressed @tt{.rktd} content. Historical observations are not
silently corrected except for choosing the documented corrected UCI Iris variant.

@(package-catalog "datasets-core")
