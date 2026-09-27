#lang scribble/manual
@(require scribble/eval datasets/core (for-label racket/base datasets/core))
@(define ev (make-base-eval))
@(ev '(require datasets/core))
@title{Datasets Core}
@author{datasets contributors}
See @other-doc['(lib "scribblings/reference/reference.scrbl")] for the Racket language.
@defmodule[datasets/core]

Bundled toy data with immutable columns and provenance. Install only
@tt{raco pkg install ./pkgs/datasets-core} for a lightweight environment;
Racket 9.3 or later is required. No Polars, math-lib, data-frame, R, or network
access is needed to load the committed data or build this manual.

@section{Tables and missing values}
@defproc[(dataset-names) (listof symbol?)]{Returns dataset identifiers in stable alphabetical order.}
@defproc[(dataset-info [name symbol?]) hash?]{Returns deeply immutable metadata.
String keys include @tt{rows}, @tt{column-count}, @tt{columns}, @tt{identifiers},
@tt{suggested-targets}, @tt{source-url}, @tt{source-version}, @tt{source-sha256},
@tt{sha256}, @tt{license}, @tt{citation}, @tt{notes}, and @tt{time-series}.
Column descriptions contain @tt{name}, @tt{original-name}, @tt{type},
@tt{levels} (in source order), @tt{unit}, @tt{description}, and @tt{missing-count}.
A false unit means unspecified. Metadata always describes the full source dataset,
including when a table selects only some columns.}
@defproc[(load-dataset-table [name symbol?] [#:columns columns (or/c #f (listof string?)) #f]) dataset-table?]{
Reads bundled data lazily. Selects all columns by default; otherwise preserves the
requested order. Empty selections, duplicates, unknown names, and unknown datasets
raise errors. Values and column vectors are immutable.}
@defproc[(dataset-table? [value any/c]) boolean?]{Recognizes tables.}
@defproc[(dataset-table-names [table dataset-table?]) (listof string?)]{Ordered selected column names.}
@defproc[(dataset-table-columns [table dataset-table?]) vector?]{Immutable vector of immutable column vectors.}
@defproc[(dataset-table-row-count [table dataset-table?]) exact-nonnegative-integer?]{Number of source rows.}
@defproc[(dataset-table-info [table dataset-table?]) hash?]{Full source metadata.}
@defproc[(dataset-table-column [table dataset-table?] [name string?]) vector?]{Looks up a selected column; unknown names raise an error.}
@defthing[dataset-missing any/c]{Dedicated missing-value singleton, distinct from false, zero, and NaN.}
@defproc[(dataset-missing? [value any/c]) boolean?]{Recognizes the singleton.}
@examples[#:eval ev
 (dataset-names)
 (define air (load-dataset-table 'airquality))
 (for/sum ([v (in-vector (dataset-table-column air "ozone"))])
   (if (dataset-missing? v) 1 0))
 (define titanic (load-dataset-table 'titanic))
 (for/sum ([v (in-vector (dataset-table-column titanic "frequency"))]) v)
 (hash-ref (dataset-info 'air-passengers) "time-series")]

@section{Catalog, variables, and provenance}
Redistribution follows upstream distribution terms: R data under GPL-2 or GPL-3,
@tt{lars} diabetes under GPL-2, and UCI data under CC BY 4.0. This records the
redistribution basis, without asserting independent original-owner permission.
Code is Apache-2.0 OR MIT. Full license texts and upstream variable descriptions
and citations are bundled in @tt{datasets/licenses} and @tt{datasets/provenance}.
The maintainer importer pins source archives by SHA-256. Normalized checksums cover
the exact UTF-8 bytes of each @tt{.rktd} file. Historical observations are not
silently corrected except for choosing the documented corrected UCI Iris variant.

@(for/list ([name (in-list (dataset-names))])
   (define info (dataset-info name))
   (list (subsection (symbol->string name))
         (para (format "~a rows × ~a columns. ~a" (hash-ref info "rows")
                       (hash-ref info "column-count") (hash-ref info "notes")))
         (para (hash-ref info "citation"))
         (para (hyperlink (hash-ref info "source-url") (hash-ref info "source-version"))
               "; " (hash-ref info "license"))
         (tabular #:sep (hspace 2)
                  (cons (list "Column" "Original" "Type" "Description / unit / levels")
                        (for/list ([c (in-list (hash-ref info "columns"))])
                          (list (hash-ref c "name") (hash-ref c "original-name")
                                (hash-ref c "type")
                                (format "~a; ~a; ~a" (hash-ref c "description")
                                        (hash-ref c "unit") (hash-ref c "levels"))))))))
@(close-eval ev)
