#lang scribble/manual
@(require scribble/eval
          (for-label racket/base racket/contract/base datasets/core))
@(define ev (make-base-eval))
@(ev '(require datasets/core))

@title[#:tag "datasets-core-reference"]{Reference}
@defmodule[datasets/core]

This section specifies the core API. For a step-by-step introduction, see the
@secref["datasets-core-guide"]. Contracts below describe accepted arguments and
results using the notation of
@other-doc['(lib "scribblings/reference/reference.scrbl")].

@section[#:tag "datasets-core-loading-reference"]{Registry and loading}

@defproc[(dataset-names [#:package package (or/c #f string?) #f]) (listof symbol?)]{
Returns the identifiers of every dataset declared by an installed registry,
sorted alphabetically, including fetch-tier datasets. If @racket[package] is a
string, returns only that package's datasets; a package that declares no
installed dataset raises @racket[exn:fail:contract] listing the available
packages. Conflicting or malformed registries raise @racket[exn:fail]; see
@secref["datasets-registries-reference"].}
@examples[#:eval ev (dataset-names #:package "datasets-core")]

@defproc[(dataset-packages) (listof string?)]{
Returns the names of the packages that declare at least one installed dataset,
sorted by @racket[string<?]. Every identifier in @racket[(dataset-names)] belongs
to exactly one of them, recorded in its metadata as @tt{package}.}
@examples[#:eval ev (dataset-packages) (hash-ref (dataset-info 'iris) "package")]

@defproc[(dataset-info [name symbol?]) (and/c hash? immutable?)]{
Returns deeply immutable metadata for @racket[name]. An unknown identifier
raises @racket[exn:fail:contract]. Metadata describes the complete source dataset;
it is independent of any column selection. See
@secref["datasets-metadata-reference"] for its fields.}

@defproc[(load-dataset-table [name symbol?]
                             [#:columns columns (or/c #f (non-empty-listof string?)) #f]
                             [#:download? download? boolean? #t])
         dataset-table?]{
Reads the observations for @racket[name] and returns a table. Bundled data files
are resolved relative to the installed module and read when this procedure is
called; the current directory does not affect loading.

If @racket[columns] is @racket[#f], all columns are returned in source order.
Otherwise, it must be a nonempty list of distinct known column names. Its order
determines the table's column order. Row order and row count are preserved.
Unknown datasets, unknown columns, empty selections, duplicate columns, and
invalid argument types raise @racket[exn:fail:contract].

The result contains immutable column vectors. Strings inside those vectors are
also immutable. Missing observations are @racket[dataset-missing]. Repeated
loads are equal; callers should not rely on object identity.

Bundled data is stored gzip-compressed and decompressed on each call; a
fetch-tier dataset is read from @racket[(dataset-cache-directory)] in the same
form, after downloading it as by @racket[fetch-dataset] if it is not cached.
Column arguments are checked before any download. If the dataset is not cached
and @racket[download?] is @racket[#f] or downloads are disabled (see
@secref["datasets-core-offline-reference"]), raises @racket[exn:fail] naming the
dataset and how to fetch it. Every load checks the data's SHA-256 against the
@tt{sha256} metadata before parsing; a mismatch or an unreadable file raises
@racket[exn:fail] naming the file.}

@examples[#:eval ev
 (dataset-table-names
  (load-dataset-table 'mtcars #:columns '("mpg" "model")))
 (eval:error (load-dataset-table 'iris #:columns '("unknown")))]

@subsection[#:tag "datasets-registries-reference"]{Registries}

A registry is a file mapping dataset names to metadata hashes. Any installed
package can declare registries with the @tt{datasets-registries} key in a
collection-level @tt{info.rkt}: a list of paths relative to that file's
directory. Each dataset's data is @tt{data/@italic{name}.rktd.gz} beside its
registry. The registries of all installed packages are merged when first used,
and merged again when a registry file or a declaring @tt{info.rkt} changes.
Package @tt{datasets-core} is reserved for the registries that
@tt{datasets/core-data/info.rkt} lists. Copies of those files discovered under
another installation's @tt{datasets/core-data} are skipped unread; any other
registry declaring @tt{datasets-core} is an error naming its file. A
name declared twice is an error naming both packages. A malformed registry is an
error naming its file. Either error makes every
registry operation raise until the package at fault is fixed or removed.
A fetch-tier entry has no data file; see @secref["datasets-core-cache-reference"].

When reading the installed package index is denied, as in a default
@racketmodname[racket/sandbox] evaluator, only the @tt{datasets-core} registries
are used: a warning is logged to the @tt{datasets} topic, an unknown dataset's
error gives the reason, and the next call tries discovery again.

@section[#:tag "datasets-core-cache-reference"]{Downloads and the cache}

Fetch-tier datasets are not stored in any package. Their data is downloaded once
into the cache directory and read from there. A cached file is named by the
SHA-256 its registry entry pins, so a registry update that changes the data
downloads the new file rather than reusing the old one.

@defproc[(dataset-cache-directory) complete-path?]{
Returns the directory holding downloaded datasets: the value of
@envvar{DATASETS_HOME} when it is set and nonempty, otherwise
@racket[(build-path (find-system-path 'cache-dir) "racket-datasets")]. The
variable is read on every call. A relative value makes this procedure,
@racket[fetch-dataset], @racket[dataset-cached?], @racket[clear-dataset-cache],
and loading a fetch-tier dataset raise @racket[exn:fail]; loading a bundled
dataset is unaffected. The directory is created by the first download, not by
this procedure.}
@examples[#:eval ev
 (parameterize ([current-environment-variables
                 (make-environment-variables #"DATASETS_HOME" #"/srv/racket-datasets")])
   (dataset-cache-directory))]

@defproc[(dataset-cached? [name symbol?]) boolean?]{
Returns @racket[#t] if loading @racket[name] needs no download: always for a
bundled dataset, and for a fetch-tier dataset when its file is in
@racket[(dataset-cache-directory)]. Never contacts a server and does not read the
file; @racket[load-dataset-table] checks its contents on every load. An unknown
dataset raises @racket[exn:fail:contract].}
@examples[#:eval ev (dataset-cached? 'iris)]

@defproc[(fetch-dataset [name symbol?]
                        [#:idle-timeout idle-timeout (and/c real? positive?) 60])
         complete-path?]{
Makes @racket[name]'s data available locally and returns the path of the
gzip-compressed @tt{.rktd} file @racket[load-dataset-table] reads: the file in its
package for a bundled dataset, the cached file for a fetch-tier one. A fetch-tier
dataset that is not cached is downloaded from the @tt{urls} in its @tt{fetch}
metadata, in order, until one succeeds. Each download goes to a temporary file
beside its destination and is renamed into place only after its size and SHA-256
match the metadata, so an interrupted, failed or concurrent download never leaves
a partial file under the cached name. A URL that delivers no data for
@racket[idle-timeout] seconds fails. If every URL fails, raises @racket[exn:fail]
listing each URL and its failure, with the expected and actual SHA-256 for a
mismatch. Raises @racket[exn:fail] without contacting a server when downloads are
disabled (@secref["datasets-core-offline-reference"]) or the @tt{format} is not
supported; an unknown dataset raises @racket[exn:fail:contract].}
@examples[#:eval ev (file-exists? (fetch-dataset 'iris))]

@defproc[(clear-dataset-cache [name (or/c #f symbol?) #f]) void?]{
Deletes downloaded files from @racket[(dataset-cache-directory)]: those of
@racket[name], or, when @racket[name] is @racket[#f], those of every dataset,
including datasets whose package is no longer installed. Only files the cache
created (verified downloads and leftover temporary files) are removed; other
files and the directory itself are left alone. A bundled dataset has nothing to
delete. An unknown @racket[name] raises @racket[exn:fail:contract]. Clearing a
dataset while another program downloads it makes that download fail.}

@subsection[#:tag "datasets-core-offline-reference"]{Disabling downloads}

A download happens only when a fetch-tier dataset is not cached and downloads are
enabled. @racket[#:download? #f] disables them for one call to
@racket[load-dataset-table]. Setting @envvar{DATASETS_OFFLINE} to any nonempty
value other than @tt{0}, @tt{false}, @tt{no} or @tt{off} (in any letter case)
disables them for the process, including in @racket[fetch-dataset], and takes
precedence over @racket[#:download? #t]. A refused download raises
@racket[exn:fail] naming the dataset, the reason, the expected cache file and how
to fetch it. Bundled and cached datasets load as usual.

Downloads are logged at level @racket['info] with topic @racket['datasets]; set
@envvar{PLTSTDERR} to @tt{info@"@"datasets} to see them.

@section[#:tag "datasets-core-table-reference"]{Table inspection}

@defproc[(dataset-table? [value any/c]) boolean?]{
Returns @racket[#t] if @racket[value] is a dataset table, @racket[#f] otherwise.
There is no public table constructor or mutator.}

@defproc[(dataset-table-names [table dataset-table?]) (listof string?)]{
Returns the selected column names in order. Each string is immutable.}

@defproc[(dataset-table-columns [table dataset-table?]) (and/c vector? immutable?)]{
Returns an immutable vector of immutable column vectors, in the same order as
@racket[dataset-table-names]. Each column has
@racket[(dataset-table-row-count table)] elements.}

@defproc[(dataset-table-column [table dataset-table?] [name string?])
         (and/c vector? immutable?)]{
Returns the column named @racket[name] from @racket[table]. A name that is not
among the selected columns raises @racket[exn:fail:contract].}

@defproc[(dataset-table-row-count [table dataset-table?]) exact-nonnegative-integer?]{
Returns the number of source observations, including rows with missing values.}

@defproc[(dataset-table-info [table dataset-table?]) (and/c hash? immutable?)]{
Returns the full source metadata. Its @racket["column-count"] may exceed the
number of selected columns in @racket[table].}

@section[#:tag "datasets-core-missing-reference"]{Missing values}

@deftogether[(@defthing[dataset-missing any/c]
              @defproc[(dataset-missing? [value any/c]) boolean?])]{
@racket[dataset-missing] is the missing-value singleton. The predicate recognizes
that value by identity. It returns @racket[#f] for @racket[#f], zero, NaN,
strings, and symbols, including @racket['missing]. The singleton is not a number
and must be handled explicitly before arithmetic.}

@section[#:tag "datasets-metadata-reference"]{Metadata fields}

All metadata hashes, lists, and strings are immutable. Dataset hashes have these
string keys:

@tabular[#:sep @hspace[2]
 (list (list @bold{Key} @bold{Meaning})
       (list @tt{name} "Dataset identifier as a string.")
       (list @tt{rows} "Number of source rows.")
       (list @tt{column-count} "Number of source columns, including identifiers.")
       (list @tt{columns} "Ordered list of column-description hashes.")
       (list @tt{identifiers} "Column names identifying observations; an empty list if none.")
       (list @tt{suggested-targets} "Optional response or label column names; an empty list if unspecified.")
       (list @tt{source-url} "URL of the pinned upstream archive.")
       (list @tt{source-version} "Upstream version or hash-pinned snapshot description.")
       (list @tt{source-sha256} "Hexadecimal SHA-256 of the upstream archive.")
       (list @tt{sha256} "Hexadecimal SHA-256 of the normalized .rktd content's UTF-8 bytes, before compression.")
       (list @tt{license} "Accepted upstream redistribution terms.")
       (list @tt{citation} "Source attribution and bibliographic citation.")
       (list @tt{notes} "Transformations, representation choices, and historical quirks.")
       (list @tt{time-series} "Time-series description hash, or #f.")
       (list @tt{tier} @elem{@tt{bundled} (shipped with its package) or @tt{fetch} (downloaded on first use).})
       (list @tt{package} "Name of the package whose registry declares the dataset.")
       (list @tt{fetch} @elem{@racket[#f] for bundled data. For fetch-tier data, a
             hash with @tt{urls} (mirrors, tried in order), @tt{sha256} and @tt{size}
             of the download, and its @tt{format}. The only format is @tt{rktd.gz}: the
             download is the dataset's gzip-compressed @tt{.rktd} file, the form
             bundled data is stored in. Other keys are ignored.}))]

Each column-description hash has @tt{name}, @tt{original-name}, @tt{type},
@tt{levels}, @tt{unit}, @tt{description}, and @tt{missing-count} keys.
Semantic types are strings: @tt{integer}, @tt{real}, @tt{categorical},
@tt{identifier}, or @tt{string}. Category levels are ordered strings; a noncategory
has an empty level list. A unit of @racket[#f] means unspecified. The missing count
refers to that column in the complete source dataset.

For AirPassengers, the time-series hash has @tt{frequency} (12 observations per
year), @tt{start} and @tt{end} (year/month lists), and @tt{index} (the year and
month column names). Other v0.1 datasets have @racket[#f] for @tt{time-series}.

@(close-eval ev)
