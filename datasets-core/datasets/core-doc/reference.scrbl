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

Procedures in this module that discover registries or take a dataset name
(@racket[dataset-names], @racket[dataset-packages], @racket[dataset-info],
@racket[load-dataset-table], @racket[fetch-dataset], @racket[dataset-cached?]
and @racket[clear-dataset-cache]) are not thread-safe: concurrent calls can
silently omit the datasets of other packages. Callers using several threads
must serialize these calls.

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
and @racket[download?] is @racket[#f], raises @racket[exn:fail] naming the dataset
and how to fetch it (see @secref["datasets-core-offline-reference"]). Every load
checks the data's SHA-256 against the @tt{sha256} metadata before parsing, for
bundled and cached data alike; a mismatch or an unreadable file raises
@racket[exn:fail] naming the file. For a cached file that cannot be read or is
not the pinned download, the error gives @racket[clear-dataset-cache] as the
remedy. For a pinned, readable one, and for any data that matches its SHA-256
but does not parse, it names the dataset and package instead: the registry entry
is wrong.}

@examples[#:eval ev
 (dataset-table-names
  (load-dataset-table 'mtcars #:columns '("mpg" "model")))
 (eval:error (load-dataset-table 'iris #:columns '("unknown")))]

@subsection[#:tag "datasets-registries-reference"]{Registries}

A registry is a file mapping dataset names to metadata hashes. Any installed
package can declare registries with the @tt{datasets-registries} key in a
collection-level @tt{info.rkt}: a list of paths relative to that file's
directory. Each dataset's data is @tt{data/@italic{name}.rktd.gz} beside its
registry: gzip-compressed UTF-8 text of an opening parenthesis and newline, each
row on its own line as two spaces followed by its @racket[write] form, then a
closing parenthesis and newline. Loading expects this form; other text may be
rejected. The registries of all installed packages are merged when first used.
Each later call checks the registry files and declaring @tt{info.rkt} files of
the packages already found, and merges again when one changes. The installed
package index is read again when the collection search path changes, as
with @racket[current-library-collection-paths], when @exec{raco setup} runs in
the same process, or after a call whose discovery failed or was interrupted, so
a package installed after the first call is usually not seen until the process
restarts.
Package @tt{datasets-core} is reserved for the registries that
@tt{datasets/core-data/info.rkt} lists. Another installation's
@tt{datasets/core-data} collection is skipped unread; any other registry
declaring @tt{datasets-core} is an error naming its file. A
name declared twice is an error naming both packages. A malformed registry is an
error naming its file. Either error makes every
registry operation raise until the package at fault is fixed or removed.
A fetch-tier entry has no data file; see @secref["datasets-core-cache-reference"].

When reading the installed package index fails with a permission error, or a
security guard denies it as in a default @racketmodname[racket/sandbox]
evaluator, only the @tt{datasets-core} registries are used: a warning is logged
to the @tt{datasets} topic, an unknown dataset or package error gives the
reason, and the next call tries discovery again. Any other failure to read the
index raises.

@section[#:tag "datasets-core-cache-reference"]{Downloads and the cache}

Fetch-tier datasets are not stored in any package. Their data is fetched once
into the cache directory and read from there, and their @tt{license} gives the
upstream terms of use rather than of redistribution, or @tt{NOASSERTION} when the
source states none, with the basis in @tt{notes}. A dataset whose @tt{fetch} format
is @tt{rktd.gz} is downloaded ready to read: its @tt{fetch} hash has @tt{urls}
(mirrors, tried in order) and the @tt{sha256} of the download. A converted set is
built locally from upstream files (see @secref["datasets-core-convert-reference"]).
Other @tt{fetch} keys are ignored. A cached file is
named by a SHA-256 its registry entry pins, of the download for @tt{rktd.gz} and of
the converted content for a converted set, so a registry update that changes the
data fetches it again rather than reusing the old file.

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
file; @racket[load-dataset-table] checks its SHA-256. An unknown dataset raises
@racket[exn:fail:contract]. For a converted set only the converted file counts;
upstream files are never kept.}
@examples[#:eval ev (dataset-cached? 'iris)]

@defproc[(fetch-dataset [name symbol?]
                        [#:idle-timeout idle-timeout (and/c real? positive?) 60])
         complete-path?]{
Makes @racket[name]'s data available locally and returns the path of the
gzip-compressed @tt{.rktd} file @racket[load-dataset-table] reads: the file in its
package for a bundled dataset, the cached file for a fetch-tier one. A fetch-tier
dataset that is not cached is downloaded from the @tt{urls} in its @tt{fetch}
metadata, in order, until one succeeds. Each download goes to a temporary file
beside its destination and is renamed into place only after its SHA-256 matches
the metadata, so an interrupted or failed download never leaves a partial file
under the cached name. A URL whose body delivers no data for
@racket[idle-timeout] seconds fails. The idle timeout bounds waits for body data
only; connecting, the TLS handshake and waiting for response headers, including
redirects, have no timeout. If every URL fails, raises
@racket[exn:fail] listing each URL and its failure, with the expected and actual
SHA-256 for a mismatch. Raises @racket[exn:fail] without contacting a server when
the @tt{format} is not supported; an unknown dataset raises
@racket[exn:fail:contract].

For a converted set the converter is loaded first, then each of its @tt{files} is
downloaded as above to a temporary file, and the converter's rows become the
cached file. The temporary files are deleted whether or not this succeeds. A file
whose every URL fails, a converter that cannot be loaded or raises, a malformed
row, or content whose SHA-256 differs from the @tt{sha256} metadata raises
@racket[exn:fail] naming the dataset and the file or converter, and nothing is
cached. A contract violation from the converter propagates unchanged (see
@secref["datasets-core-convert-reference"]).}
@examples[#:eval ev (file-exists? (fetch-dataset 'iris))]

@defproc[(clear-dataset-cache [name (or/c #f symbol?) #f]) void?]{
Deletes downloaded files from @racket[(dataset-cache-directory)]: those of
@racket[name], or, when @racket[name] is @racket[#f], those of every dataset,
including datasets whose package is no longer installed. Only files the cache
created (verified downloads, converted files and leftover temporary files) are removed; other
files and the directory itself are left alone. A bundled dataset has nothing to
delete. An unknown @racket[name] raises @racket[exn:fail:contract]. Clearing a
dataset while another program downloads it makes that download fail.}

@subsection[#:tag "datasets-core-convert-reference"]{Converted sets}

A fetch-tier dataset whose @tt{fetch} format is @tt{convert} is built on first use
from upstream files. Its @tt{fetch} hash has @tt{files}, a nonempty list of hashes
each with a @tt{name}, @tt{urls} (mirrors, tried in order) and that file's
@tt{sha256}, and @tt{converter}, a hash with a @tt{module} (a collection-based
module path, as a string) and a @tt{binding} it provides. Its @tt{source-sha256} is
the SHA-256 of the files' @tt{sha256} values joined by newlines, in @tt{files}
order, and its @tt{source-url} may name the upstream page:

@racketblock[
(hash "format" "convert"
      "files" (list (hash "name" "covtype.data.gz"
                          "urls" (list "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz"
                                       "https://ndownloader.figshare.com/files/5976039")
                          "sha256" "614360d0257557dd1792834a85a1cdebfadc3c4f30b011d56afee7ffb5b15771"))
      "converter" (hash "module" "datasets/fetch-private/convert" "binding" "covtype-rows"))]

The converted rows are written in the form bundled data is stored in (see
@secref["datasets-registries-reference"]). The dataset's
@tt{sha256} pins the SHA-256 of that text, which is checked before the file is
cached and again on every load. A converter change that alters the data therefore
takes effect only with a registry update that pins the new content.

@defproc[#:kind "converter" #:link-target? #f
         (converter [files (hash/c string? complete-path? #:immutable #t)]
                    [info (and/c hash? immutable?)])
         sequence?]{
The protocol a converter binding follows. @racket[files] maps each file's
@tt{name} to its verified download and @racket[info] is the dataset's
@racket[dataset-info]. The result is a sequence, possibly lazy, of rows in source
order, each a list of @tt{column-count} exact integers, flonums, strings or
@racket['missing]. A contract violation raised while loading the converter or
producing rows, such as a binding that is not a procedure of two arguments or a
result that is not a sequence, propagates unchanged; any other @racket[exn:fail]
is reported as described for @racket[fetch-dataset].}

@subsection[#:tag "datasets-core-offline-reference"]{Disabling downloads}

A download happens only when a fetch-tier dataset is not cached.
@racket[#:download? #f] disables it for one call to @racket[load-dataset-table]; a
refused download raises @racket[exn:fail] naming the dataset, the expected cache
file and how to fetch it. Bundled and cached datasets load as usual. A converted
set is neither downloaded nor converted while downloads are disabled. Every load
checks the data's SHA-256, for both tiers; a corrupt cached file raises
@racket[exn:fail] naming @racket[clear-dataset-cache] as the remedy.

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
       (list @tt{source-url} "URL of the pinned upstream archive or page.")
       (list @tt{source-version} "Upstream version or hash-pinned snapshot description.")
       (list @tt{source-sha256} "Hexadecimal SHA-256 of the upstream archive.")
       (list @tt{sha256} "Hexadecimal SHA-256 of the normalized .rktd content's UTF-8 bytes, before compression.")
       (list @tt{license} "Accepted upstream terms.")
       (list @tt{citation} "Source attribution and bibliographic citation.")
       (list @tt{notes} "Transformations, representation choices, and historical quirks.")
       (list @tt{time-series} "Time-series description hash, or #f.")
       (list @tt{tier} @elem{@tt{bundled} (shipped with its package) or @tt{fetch} (downloaded on first use).})
       (list @tt{package} "Name of the package whose registry declares the dataset.")
       (list @tt{fetch} @elem{@racket[#f] for bundled data, a hash for fetch-tier data; see @secref["datasets-core-cache-reference"].}))]

A package registry may add further keys, which core passes through unchecked to
@racket[dataset-info]; @tt{datasets-islr}, for example, adds @tt{islr2-chapters}.

Each column-description hash has @tt{name}, @tt{original-name}, @tt{type},
@tt{levels}, @tt{unit}, @tt{description}, and @tt{missing-count} keys.
Semantic types are strings: @tt{integer}, @tt{real}, @tt{categorical},
@tt{identifier}, or @tt{string}. Category levels are ordered strings; a noncategory
has an empty level list. A unit of @racket[#f] means unspecified. The missing count
refers to that column in the complete source dataset.

A time-series hash has @tt{frequency} (observations per unit of the first index
column, such as 12 for monthly data indexed by year and month), @tt{index} (the
names of the index columns, outermost first), and @tt{start} and @tt{end} (the
index values of the first and last rows, one per index column).

@(close-eval ev)
