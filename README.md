# datasets

The documentation is a work in progress. Basic loading, table access, metadata,
and format conversion are documented; explanations and worked examples are
still being expanded.

Bundled datasets for Racket 9.3+: mtcars, iris, faithful, anscombe,
airquality, us-arrests, plant-growth, tooth-growth, titanic, air-passengers,
diabetes, breast-cancer, and wine in `datasets-core`, plus penguins,
penguins-raw, gapminder, datasaurus-dozen, titanic-passengers,
nycflights13-airlines, nycflights13-airports, nycflights13-planes,
nycflights13-weather, and the rest of R's `datasets` package (79 more sets:
cars, co2, quakes, state, volcano, ...) in `datasets-r`, plus digits,
optdigits, adult, auto-mpg, abalone, wine-quality, bank-marketing,
heart-disease, mushroom, car-evaluation, spambase, banknote-authentication,
seeds, concrete-strength, energy-efficiency, student-performance,
bike-sharing-daily, and bike-sharing-hourly in `datasets-uci`. These are
bundled, so loading them downloads nothing.

```sh
raco pkg install --auto datasets
```

The 20 datasets of ISLR2 (auto, boston, hitters, smarket, wage, ...) are in
`datasets-islr`. `datasets-fetch` lists larger sets such as covtype and mnist
that are downloaded from upstream and converted on first load; it ships no
data. For the lightweight table API alone, install `datasets-core` instead.
From a checkout, install the six packages with:

```sh
raco pkg install ./datasets-core ./datasets-r ./datasets-uci ./datasets-islr ./datasets-fetch
raco pkg install --auto ./datasets
```

```racket
(require datasets)
(load-iris)                        ; independent Polars dataframe
(load-dataset 'mtcars)
(dataset-names)                    ; sorted symbols
(dataset-info 'iris)               ; deeply immutable metadata
(load-iris #:format 'data-frame)
(load-iris #:format 'table)
(load-iris #:format 'matrix
           #:columns '("sepal-length" "sepal-width" "petal-length" "petal-width"))
```

Every `load-NAME` accepts the same keywords. Columns use lowercase kebab-case;
source abbreviations such as `mpg` remain. Selection preserves requested order.
Empty selections and duplicate names are rejected. Matrices require numeric,
nonmissing values: categories are not encoded, nor missing values imputed.

The repository has six top-level, independently installable package directories:
`datasets-core/`, `datasets-r/`, `datasets-uci/`, `datasets-islr/`, `datasets-fetch/`, and `datasets/`. Each declares
`collection 'multi` and contributes distinct directories to the shared `datasets` collection.
`datasets-r` contributes `datasets/r-data`, found through its registry, and its
tests; it depends on `datasets-core`, and `datasets` depends on it. Its sets have
no named loader: use `(load-dataset 'penguins)`. `datasets-uci` contributes
`datasets/uci-data` the same way; `(load-dataset 'digits)`. `datasets-islr`
contributes `datasets/islr-data` the same way. `datasets-fetch` contributes
`datasets/fetch-data`, a registry of converted fetch-tier sets, and their
private converters; it requires `datasets-core` 0.3.

`datasets-core` contributes `datasets/core` and depends only on `base` at runtime.
It provides `load-dataset-table`, `dataset-names`, `dataset-info`, the
`dataset-table` constructor, table predicates and accessors, and a dedicated
`dataset-missing` singleton. Every table, loaded, generated or constructed, is
immutable throughout: column vectors, strings, and metadata with its nested lists
and hashes. Metadata describes the full source dataset, even on selected tables. Data is read lazily using runtime-relative paths.
Core's tests and Scribble manual build without Polars, math-lib, or data-frame.

`datasets` contributes the main loader module and `datasets/polars`,
`datasets/matrix`, and `datasets/data-frame`. Polars categories are strings,
missing observations are `polars-null`, and category order lives in metadata.
The data-frame adapter explicitly configures `dataset-missing` as NA. Since
`df-series-names` has unspecified order, use `data-frame-column-names` from
`datasets/data-frame` to retrieve the initial selection stored in the
`datasets:columns` frame property. It is a snapshot; update it after changing
columns yourself. Every dataframe load has independent storage.

## Data and licensing

| Dataset | Rows | Columns | Notes |
|---|---:|---:|---|
| mtcars | 32 | 12 | model identifier plus 11 variables |
| iris | 150 | 5 | corrected UCI bezdekIris.data |
| faithful | 272 | 2 | rounded durations retained |
| anscombe | 11 | 8 | wide quartet |
| airquality | 153 | 6 | all 44 missing cells retained |
| us-arrests | 50 | 5 | state identifiers, historical values retained |
| plant-growth | 30 | 2 | control/treatment labels |
| tooth-growth | 60 | 3 | OJ/VC labels and dose |
| titanic | 32 | 5 | contingency counts, including eight zero cells |
| air-passengers | 144 | 3 | year, month, count in thousands |
| penguins | 344 | 8 | palmerpenguins; species/island/sex labels, missing retained |
| penguins-raw | 344 | 17 | unprocessed Palmer Station LTER records |
| gapminder | 1704 | 6 | 142 countries, 1952–2007 every five years |
| datasaurus-dozen | 1846 | 3 | 13 datasets in long form, `dataset` column |
| titanic-passengers | 891 | 12 | Kaggle training set, one row per passenger |
| nycflights13-airlines | 16 | 2 | carrier codes and airline names |
| nycflights13-airports | 1458 | 8 | FAA codes, location, altitude, time zone |
| nycflights13-planes | 3322 | 9 | FAA registry metadata by tail number |
| nycflights13-weather | 26115 | 15 | hourly weather at EWR, JFK, LGA; UTC `time-hour` |
| diabetes | 442 | 11 | lars standardized predictors and response |
| breast-cancer | 569 | 32 | measurements, M/B labels, source ID |
| wine | 178 | 14 | measurements and cultivar labels |

New code is **Apache-2.0 OR MIT**. Bundled data retains the accepted upstream
redistribution terms: [R's GPL-2/GPL-3 terms](https://www.r-project.org/Licenses/),
GPL-2 for [lars 1.3](https://CRAN.R-project.org/package=lars), and CC BY 4.0 for
[UCI Iris](https://archive.ics.uci.edu/dataset/53/iris),
[Wine](https://archive.ics.uci.edu/dataset/109/wine), and
[Breast Cancer Wisconsin (Diagnostic)](https://archive.ics.uci.edu/dataset/17/breast+cancer+wisconsin+diagnostic).
The `datasets-r` sets are CC0 (palmerpenguins 0.1.1, gapminder 1.0.1,
titanic 0.1.0, nycflights13 1.0.2), MIT (datasauRus 0.1.9), and GPL-2 or GPL-3 (R 4.3.3 `datasets`).
The `datasets-uci` sets are CC BY 4.0 per their UCI Machine Learning Repository pages.
This is a redistribution basis, not a claim of independent original-owner
permission. The `datasets-islr` sets are GPL-2
([ISLR2 1.3-2](https://CRAN.R-project.org/package=ISLR2)). `datasets-fetch`
redistributes no data; each entry names the upstream terms of use. See [NOTICE](datasets-core/NOTICE) and
[datasets-r/NOTICE](datasets-r/NOTICE), [datasets-uci/NOTICE](datasets-uci/NOTICE),
[datasets-islr/NOTICE](datasets-islr/NOTICE), bundled license texts, and upstream documentation under
`datasets-core/datasets/provenance/`, `datasets-r/datasets/r-data/provenance/`,
`datasets-uci/datasets/uci-data/provenance/`, and `datasets-islr/datasets/islr-data/provenance/`.
The registry records original spellings, units, descriptions, levels, identifiers,
suggested targets, citations, transformation notes, and source/normalized SHA-256.
Diabetes does not promise raw measurements or exact scikit-learn parity.

## Development and verification

```sh
nix build                  # complete package, tests, manuals, dependency checks
nix build .#core            # lightweight package, independent installation
nix flake check
nix develop                # isolated environment, no R tooling
```

Outputs cover x86-64/AArch64 Linux and macOS. CI runs Nix on Linux x86-64 and
macOS AArch64, plus ordinary Racket 9.3/stable installation checks. The flake
pins sources and reuses Polars' native derivation and prefetched dependency
closure. Intel macOS uses Polars' older pinned platform packages with the
Racket 9.3 recipes, since the newer nixpkgs pin has dropped that platform.
Installation, examples, tests, and documentation run offline.

For ordinary Racket, separate dependency fetching from validation:

```sh
bash scripts/prefetch.sh /tmp/datasets-dependencies  # network phase
bash scripts/install-check.sh /tmp/datasets-dependencies/sources core
bash scripts/install-check.sh /tmp/datasets-dependencies/sources full
```

The full checks require Polars' native library, either its packaged binary or
`RKT_POLARS_COMPAT_LIB_PATH` pointing to the native derivation. Both manuals have a narrative guide and a separate API reference, with evaluated
examples and cross-references. The core manual also includes the generated catalog.
Their entry points are [datasets-core.scrbl](datasets-core/datasets/core-doc/datasets-core.scrbl)
and [datasets.scrbl](datasets/datasets/main-doc/datasets.scrbl).

## Reading the guide and reference locally

In the development shell, render both manuals and serve the ignored HTML output:

```sh
bash scripts/build-docs.sh
python3 -m http.server 8765 --bind 127.0.0.1 --directory doc
```

Open <http://127.0.0.1:8765/datasets/datasets-guide.html> for the guide or
<http://127.0.0.1:8765/datasets/datasets-reference.html> for the reference.
The lightweight core manual is at <http://127.0.0.1:8765/datasets-core/index.html>.
The preview uses Scribble's standard manual CSS, fonts, navigation, and API
boxes. It rebuilds into a clean staging directory and checks local links and
anchors before replacing `doc/`. Links between our manuals stay local; links to
Racket and dependency manuals go directly to `docs.racket-lang.org` (and require
internet access). Python 3, included in the development shell, performs the link
checks. Examples are evaluated while building the manuals; generated HTML and
preview screenshots stay untracked.

## Reimporting (maintainers only)

```sh
nix develop .#maintainer
python3 scripts/import-data.py --fetch /tmp/datasets-sources
python3 scripts/import-data.py /tmp/datasets-sources  # subsequent offline import
```

Python and R evaluate the hash-pinned R 4.3.3 source dataset definitions and
lars 1.3 archive; R's numeric export uses 17 significant digits for binary64
round trips. ZIP sources are also hash-verified before use. End-user builds
consume only the committed gzip-compressed `.rktd.gz` files under
`datasets-core/datasets/core-data`; registry checksums cover the uncompressed
text. `--package datasets-r` imports the CRAN teaching sets and R's `datasets`
package (through `scripts/export-r-datasets.R`) into
`datasets-r/datasets/r-data`, and `--package datasets-uci` the UCI sets into
`datasets-uci/datasets/uci-data` (Python only, with xlrd for one `.xls`
source); `--target DIR` writes elsewhere.
Reimport, inspect the diff, then run checks.
`--package datasets-islr` imports the ISLR2 sets into
`datasets-islr/datasets/islr-data`.
Source selection is in `scripts/sources.json`; original documentation is copied
alongside data. No X/y helpers are included in v0.1.

### Synthetic generator fixtures

The stream-1 golden values change only when the random stream version is bumped;
the scikit-learn parity fixture only when the reference version changes:

```sh
racket scripts/synthetic-golden.rkt  # core-tests/fixtures/synthetic-golden.rktd
nix shell nixpkgs#python3Packages.scikit-learn -c python3 scripts/sklearn-synthetic-fixtures.py
```

## Downstream integration without a dependency cycle

Depend on `datasets-core`, then use `(require datasets/core)` and your own Polars
constructors, as in [the runnable example](examples/core-with-polars.rkt).
Polars documentation must not depend on the adapter package `datasets`.
For downstream Nix, fetch this repository as source:

```nix
inputs.datasets-src = { url = "github:bkc39/datasets/REV"; flake = false; };
# In an existing Racket build, offline:
# raco pkg install --batch --deps fail --copy --name datasets-core \
#   ${datasets-src}/datasets-core
```

Pin REV to the revision required by your downstream build. This does not import the complete flake
or create a reverse native dependency. Downstream repositories are unchanged;
GitHub/catalog publication and example migrations are separate work.
