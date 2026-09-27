# datasets

Thirteen bundled datasets for Racket 9.3+: mtcars, iris, faithful, anscombe,
airquality, us-arrests, plant-growth, tooth-growth, titanic, air-passengers,
diabetes, breast-cancer, and wine. No downloads occur when loading data.

```sh
raco pkg install ./pkgs/datasets-core
raco pkg install --auto ./pkgs/datasets
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

`datasets-core` contributes `datasets/core` and depends only on `base` at runtime.
It provides `load-dataset-table`, `dataset-names`, `dataset-info`, table predicates
and accessors, and a dedicated `dataset-missing` singleton. Column vectors,
strings, and metadata are immutable. Metadata describes the full source dataset,
even on selected tables. Data is read lazily using runtime-relative paths.
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
| diabetes | 442 | 11 | lars standardized predictors and response |
| breast-cancer | 569 | 32 | measurements, M/B labels, source ID |
| wine | 178 | 14 | measurements and cultivar labels |

New code is **Apache-2.0 OR MIT**. Bundled data retains the accepted upstream
redistribution terms: [R's GPL-2/GPL-3 terms](https://www.r-project.org/Licenses/),
GPL-2 for [lars 1.3](https://CRAN.R-project.org/package=lars), and CC BY 4.0 for
[UCI Iris](https://archive.ics.uci.edu/dataset/53/iris),
[Wine](https://archive.ics.uci.edu/dataset/109/wine), and
[Breast Cancer Wisconsin (Diagnostic)](https://archive.ics.uci.edu/dataset/17/breast+cancer+wisconsin+diagnostic).
This is a redistribution basis, not a claim of independent original-owner
permission. See [NOTICE](pkgs/datasets-core/NOTICE), bundled license texts,
and upstream documentation under `pkgs/datasets-core/datasets/provenance/`.
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
`RKT_POLARS_COMPAT_LIB_PATH` pointing to the native derivation. The manuals are
`datasets/core-doc/datasets-core.scrbl` and `datasets/main-doc/datasets.scrbl`.

## Reimporting (maintainers only)

```sh
nix develop .#maintainer
python3 scripts/import-data.py --fetch /tmp/datasets-sources
python3 scripts/import-data.py /tmp/datasets-sources  # subsequent offline import
```

Python and R evaluate the hash-pinned R 4.3.3 source dataset definitions and
lars 1.3 archive; R's numeric export uses 17 significant digits for binary64
round trips. ZIP sources are also hash-verified before use. End-user builds
consume only committed `.rktd` files. Reimport, inspect the diff, then run checks.
Source selection is in `scripts/sources.json`; original documentation is copied
alongside data. No synthetic generators, caching, X/y helpers, or additional
datasets are included in v0.1.

## Downstream integration without a dependency cycle

Depend on `datasets-core`, then use `(require datasets/core)` and your own Polars
constructors, as in [the runnable example](examples/core-with-polars.rkt).
Polars documentation must not depend on the adapter package `datasets`.
For downstream Nix, fetch this repository as source:

```nix
inputs.datasets-src = { url = "github:OWNER/datasets/REV"; flake = false; };
# In an existing Racket build, offline:
# raco pkg install --batch --deps fail --copy --name datasets-core \
#   ${datasets-src}/pkgs/datasets-core
```

Replace OWNER/REV after publication. This does not import the complete flake
or create a reverse native dependency. Downstream repositories are unchanged;
GitHub/catalog publication and example migrations are separate work.
