#!/usr/bin/env bash
# Offline checks. Dependencies must already be installed in PLTUSERHOME.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
mode=${1:-full}
if [[ "$mode" == full ]]; then
  # Render the manuals referenced by the evaluated guide examples.
  raco setup --pkgs polars data-frame threading-doc
  # Index sibling registries before core's catalog is rendered.
  raco setup --no-docs --pkgs datasets-r
fi
raco setup --check-pkg-deps --unused-pkg-deps --pkgs datasets-core
raco test --timeout 600 -x datasets-core
if [[ "$mode" == full ]]; then
  raco setup --check-pkg-deps --unused-pkg-deps --pkgs datasets-r datasets
  raco test --timeout 600 -x datasets-r datasets
  racket examples/core-with-polars.rkt
fi
