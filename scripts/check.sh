#!/usr/bin/env bash
# Offline checks. Dependencies must already be installed in PLTUSERHOME.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
mode=${1:-full}
if [[ "$mode" == full ]]; then
  # Render the manuals referenced by the evaluated guide examples.
  raco setup --avoid-main --pkgs polars data-frame threading-doc
  raco setup --no-docs --pkgs datasets-r datasets-islr
fi
raco setup --check-pkg-deps --unused-pkg-deps --pkgs datasets-core
raco test --timeout 600 -x datasets-core
if [[ "$mode" == full ]]; then
  raco setup --check-pkg-deps --unused-pkg-deps --pkgs datasets-r datasets-islr datasets
  raco test --timeout 600 -x datasets-r datasets-islr datasets
  racket examples/core-with-polars.rkt
fi
