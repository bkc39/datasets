#!/usr/bin/env bash
# Offline checks. Dependencies must already be installed in PLTUSERHOME.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
mode=${1:-full}
source scripts/packages.sh
raco test scripts/check-package-list.rkt
racket scripts/check-package-list.rkt
if [[ "$mode" == full ]]; then
  # Render the manuals referenced by the evaluated guide examples.
  raco setup --avoid-main --pkgs polars data-frame threading-doc
  raco setup --no-docs --pkgs "${families[@]}"
fi
raco setup --check-pkg-deps --unused-pkg-deps --pkgs "$core"
raco test --timeout 600 -x "$core"
if [[ "$mode" == full ]]; then
  raco setup --check-pkg-deps --unused-pkg-deps --pkgs "${dependents[@]}"
  raco test --timeout 600 -x "${dependents[@]}"
  racket examples/core-with-polars.rkt
fi
