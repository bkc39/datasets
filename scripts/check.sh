#!/usr/bin/env bash
# Offline checks. Dependencies must already be installed in PLTUSERHOME.
set -euo pipefail
mode=${1:-full}
raco setup --check-pkg-deps --unused-pkg-deps --pkgs datasets-core
raco test -x pkgs/datasets-core
if [[ "$mode" == full ]]; then
  raco setup --check-pkg-deps --unused-pkg-deps --pkgs datasets
  raco test -x pkgs/datasets
  racket examples/core-with-polars.rkt
fi
