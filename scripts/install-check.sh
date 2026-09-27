#!/usr/bin/env bash
# Dependencies come exclusively from prefetch.sh; run this phase without network.
set -euo pipefail
sources=${1:?usage: install-check.sh PREFETCHED-SOURCES [core|full]}
mode=${2:-full}
export PLTUSERHOME=$(mktemp -d)
export TMPDIR=${TMPDIR:-/tmp}
if [[ "$mode" == full ]]; then
  raco pkg install --batch --deps fail --copy --no-setup --scope user "$sources"/*/
  raco setup --no-docs --pkgs tzdata polars data-frame
fi
raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-core ./pkgs/datasets-core
if [[ "$mode" == full ]]; then
  racket examples/core-with-polars.rkt
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets ./pkgs/datasets
fi
bash scripts/check.sh "$mode"
