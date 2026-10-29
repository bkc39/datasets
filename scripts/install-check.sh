#!/usr/bin/env bash
# Dependencies come exclusively from prefetch.sh; run this phase without network.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
if [[ $EUID -eq 0 ]] && [[ $(ip link show lo 2>/dev/null || true) == *"state DOWN"* ]]; then
  ip link set lo up
fi
sources=${1:?usage: install-check.sh PREFETCHED-SOURCES [core|full]}
mode=${2:-full}
export PLTUSERHOME=$(mktemp -d)
export TMPDIR=${TMPDIR:-/tmp}
if [[ "$mode" == full ]]; then
  raco pkg install --batch --deps fail --copy --no-setup --scope user "$sources"/*/
  raco setup --no-docs --pkgs tzdata polars data-frame
fi
raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-core "$project_root/datasets-core"
if [[ "$mode" == full ]]; then
  racket examples/core-with-polars.rkt
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-r "$project_root/datasets-r"
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-uci "$project_root/datasets-uci"
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-islr "$project_root/datasets-islr"
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-fetch "$project_root/datasets-fetch"
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets "$project_root/datasets"
fi
bash scripts/check.sh "$mode"
