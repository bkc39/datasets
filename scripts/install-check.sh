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
source scripts/packages.sh
install_package() {
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name "$1" "$project_root/$1"
}
export PLTUSERHOME=$(mktemp -d)
export TMPDIR=${TMPDIR:-/tmp}
if [[ "$mode" == full ]]; then
  raco pkg install --batch --deps fail --copy --no-setup --scope user "$sources"/*/
  raco setup --no-docs --pkgs tzdata polars data-frame
fi
install_package "$core"
if [[ "$mode" == full ]]; then
  racket examples/core-with-polars.rkt
  for package in "${dependents[@]}"; do
    install_package "$package"
  done
fi
bash scripts/check.sh "$mode"
