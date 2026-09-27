#!/usr/bin/env bash
# Network phase only; export source packages for subsequent offline installation.
set -euo pipefail
out=${1:?usage: prefetch.sh OUTPUT-DIRECTORY}
mkdir -p "$out"
out=$(cd "$out" && pwd)
export PLTUSERHOME=$(mktemp -d)
export TMPDIR=${TMPDIR:-/tmp}
raco pkg install --batch --auto --no-setup --scope user polars data-frame tzdata
mapfile -t packages < <(racket -e '(require pkg/lib) (for ([p (installed-pkg-names #:scope (quote user))]) (displayln p))')
raco pkg archive "$out/archive" "${packages[@]}"
mkdir -p "$out/sources"
for zip in "$out"/archive/pkgs/*.zip; do
  name=$(basename "$zip" .zip)
  mkdir -p "$out/sources/$name"
  unzip -q "$zip" -d "$out/sources/$name"
done
