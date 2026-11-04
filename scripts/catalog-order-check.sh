#!/usr/bin/env bash
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
source scripts/packages.sh
user_deps=$(racket scripts/user-deps.rkt "$core" "${families[@]}")
home=$(mktemp -d "${TMPDIR:-/tmp}/datasets-catalog-order.XXXXXX")
trap 'rm -rf -- "$home"' EXIT
export PLTUSERHOME=$home
while read -r name dir; do
  [[ -z $name ]] || raco pkg install --batch --deps fail --copy --no-setup --scope user --name "$name" "$dir"
done <<<"$user_deps"
manual() {
  local dir
  dir=$(racket -l racket/base -l pkg/lib -e "(display (pkg-directory \"$1\"))")
  printf '%s\n' "$dir/datasets/${1#datasets-}-doc/doc"
}
install_rendered() {
  raco pkg install --batch --deps fail --copy --no-setup --scope user --name "$1" "$project_root/$1"
  rm -rf -- "$(manual "$1")"
  raco setup -j 2 --pkgs "$1"
}
fail() {
  echo "catalog-order-check: $*" >&2
  exit 1
}
install_rendered "$core"
core_manual=$(manual "$core")
grep -rqF "datasets-package-$core" "$core_manual" || fail "the $core manual has no catalog section"
for family in "${families[@]}"; do
  install_rendered "$family"
done
for family in "${families[@]}"; do
  grep -rqF "datasets-package-$family" "$(manual "$family")" ||
    fail "$family's manual has no catalog section"
done
for package in "$core" "${families[@]}"; do
  ! grep -rlF 'class="badlink"' "$(manual "$package")" ||
    fail "broken links in the $package manual"
done
echo "catalog-order-check: ${families[*]} catalogued in their own manuals after $core"
