#!/usr/bin/env bash
# Render every manual into the ignored doc/ directory; packages must be installed.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
mkdir -p "$project_root/.cache"
staging=$(mktemp -d "$project_root/.cache/docs.XXXXXX")
trap 'rm -rf -- "$staging"' EXIT
raco setup --avoid-main --pkgs polars data-frame threading-doc
manuals=(datasets-core/datasets/core-doc/datasets-core.scrbl
  datasets-r/datasets/r-doc/datasets-r.scrbl
  datasets-uci/datasets/uci-doc/datasets-uci.scrbl
  datasets-islr/datasets/islr-doc/datasets-islr.scrbl
  datasets-fetch/datasets/fetch-doc/datasets-fetch.scrbl
  datasets/datasets/main-doc/datasets.scrbl)
installed=()
for manual in "${manuals[@]}"; do
  installed+=("$(dirname -- "$manual")/doc")
done
rm -rf -- "${installed[@]}"
source scripts/packages.sh
raco setup --pkgs "${packages[@]}" 2>&1 | tee "$staging/setup.log"
if grep -q '^raco setup: WARNING: undefined tag in <pkgs>/datasets' "$staging/setup.log"; then
  echo "build-docs: a datasets manual links an undocumented binding" >&2
  exit 1
fi
raco scribble --htmls --dest "$staging/site" --dest-base ../ \
  --redirect-main https://docs.racket-lang.org/ \
  --redirect https://docs.racket-lang.org/local-redirect/index.html \
  ++xref-in setup/xref load-collections-xref \
  "${manuals[@]}"
python3 scripts/finish-docs.py "$staging/site"
scanned=("$staging/site" "${installed[@]}")
for dir in "${scanned[@]}"; do
  if [[ ! -d $dir ]]; then
    echo "build-docs: $dir missing" >&2
    exit 1
  fi
done
scan=0
badlinks=$(grep -rl 'class="badlink"' "${scanned[@]}") || scan=$?
rejected="$project_root/.cache/docs-badlinks"
rm -rf -- "$rejected"
case $scan in
  0)
    mv "$staging/site" "$rejected"
    printf 'build-docs: broken links in\n%s\n' "${badlinks//"$staging/site"/"$rejected"}" >&2
    exit 1
    ;;
  1) ;;
  *)
    echo "build-docs: badlink scan failed" >&2
    exit 1
    ;;
esac
if [[ -e "$project_root/doc" ]]; then
  mv "$project_root/doc" "$staging/previous"
fi
mv "$staging/site" "$project_root/doc"
printf '%s\n' "Guide: $project_root/doc/datasets/datasets-guide.html" \
  "Reference: $project_root/doc/datasets/datasets-reference.html"
