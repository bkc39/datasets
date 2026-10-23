#!/usr/bin/env bash
# Render both manuals into the ignored doc/ directory; packages must be installed.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
mkdir -p "$project_root/.cache"
staging=$(mktemp -d "$project_root/.cache/docs.XXXXXX")
trap 'rm -rf -- "$staging"' EXIT
raco setup --avoid-main --pkgs polars data-frame threading-doc
rm -rf -- "$project_root/datasets-core/datasets/core-doc/doc" "$project_root/datasets/datasets/main-doc/doc"
raco setup --pkgs datasets-core datasets-r datasets-islr datasets 2>&1 | tee "$staging/setup.log"
if grep -q '^raco setup: WARNING: undefined tag in <pkgs>/datasets' "$staging/setup.log"; then
  echo "build-docs: a datasets manual links an undocumented binding" >&2
  exit 1
fi
raco scribble --htmls --dest "$staging/site" --dest-base ../ \
  --redirect-main https://docs.racket-lang.org/ \
  --redirect https://docs.racket-lang.org/local-redirect/index.html \
  ++xref-in setup/xref load-collections-xref \
  datasets-core/datasets/core-doc/datasets-core.scrbl \
  datasets/datasets/main-doc/datasets.scrbl
python3 scripts/finish-docs.py "$staging/site"
if [[ -e "$project_root/doc" ]]; then
  mv "$project_root/doc" "$staging/previous"
fi
mv "$staging/site" "$project_root/doc"
printf '%s\n' "Guide: $project_root/doc/datasets/datasets-guide.html" \
  "Reference: $project_root/doc/datasets/datasets-reference.html"
