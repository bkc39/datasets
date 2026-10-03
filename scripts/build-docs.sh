#!/usr/bin/env bash
# Render both manuals into the ignored doc/ directory; packages must be installed.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
mkdir -p "$project_root/.cache"
staging=$(mktemp -d "$project_root/.cache/docs.XXXXXX")
trap 'rm -rf -- "$staging"' EXIT
raco scribble --htmls --dest "$staging/site" --dest-base ../ \
  --redirect-main https://docs.racket-lang.org/ \
  --redirect https://docs.racket-lang.org/local-redirect/index.html \
  ++xref-in setup/xref load-collections-xref \
  datasets-core/datasets/core-doc/datasets-core.scrbl \
  datasets/datasets/main-doc/datasets.scrbl
python3 scripts/finish-docs.py "$staging/site"
# Replace the previous preview only after rendering and link checks succeed.
if [[ -e "$project_root/doc" ]]; then
  mv "$project_root/doc" "$staging/previous"
fi
mv "$staging/site" "$project_root/doc"
printf '%s\n' "Guide: $project_root/doc/datasets/datasets-guide.html" \
  "Reference: $project_root/doc/datasets/datasets-reference.html"
