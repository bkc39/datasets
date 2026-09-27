#!/usr/bin/env bash
# Render both manuals into the ignored doc/ directory; packages must be installed.
set -euo pipefail
project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$project_root"
raco scribble --htmls --dest "$project_root/doc" \
  --redirect-main https://docs.racket-lang.org/ \
  ++xref-in setup/xref load-collections-xref \
  datasets-core/datasets/core-doc/datasets-core.scrbl \
  datasets/datasets/main-doc/datasets.scrbl
printf '%s\n' "Guide: $project_root/doc/datasets/datasets-guide.html" \
  "Reference: $project_root/doc/datasets/datasets-reference.html"
