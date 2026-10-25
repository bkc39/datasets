#!/usr/bin/env bash
set -euo pipefail
cache=${DATASETS_PARITY_CACHE:-$HOME/.cache/datasets-parity}
python='(builtins.getFlake "github:NixOS/nixpkgs/9eac87a12312b8f60dd52e1c6e1a265f6fc7f5fc").legacyPackages.x86_64-linux.python3.withPackages (p: [p.scikit-learn p.pandas p.torchvision])'
SCIKIT_LEARN_DATA=$cache TORCHVISION_ROOT=$cache/torchvision \
  nix shell --impure --expr "$python" -c python3 -I "$(dirname -- "${BASH_SOURCE[0]}")/fetch-parity.py"
