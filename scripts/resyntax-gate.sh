#!/usr/bin/env bash
#   scripts/resyntax-gate.sh [BASE_REF [BASE_NAME]]
# Resyntax exits 0 with suggestions, so they are matched in its output.
set -euo pipefail

base_ref=${1:-origin/master}
fix_ref=${2:+origin/$2}

fail() {
  echo "::error::$1"
  echo "resyntax: resyntax-gate.rkt:0:0 [gate] $1"
  exit 1
}

if ! racket -l racket/base -l pkg/lib \
     -e "(exit (if (hash-ref (installed-pkg-table #:scope (quote user)) \"resyntax\" #f) 0 1))"; then
  fail "The pinned Resyntax is not installed; enter the dev shell (nix develop)."
fi

rc=0
out=$(racket -l- resyntax/cli analyze \
        --local-git-repository . "$base_ref" --analyzer-timeout 30000 2>&1) || rc=$?
echo "$out"
if [ "$rc" -ne 0 ]; then
  fail "Resyntax exited $rc without completing the analysis."
fi

status=0
broken=$(grep -B1 -E "^ encountered an error during (initial analysis|macro expansion)" <<<"$out" \
           | sed -n "s/^resyntax: skipping //p" | sort -u | paste -sd " " || true)
if [ -n "$broken" ]; then
  echo "::error::Resyntax could not analyze $broken; see each error above."
  echo "resyntax: resyntax-gate.rkt:0:0 [gate] could not analyze $broken"
  status=1
fi
if grep -qE "resyntax: .*\.rkt:[0-9]+:[0-9]+ \[" <<<"$out"; then
  echo "::error::Resyntax found refactoring suggestions. Run: racket -l- resyntax/cli fix --local-git-repository . ${fix_ref:-$base_ref}"
  status=1
fi
timeouts=$(grep -c "timed out" <<<"$out" || true)
if [ "$timeouts" -gt 0 ]; then
  echo "::warning::Resyntax analyzer timed out on $timeouts file(s); that part of the analysis was partial."
fi
if [ "$status" -eq 0 ]; then
  echo "Resyntax: no suggestions."
fi
exit "$status"
