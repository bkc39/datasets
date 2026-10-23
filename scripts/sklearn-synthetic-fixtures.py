#!/usr/bin/env python3
"""Write the scikit-learn parity fixture for datasets/synthetic.
Run with scikit-learn from nixpkgs 07e1d92:
  nix shell --impure --expr '(builtins.getFlake "github:NixOS/nixpkgs/07e1d92cdc0ed416cfa11ff3ca40d17e61cfba7a").legacyPackages.x86_64-linux.python3.withPackages (p: [p.scikit-learn])' -c python3 -I scripts/sklearn-synthetic-fixtures.py
Only the deterministic parts are recorded (labels, noise-free curves, singular values);
the random stream is not NumPy's.
"""
from pathlib import Path
import numpy as np
import sklearn
from sklearn import datasets

VERSION = "1.8.0"
assert sklearn.__version__ == VERSION, sklearn.__version__
OUT = Path(__file__).resolve().parents[1] / "datasets-core/datasets/core-tests/fixtures/sklearn-synthetic.rktd"

# (case name, Racket generator, Racket keyword arguments, expected columns)
CASES = [
    ("moons-odd", "make-moons", [("n-samples", 7), ("shuffle?", False)], ["x0", "x1", "y"]),
    ("moons-split", "make-moons", [("n-samples", [2, 5]), ("shuffle?", False)], ["x0", "x1", "y"]),
    ("circles-odd", "make-circles", [("n-samples", 7), ("factor", 0.3), ("shuffle?", False)], ["x0", "x1", "y"]),
    ("circles-split", "make-circles", [("n-samples", [4, 1]), ("shuffle?", False)], ["x0", "x1", "y"]),
    ("circles-empty", "make-circles", [("n-samples", 0)], ["y"]),
    ("blobs-even", "make-blobs", [("n-samples", 10), ("centers", 3), ("shuffle?", False)], ["y"]),
    ("blobs-counts", "make-blobs",
     [("n-samples", [3, 0, 2]), ("centers", [[0, 0], [1, 1], [2, 2]]), ("shuffle?", False), ("cluster-std", 0)],
     ["x0", "x1", "y"]),
    ("class-default", "make-classification", [("n-samples", 100), ("flip-y", 0.0), ("shuffle?", False)], ["y"]),
    ("class-remainder", "make-classification",
     [("n-samples", 101), ("n-classes", 3), ("n-informative", 3), ("flip-y", 0.0), ("shuffle?", False)], ["y"]),
    ("class-weights-short", "make-classification",
     [("n-samples", 100), ("n-classes", 3), ("n-clusters-per-class", 1), ("weights", [0.1, 0.3]),
      ("flip-y", 0.0), ("shuffle?", False)], ["y"]),
    ("class-weights-odd", "make-classification",
     [("n-samples", 10), ("n-classes", 3), ("n-clusters-per-class", 1), ("weights", [0.5, 0.25, 0.25]),
      ("flip-y", 0.0), ("shuffle?", False)], ["y"]),
    ("quantiles", "make-gaussian-quantiles", [("n-samples", 10), ("n-classes", 3), ("shuffle?", False)], ["y"]),
]
LOW_RANK = [
    ("low-rank-even", [("n-samples", 20), ("n-features", 10), ("effective-rank", 3), ("tail-strength", 0.5)]),
    ("low-rank-tail", [("n-samples", 20), ("n-features", 10), ("effective-rank", 3), ("tail-strength", 0.2)]),
]


def python_name(keyword):
    return keyword.replace("-", "_").rstrip("?")


def python_value(keyword, value):
    return tuple(value) if keyword == "n-samples" and isinstance(value, list) else value


def kwargs(args):
    return {python_name(k): python_value(k, v) for k, v in args}


def sexp(x):
    if isinstance(x, bool):
        return "#t" if x else "#f"
    if isinstance(x, str):
        return '"' + x + '"'
    if isinstance(x, (list, tuple)):
        return "(" + " ".join(map(sexp, x)) + ")"
    if isinstance(x, (float, np.floating)):
        return repr(float(x))
    return str(int(x))


def keyword_args(args):
    return "(" + " ".join(f"#:{k} {sexp(v)}" for k, v in args) + ")"


def columns(table):
    return "#hash(" + " ".join(f"({sexp(n)} . {sexp(vs)})" for n, vs in table.items()) + ")"


def case(name, generator, args, wanted):
    X, y = getattr(datasets, generator.replace("-", "_"))(**kwargs(args), random_state=0)
    table = {f"x{j}": [float(v) for v in X[:, j]] for j in range(X.shape[1])}
    table["y"] = [int(v) for v in y]
    return f"({name} {generator} {keyword_args(args)} {columns({n: table[n] for n in wanted})})"


def low_rank_case(name, args):
    matrix = datasets.make_low_rank_matrix(**kwargs(args), random_state=0)
    s = np.linalg.svd(matrix, compute_uv=False)
    return f"({name} low-rank-profile {keyword_args(args)} {columns({'singular-values': list(s)})})"


cases = [case(*c) for c in CASES] + [low_rank_case(*c) for c in LOW_RANK]
OUT.write_text(f'#hash(("scikit-learn" . "{VERSION}")\n       ("cases" .\n        (' + "\n         ".join(cases) + ")))\n")
print(OUT)
