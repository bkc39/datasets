#!/usr/bin/env python3
"""Write fetch-tests/fixtures/parity.json, the column digests (digest.rkt) of scikit-learn's loaders.
  SCIKIT_LEARN_DATA=$HOME/.cache/datasets-parity nix shell --impure --expr '(builtins.getFlake "github:NixOS/nixpkgs/07e1d92cdc0ed416cfa11ff3ca40d17e61cfba7a").legacyPackages.x86_64-linux.python3.withPackages (p: [p.scikit-learn p.pandas])' -c python3 -I scripts/fetch-parity.py
"""
import hashlib
import json
from pathlib import Path
import numpy as np
import sklearn
from sklearn import datasets

VERSION = "1.8.0"
assert sklearn.__version__ == VERSION, sklearn.__version__
OUT = Path(__file__).resolve().parents[1] / "datasets-fetch/datasets/fetch-tests/fixtures/parity.json"

CALIFORNIA = {"MedInc": "med-inc", "HouseAge": "house-age", "AveRooms": "ave-rooms",
              "AveBedrms": "ave-bedrms", "Population": "population", "AveOccup": "ave-occup",
              "Latitude": "latitude", "Longitude": "longitude", "MedHouseVal": "med-house-val"}


def digest(tag, kind, values):
    cells = np.empty(len(values), dtype=[("tag", "S1"), ("value", kind)])
    cells["tag"] = tag
    cells["value"] = values
    return hashlib.sha256(cells.tobytes()).hexdigest()


def integers(values):
    assert np.array_equal(values, np.round(values)), "non-integer value"
    return digest(b"i", "<i8", values.astype(np.int64))


def reals(values):
    assert not np.isnan(values).any(), "missing value"
    return digest(b"f", "<f8", values.astype(np.float64))


def covtype():
    bunch = datasets.fetch_covtype()
    names = [n.lower().replace("_", "-") for n in bunch.feature_names + bunch.target_names]
    columns = [bunch.data[:, k] for k in range(bunch.data.shape[1])] + [bunch.target]
    return len(bunch.target), {n: integers(c) for n, c in zip(names, columns)}


def california_housing():
    frame = datasets.fetch_california_housing(as_frame=True).frame
    return len(frame), {CALIFORNIA[n]: reals(frame[n].to_numpy()) for n in frame.columns}


reference = {"versions": {"scikit-learn": VERSION}}
for name, load in [("covtype", covtype), ("california-housing", california_housing)]:
    rows, columns = load()
    reference[name] = {"rows": rows, "columns": columns}
OUT.write_text(json.dumps(reference, indent=1) + "\n")
