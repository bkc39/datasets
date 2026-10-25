#!/usr/bin/env python3
"""
SCIKIT_LEARN_DATA=$HOME/.cache/datasets-parity nix shell --impure --expr '(builtins.getFlake "github:NixOS/nixpkgs/07e1d92cdc0ed416cfa11ff3ca40d17e61cfba7a").legacyPackages.x86_64-linux.python3.withPackages (p: [p.scikit-learn p.pandas])' -c python3 -I scripts/fetch-parity.py
nix develop .#maintainer -c python3 -I scripts/fetch-parity.py nycflights13 ARCHIVE_DIR
"""
import hashlib
import json
import re
import struct
import subprocess
import sys
import tarfile
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from rtable import read_r_table

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "datasets-fetch/datasets/fetch-tests/fixtures/parity.json"
SKLEARN = "1.8.0"
NYCFLIGHTS13 = "1.0.2"
NYCFLIGHTS13_TABLES = ["airlines", "airports", "flights", "planes", "weather"]

CALIFORNIA = {"MedInc": "med-inc", "HouseAge": "house-age", "AveRooms": "ave-rooms",
              "AveBedrms": "ave-bedrms", "Population": "population", "AveOccup": "ave-occup",
              "Latitude": "latitude", "Longitude": "longitude", "MedHouseVal": "med-house-val"}


def sklearn_references():
    import numpy as np
    import sklearn
    from sklearn import datasets
    assert sklearn.__version__ == SKLEARN, sklearn.__version__

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

    bunch = datasets.fetch_covtype()
    names = [n.lower().replace("_", "-") for n in bunch.feature_names + bunch.target_names]
    columns = [bunch.data[:, k] for k in range(bunch.data.shape[1])] + [bunch.target]
    frame = datasets.fetch_california_housing(as_frame=True).frame
    return {"scikit-learn": SKLEARN}, {
        "covtype": (len(bunch.target), {n: integers(c) for n, c in zip(names, columns)}),
        "california-housing": (len(frame), {CALIFORNIA[n]: reals(frame[n].to_numpy()) for n in frame.columns}),
    }


def r_cell(value):
    match value:
        case None:
            return b"m"
        case int():
            return b"i" + struct.pack("<q", value)
        case float():
            return b"f" + struct.pack("<d", value)
        case str():
            b = value.encode("utf-8")
            return b"s" + struct.pack("<I", len(b)) + b


def r_table(directory, id):
    header, _, rows = read_r_table(directory, id)
    names = [re.sub("[^a-z0-9]+", "-", n.lower()).strip("-") for n in header]
    columns = {n: hashlib.sha256(b"".join(r_cell(r[j]) for r in rows)).hexdigest() for j, n in enumerate(names)}
    return len(rows), columns


def nycflights13_references(archive_dir):
    source = json.loads((ROOT / "scripts/sources.json").read_text())["nycflights13"]
    archive = Path(archive_dir) / source["file"]
    assert hashlib.sha256(archive.read_bytes()).hexdigest() == source["sha256"], archive
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        with tarfile.open(archive) as a:
            a.extractall(tmp, filter="data")
        subprocess.run(["Rscript", str(ROOT / "scripts/export-r.R"), str(tmp), str(tmp), "nycflights13"],
                       check=True)
        tables = {f"nycflights13-{t}": r_table(tmp, f"nycflights13-{t}") for t in NYCFLIGHTS13_TABLES}
    return {"nycflights13": NYCFLIGHTS13}, tables


match sys.argv[1:]:
    case []:
        versions, tables = sklearn_references()
    case ["nycflights13", archive_dir]:
        versions, tables = nycflights13_references(archive_dir)
    case _:
        sys.exit(__doc__)
reference = json.loads(OUT.read_text()) if OUT.exists() else {"versions": {}}
reference["versions"].update(versions)
for name, (rows, columns) in tables.items():
    reference[name] = {"rows": rows, "columns": columns}
OUT.write_text(json.dumps(reference, indent=1) + "\n")
