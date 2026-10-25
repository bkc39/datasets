#!/usr/bin/env python3
"""Update fetch-tests/fixtures/parity.json, the column digests (digest.rkt) of the reference loaders.
scikit-learn's covtype and california-housing:
  SCIKIT_LEARN_DATA=$HOME/.cache/datasets-parity nix shell --impure --expr '(builtins.getFlake "github:NixOS/nixpkgs/07e1d92cdc0ed416cfa11ff3ca40d17e61cfba7a").legacyPackages.x86_64-linux.python3.withPackages (p: [p.scikit-learn p.pandas])' -c python3 -I scripts/fetch-parity.py
R's nycflights13 tables, from the hash-pinned CRAN tarball named in scripts/sources.json (in ARCHIVE_DIR), in the maintainer shell:
  nix develop .#maintainer -c python3 -I scripts/fetch-parity.py nycflights13 ARCHIVE_DIR
"""
import csv
import hashlib
import json
import re
import struct
import subprocess
import sys
import tarfile
import tempfile
from pathlib import Path

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

    def covtype():
        bunch = datasets.fetch_covtype()
        names = [n.lower().replace("_", "-") for n in bunch.feature_names + bunch.target_names]
        columns = [bunch.data[:, k] for k in range(bunch.data.shape[1])] + [bunch.target]
        return len(bunch.target), {n: integers(c) for n, c in zip(names, columns)}

    def california_housing():
        frame = datasets.fetch_california_housing(as_frame=True).frame
        return len(frame), {CALIFORNIA[n]: reals(frame[n].to_numpy()) for n in frame.columns}

    return {"scikit-learn": SKLEARN}, [("covtype", covtype), ("california-housing", california_housing)]


def r_cell(kind, text):
    if text == "NA":
        return b"m"
    if kind == "integer":
        return b"i" + struct.pack("<q", int(text))
    if kind == "double":
        return b"f" + struct.pack("<d", float(text))
    assert kind == "character", kind
    b = text.encode("utf-8")
    return b"s" + struct.pack("<I", len(b)) + b


def r_table(directory, id):
    with (directory / f"{id}.types").open(encoding="utf-8") as f:
        kinds = [row["type"] for row in csv.DictReader(f)]
    with (directory / f"{id}.csv").open(encoding="utf-8", newline="") as f:
        header, *records = list(csv.reader(f))
    names = [re.sub("[^a-z0-9]+", "-", n.lower()).strip("-") for n in header]
    columns = {n: hashlib.sha256(b"".join(r_cell(k, r[j]) for r in records)).hexdigest()
               for j, (n, k) in enumerate(zip(names, kinds))}
    return len(records), columns


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
    return {"nycflights13": NYCFLIGHTS13}, [(name, lambda t=t: t) for name, t in tables.items()]


match sys.argv[1:]:
    case []:
        versions, loads = sklearn_references()
    case ["nycflights13", archive_dir]:
        versions, loads = nycflights13_references(archive_dir)
    case _:
        sys.exit(__doc__)
reference = json.loads(OUT.read_text()) if OUT.exists() else {"versions": {}}
reference["versions"].update(versions)
for name, load in loads:
    rows, columns = load()
    reference[name] = {"rows": rows, "columns": columns}
OUT.write_text(json.dumps(reference, indent=1) + "\n")
