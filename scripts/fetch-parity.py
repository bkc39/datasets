#!/usr/bin/env python3
"""
scripts/fetch-parity.sh
nix develop .#maintainer -c python3 -I scripts/fetch-parity.py nycflights13 ARCHIVE_DIR
"""
import hashlib
import json
import os
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
NYCFLIGHTS13_TABLES = ["airlines", "airports", "flights", "planes", "weather"]

CALIFORNIA = {"MedInc": "med-inc", "HouseAge": "house-age", "AveRooms": "ave-rooms",
              "AveBedrms": "ave-bedrms", "Population": "population", "AveOccup": "ave-occup",
              "Latitude": "latitude", "Longitude": "longitude", "MedHouseVal": "med-house-val"}


def cell(value):
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


def cells(values):
    return hashlib.sha256(b"".join(cell(v) for v in values)).hexdigest()


def tagged(tag, kind, values):
    import numpy as np
    column = np.empty(len(values), dtype=[("tag", "S1"), ("value", kind)])
    column["tag"] = tag
    column["value"] = values
    return hashlib.sha256(column.tobytes()).hexdigest()


def integers(values):
    import numpy as np
    assert np.array_equal(values, np.round(values)), "non-integer value"
    return tagged(b"i", "<i8", values.astype(np.int64))


def reals(values):
    import numpy as np
    assert not np.isnan(values).any(), "missing value"
    return tagged(b"f", "<f8", values.astype(np.float64))


def images(loader, root):
    import numpy as np
    splits = [loader(root, train=train, download=True) for train in (True, False)]
    pixels = np.concatenate([s.data.numpy().reshape(-1, 784) for s in splits])
    labels = np.concatenate([s.targets.numpy() for s in splits])
    split = ["train"] * len(splits[0].targets) + ["test"] * len(splits[1].targets)
    columns = {f"pixel{k + 1}": integers(pixels[:, k]) for k in range(784)}
    columns["label"] = integers(labels)
    columns["split"] = cells(split)
    return len(labels), columns


def loader_references():
    import sklearn
    import torchvision
    from sklearn import datasets
    from torchvision.datasets import MNIST, FashionMNIST
    root = os.environ["TORCHVISION_ROOT"]
    bunch = datasets.fetch_covtype()
    names = [n.lower().replace("_", "-") for n in bunch.feature_names + bunch.target_names]
    columns = [bunch.data[:, k] for k in range(bunch.data.shape[1])] + [bunch.target]
    frame = datasets.fetch_california_housing(as_frame=True).frame
    return {"scikit-learn": sklearn.__version__, "torchvision": torchvision.__version__}, {
        "covtype": (len(bunch.target), {n: integers(c) for n, c in zip(names, columns)}),
        "california-housing": (len(frame), {CALIFORNIA[n]: reals(frame[n].to_numpy()) for n in frame.columns}),
        "mnist": images(MNIST, root),
        "fashion-mnist": images(FashionMNIST, root),
    }


def r_table(directory, id):
    header, _, rows = read_r_table(directory, id)
    names = [re.sub("[^a-z0-9]+", "-", n.lower()).strip("-") for n in header]
    return len(rows), {n: cells(r[j] for r in rows) for j, n in enumerate(names)}


def description_version(package_dir):
    fields = dict(line.split(": ", 1) for line in (package_dir / "DESCRIPTION").read_text().splitlines()
                  if re.match(r"^[A-Za-z]+: ", line))
    return fields["Version"]


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
        version = description_version(tmp / "nycflights13")
    return {"nycflights13": version}, tables


match sys.argv[1:]:
    case []:
        versions, tables = loader_references()
    case ["nycflights13", archive_dir]:
        versions, tables = nycflights13_references(archive_dir)
    case _:
        sys.exit(__doc__)
reference = json.loads(OUT.read_text()) if OUT.exists() else {"versions": {}}
reference["versions"].update(versions)
for name, (rows, columns) in tables.items():
    reference[name] = {"rows": rows, "columns": columns}
OUT.write_text(json.dumps(reference, indent=1) + "\n")
