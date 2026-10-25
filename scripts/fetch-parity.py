#!/usr/bin/env python3
"""Write fetch-tests/fixtures/parity.json: the column digests (digest.rkt) of scikit-learn's and torchvision's loaders
and the library versions they ran with, which scripts/fetch-parity.sh pins.
Run with scripts/fetch-parity.sh.
"""
import hashlib
import json
import os
import struct
from pathlib import Path
import numpy as np
import sklearn
import torchvision
from sklearn import datasets
from torchvision.datasets import MNIST, FashionMNIST

TORCHVISION_ROOT = os.environ["TORCHVISION_ROOT"]
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


def strings(values):
    out = hashlib.sha256()
    for v in values:
        b = v.encode("utf-8")
        out.update(b"s" + struct.pack("<I", len(b)) + b)
    return out.hexdigest()


def images(loader):
    splits = [loader(TORCHVISION_ROOT, train=train, download=True) for train in (True, False)]
    pixels = np.concatenate([s.data.numpy().reshape(-1, 784) for s in splits])
    labels = np.concatenate([s.targets.numpy() for s in splits])
    split = ["train"] * len(splits[0].targets) + ["test"] * len(splits[1].targets)
    columns = {f"pixel{k + 1}": integers(pixels[:, k]) for k in range(784)}
    columns["label"] = integers(labels)
    columns["split"] = strings(split)
    return len(labels), columns


def covtype():
    bunch = datasets.fetch_covtype()
    names = [n.lower().replace("_", "-") for n in bunch.feature_names + bunch.target_names]
    columns = [bunch.data[:, k] for k in range(bunch.data.shape[1])] + [bunch.target]
    return len(bunch.target), {n: integers(c) for n, c in zip(names, columns)}


def california_housing():
    frame = datasets.fetch_california_housing(as_frame=True).frame
    return len(frame), {CALIFORNIA[n]: reals(frame[n].to_numpy()) for n in frame.columns}


reference = {"versions": {"scikit-learn": sklearn.__version__, "torchvision": torchvision.__version__}}
for name, load in [("covtype", covtype), ("california-housing", california_housing),
                   ("mnist", lambda: images(MNIST)), ("fashion-mnist", lambda: images(FashionMNIST))]:
    rows, columns = load()
    reference[name] = {"rows": rows, "columns": columns}
OUT.write_text(json.dumps(reference, indent=1) + "\n")
