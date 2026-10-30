"""Read the CSV files written by scripts/export-r.R and scripts/export-r-datasets.R, and committed .rktd data."""
import csv
import json
import re
import subprocess
from pathlib import Path

R_TYPES = {"integer": (int, "integer"), "double": (float, "real"), "character": (str, "string"),
           "factor": (str, "categorical")}


def read_csv_rows(directory, name):
    """Return the rows of directory/name as dictionaries keyed by the header."""
    with (directory / name).open(encoding="utf-8", newline="") as f:
        return list(csv.DictReader(f))


def read_r_levels(directory, id, rename=lambda column: column):
    """Return the factor levels of <id>.levels by renamed column, in level order."""
    levels = {}
    for row in read_csv_rows(directory, f"{id}.levels"):
        levels.setdefault(rename(row["column"]), []).append(row["level"])
    return levels


def read_r_table(directory, id):
    """Return the header, the registry kind of each column and the rows, NA as None."""
    types = read_csv_rows(directory, f"{id}.types")
    unsupported = [f"{t['column']} ({t['type']})" for t in types if t["type"] not in R_TYPES]
    if unsupported:
        raise ValueError(f"{id}: unsupported R column types: {', '.join(unsupported)}")
    casts, kinds = zip(*(R_TYPES[t["type"]] for t in types))
    with (directory / f"{id}.csv").open(encoding="utf-8", newline="") as f:
        header, *records = list(csv.reader(f))
    rows = [[None if v == "NA" else cast(v) for v, cast in zip(r, casts)] for r in records]
    return header, list(kinds), rows


def kebab(name):
    """Lower-case name with each run of other characters replaced by one hyphen."""
    return re.sub("[^a-z0-9]+", "-", name.lower()).strip("-")


def read_rktd(path, *keys):
    """Return the datum of a .rktd or .rktd.gz file as JSON values, descending into the hash keys given."""
    reader = Path(__file__).resolve().parent / "rktd-json.rkt"
    result = subprocess.run(["racket", str(reader), str(path), *keys], check=True, capture_output=True, text=True)
    return json.loads(result.stdout)
