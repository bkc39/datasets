"""Read <id>.csv and <id>.types written by scripts/export-r.R."""
import csv

CAST = {"integer": int, "double": float, "character": str, "factor": str}


def read_r_table(directory, id):
    """Return the header, the R types and the rows, NA as None."""
    with (directory / f"{id}.types").open(encoding="utf-8") as f:
        kinds = [row["type"] for row in csv.DictReader(f)]
    with (directory / f"{id}.csv").open(encoding="utf-8", newline="") as f:
        header, *records = list(csv.reader(f))
    rows = [[None if v == "NA" else CAST[k](v) for v, k in zip(r, kinds)] for r in records]
    return header, kinds, rows
