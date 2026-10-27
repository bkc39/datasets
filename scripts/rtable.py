"""Read the CSV files written by scripts/export-r.R and scripts/export-r-datasets.R."""
import csv

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
