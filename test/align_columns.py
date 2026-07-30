#!/usr/bin/env python3
"""
align_columns.py -- right-justify the columns of "simple table" SWAT+ input files so every
data value lines up under its column header.

WHAT IT DOES
------------
Walks each dataset folder under a root (default: the repo's workdata/), finds the input files
that are a single flat table, and rewrites them with every column right-justified to a common
width. Values are never changed -- only the whitespace between them. A rewritten file always
tokenizes to exactly the same sequence of tokens as the original; the script asserts this
before writing and refuses to write the file if it ever fails.

THE FILE SHAPE IT HANDLES
-------------------------
    line 1     free-text title/comment           (copied through verbatim, never touched)
    line 2     column headers                    (aligned)
    line 3     OPTIONAL units row                (aligned; see UNITS ROW below)
    line 4+    data rows                         (aligned)

Every data row must have exactly as many whitespace-separated fields as the header. That one
rule is what makes this safe, and it is why the script skips far more files than it rewrites:
anything with a nested, multi-row, or variable-width record fails it automatically. Skipped
files are reported with the reason, so nothing fails silently.

WHY SO MANY FILES ARE SKIPPED (all confirmed against the real workdata tree)
---------------------------------------------------------------------------
  print.prt, file.cio    Explicitly excluded -- multi-section files with several different
                         record shapes, not one table.
  soils.sol              A soil is a header row plus one continuation row per layer; the rows
                         legitimately have different field counts.
  wth.pcp/.tmp/.slr      Two stacked tables: a 5-column station record, then daily records
                         under no header of their own.
  weather-wgn.cli        A station row on line 2, then the real monthly header on line 3.
  *_PS.dat               A record-count line sits between the title and the header, so the
                         header is on line 3, not line 2.
  recall_db.rec          Data rows carry more fields than the 4-column header.
  management.sch,        Nested blocks: an operation-count row followed by that many
  plant.ini, lum.dtl     sub-rows.
  w_pcp.cli, *.cli lists Single-column file lists -- nothing to align.

UNITS ROW
---------
Some SWAT+ tables put a row of units between the header and the data. It is detected
structurally, not by a vocabulary of unit names: the row directly after the header qualifies
only if all of its tokens are non-numeric AND the row after it is numeric-heavy. When found it
is aligned like any other row.

Worth knowing: as of this writing NO file in the workdata tree actually has one. Units rows
appear in SWAT+ *output* files, not inputs. The support is here because the format allows it,
but that code path is unexercised by the current data -- treat it as untested.

WHY CHANGING THE SPACING IS SAFE
--------------------------------
SWAT+ reads these files with list-directed READs (`read (unit,*) ...`), which are
whitespace-delimited and completely indifferent to column positions. A survey of src/ found
381 list-directed reads and zero fixed-FORMAT numeric reads; the only formatted reads are two
`'(A)'` whole-line slurps (copy_file.f90, readcio_read.f90) that read into a character buffer.
So realigning cannot change how any value is parsed.

SAFETY
------
workdata/ is gitignored and has NO files tracked by git, so there is no version-control undo
for these edits. Accordingly:
  * dry-run is the DEFAULT -- nothing is written unless you pass --write
  * --backup DIR copies each file's original before rewriting it
  * a file is rewritten only if its token stream is provably unchanged
  * per-file line endings (CRLF vs LF) and the trailing-blank-line layout are preserved

USAGE
-----
    python3 test/align_columns.py                       # dry run over workdata/, summary
    python3 test/align_columns.py --show-skipped        # also list every skip + reason
    python3 test/align_columns.py --diff hydrology.hyd  # preview one file's before/after
    python3 test/align_columns.py --write --backup /tmp/wd-backup
    python3 test/align_columns.py --root workdata/racoon_creek_120hru --write

Exit code is 0 on success, 1 if any file failed its verification check.
"""

import argparse
import os
import re
import shutil
import sys

# Files that look tabular but are not a single table. See the module docstring.
SKIP_NAMES = {"print.prt", "file.cio"}

# Model output, build artifacts, and scratch files -- never inputs.
SKIP_RE = re.compile(
    r"\.(csv|txt|out|exe|o|mod|log|zip|gz|pdf|png|jpg)$|^fort\.\d+|^swatplus-|~$",
    re.IGNORECASE,
)

GAP = 2  # spaces between columns


def is_number(tok):
    """True if the token would read as a Fortran numeric literal."""
    try:
        float(tok.replace("d", "e").replace("D", "E"))
        return True
    except ValueError:
        return False


def numeric_fraction(tokens):
    return sum(map(is_number, tokens)) / len(tokens) if tokens else 0.0


def looks_like_units_row(rows, idx):
    """
    Structural units-row test for rows[idx]: every token non-numeric, and the row after it is
    numeric-heavy. Deliberately not a list of known unit names -- that would be a guess about
    vocabulary, this is a guess about shape, and shape is what actually distinguishes a units
    row from a row of text data (names, file references) that happens to follow the header.
    """
    if idx + 1 >= len(rows):
        return False
    here, nxt = rows[idx], rows[idx + 1]
    if not here or not nxt:
        return False
    return numeric_fraction(here) == 0.0 and numeric_fraction(nxt) > 0.6


def split_line(raw):
    """Strip a trailing CR, returning (text, line_ending_suffix)."""
    return (raw[:-1], "\r") if raw.endswith("\r") else (raw, "")


def align_text(text, min_width=0):
    """
    Return (new_text, note) with columns right-justified, or (None, reason) if the file is not
    a simple table. `text` is the whole file with '\\n' separators still intact.

    min_width floors every column's width. 0 gives the narrowest correct alignment; ~10 roughly
    reproduces the SWAT+ editor's roomier look.
    """
    lines = text.split("\n")

    # Trailing blank lines are layout, not content -- hold them aside and restore verbatim.
    tail = 0
    while lines and lines[-1].strip() == "":
        tail += 1
        lines.pop()
    if len(lines) < 3:
        return None, "fewer than 3 non-blank lines (no header + data)"

    bodies, crs = zip(*(split_line(ln) for ln in lines))
    bodies, crs = list(bodies), list(crs)

    rows = [b.split() for b in bodies]
    header = rows[1]
    if len(header) < 2:
        return None, f"header has {len(header)} column(s); nothing to align"

    units_idx = 2 if looks_like_units_row(rows, 2) else None

    # Every row from the header down must agree on field count. This is the safety rule.
    table_idx = [1] + ([units_idx] if units_idx else [])
    table_idx += [i for i in range(2 if units_idx is None else 3, len(rows))]
    for i in table_idx:
        if not rows[i]:
            return None, f"blank line inside the table at line {i + 1}"
        if len(rows[i]) != len(header):
            return None, (
                f"line {i + 1} has {len(rows[i])} fields but the header has "
                f"{len(header)} -- not a simple table"
            )

    widths = [
        max(min_width, max(len(rows[i][c]) for i in table_idx))
        for c in range(len(header))
    ]

    for i in table_idx:
        bodies[i] = "".join(
            " " * GAP + tok.rjust(w) for tok, w in zip(rows[i], widths)
        )

    out = "\n".join(b + cr for b, cr in zip(bodies, crs))
    out += "\n" * tail
    note = "units row aligned" if units_idx else ""
    return out, note


def tokens_of(text):
    return text.split()


def process(path, args, stats):
    name = os.path.basename(path)
    if name in SKIP_NAMES:
        stats["skipped"].append((path, "on the explicit skip list"))
        return
    if SKIP_RE.search(name):
        return  # not an input file; not worth reporting

    try:
        with open(path, "r", newline="", errors="strict") as fh:
            original = fh.read()
    except (UnicodeDecodeError, OSError) as exc:
        stats["skipped"].append((path, f"unreadable ({type(exc).__name__})"))
        return
    if "\0" in original:
        stats["skipped"].append((path, "binary"))
        return

    new, note = align_text(original, args.min_width)
    if new is None:
        stats["skipped"].append((path, note))
        return

    # Hard guarantee: whitespace-only change. Never write a file that fails this.
    if tokens_of(new) != tokens_of(original):
        stats["failed"].append((path, "token stream changed -- REFUSED"))
        return

    if new == original:
        stats["already"] += 1
        return

    stats["changed"].append((path, note))
    if args.diff and name == args.diff:
        show_diff(path, original, new)

    if args.write:
        if args.backup:
            dest = os.path.join(args.backup, os.path.relpath(path, args.root))
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            shutil.copy2(path, dest)
        with open(path, "w", newline="") as fh:
            fh.write(new)


def show_diff(path, before, after, context=4):
    print(f"\n----- {path} -----")
    b, a = before.split("\n"), after.split("\n")
    print("  BEFORE:")
    for ln in b[: context + 1]:
        print("    " + ln.rstrip("\r")[:160])
    print("  AFTER:")
    for ln in a[: context + 1]:
        print("    " + ln.rstrip("\r")[:160])


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    ap = argparse.ArgumentParser(
        description="Right-justify columns in simple-table SWAT+ input files.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    ap.add_argument(
        "--root",
        default=os.path.join(os.path.dirname(here), "workdata"),
        help="directory to walk (default: the repo's workdata/)",
    )
    ap.add_argument(
        "--write",
        action="store_true",
        help="actually rewrite files (default is a dry run that changes nothing)",
    )
    ap.add_argument("--backup", help="copy each original into this directory before rewriting")
    ap.add_argument(
        "--min-width",
        type=int,
        default=0,
        metavar="N",
        help="minimum width for every column (default 0 = narrowest correct alignment; "
        "try 10 for the roomier look the SWAT+ editor writes)",
    )
    ap.add_argument("--show-skipped", action="store_true", help="list every skipped file + reason")
    ap.add_argument("--diff", metavar="FILENAME", help="print before/after for files with this name")
    args = ap.parse_args()

    if not os.path.isdir(args.root):
        sys.exit(f"error: root {args.root!r} is not a directory")
    if args.backup and not args.write:
        print("note: --backup has no effect without --write (this is a dry run)\n")

    stats = {"changed": [], "skipped": [], "failed": [], "already": 0}
    for dirpath, dirnames, filenames in os.walk(args.root):
        dirnames.sort()
        for fn in sorted(filenames):
            process(os.path.join(dirpath, fn), args, stats)

    mode = "REWROTE" if args.write else "would rewrite (dry run)"
    print(f"\n=== {mode}: {len(stats['changed'])} file(s) ===")
    by_name = {}
    for path, note in stats["changed"]:
        by_name.setdefault(os.path.basename(path), []).append(note)
    for name in sorted(by_name):
        extra = " [units row]" if any(by_name[name]) else ""
        print(f"  {len(by_name[name]):4d}  {name}{extra}")

    print(f"\nalready aligned : {stats['already']}")
    print(f"skipped         : {len(stats['skipped'])}")
    if args.show_skipped:
        for path, why in stats["skipped"]:
            print(f"    {os.path.relpath(path, args.root)}: {why}")
    else:
        reasons = {}
        for _, why in stats["skipped"]:
            key = re.sub(r"\d+", "N", why)
            reasons[key] = reasons.get(key, 0) + 1
        for why, n in sorted(reasons.items(), key=lambda kv: -kv[1]):
            print(f"    {n:5d}  {why}")

    if stats["failed"]:
        print(f"\nFAILED VERIFICATION: {len(stats['failed'])}")
        for path, why in stats["failed"]:
            print(f"    {path}: {why}")
        return 1
    if not args.write and stats["changed"]:
        print("\n(dry run -- nothing written. Re-run with --write, ideally with --backup DIR.)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
