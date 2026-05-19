#!/usr/bin/env python3
"""Diff JSONL records from build_fixture round-trip runs.

Usage:
    python3 diff.py single_backend.jsonl
    python3 diff.py cross_backend.jsonl

Group by `key`, treat `memory` as reference, compare every other
backend against it. Report:

    - Per-backend error rate
    - Per-key divergence class (dtype, shape, names, value)
    - Bucketed summary by (kind, manifest_dtype, storage, shape-class)

Output:
    summary.txt   — human-readable buckets
    divergences.jsonl — per-record divergence with diagnostic detail
"""
from __future__ import annotations

import json
import sys
from collections import defaultdict, Counter
from pathlib import Path


def load(path: Path) -> list[dict]:
    out: list[dict] = []
    with path.open() as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            out.append(json.loads(line))
    return out


def shape_class(shape) -> str:
    if shape is None or shape == [] or shape == 0:
        return "scalar"
    if isinstance(shape, list):
        if len(shape) == 1:
            n = shape[0]
            if n == 0:
                return "empty_vec"
            if n == 1:
                return "single_vec"
            return f"vec_n={n}"
        if len(shape) == 2:
            nr, nc = shape
            if nr == 0 or nc == 0:
                return f"empty_mat[{nr}x{nc}]"
            if nr == 1 or nc == 1:
                return f"thin_mat[{nr}x{nc}]"
            if nr == nc:
                return f"sq_mat[{nr}]"
            return f"mat[{nr}x{nc}]"
    # fallthrough for ints
    return f"shape={shape}"


def diff_pair(ref: dict, cmp: dict) -> dict | None:
    """Compare cmp against ref. Returns a divergence record or None."""
    classes: list[str] = []
    detail: dict = {}
    if ref["status"] != cmp["status"]:
        classes.append("status")
        detail["status"] = (ref["status"], cmp["status"])
        if cmp["status"] == "error":
            detail["error"] = cmp.get("error")
    if ref.get("dtype") != cmp.get("dtype"):
        classes.append("dtype")
        detail["dtype"] = (ref.get("dtype"), cmp.get("dtype"))
    if ref.get("shape") != cmp.get("shape"):
        classes.append("shape")
        detail["shape"] = (ref.get("shape"), cmp.get("shape"))
    if ref.get("storage") != cmp.get("storage"):
        # Acceptable: dense vs sparse if same value_hash (rare).
        if ref.get("value_hash") != cmp.get("value_hash"):
            classes.append("storage")
            detail["storage"] = (ref.get("storage"), cmp.get("storage"))
    if ref.get("names_hash") != cmp.get("names_hash"):
        classes.append("names")
        detail["names"] = (ref.get("names"), cmp.get("names"))
        detail["rownames"] = (ref.get("rownames"), cmp.get("rownames"))
        detail["colnames"] = (ref.get("colnames"), cmp.get("colnames"))
    if ref.get("value_hash") != cmp.get("value_hash"):
        classes.append("value")
        if ref.get("value") is not None or cmp.get("value") is not None:
            detail["value_ref"] = ref.get("value")
            detail["value_cmp"] = cmp.get("value")
        if ref.get("sparse_x") is not None or cmp.get("sparse_x") is not None:
            detail["sparse_p_ref"] = ref.get("sparse_p")
            detail["sparse_p_cmp"] = cmp.get("sparse_p")
            detail["sparse_i_ref"] = ref.get("sparse_i")
            detail["sparse_i_cmp"] = cmp.get("sparse_i")
            detail["sparse_x_ref"] = ref.get("sparse_x")
            detail["sparse_x_cmp"] = cmp.get("sparse_x")
    if not classes:
        return None
    return {
        "key": ref["key"],
        "kind": ref["kind"],
        "manifest_dtype": ref.get("manifest_dtype"),
        "ref_backend": ref["backend"],
        "cmp_backend": cmp["backend"],
        "classes": classes,
        "shape_class": shape_class(cmp.get("shape") or ref.get("shape")),
        "storage": cmp.get("storage") or ref.get("storage"),
        "detail": detail,
    }


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    in_path = Path(argv[1])
    records = load(in_path)
    by_key: dict[str, dict[str, dict]] = defaultdict(dict)
    backends: set[str] = set()
    for rec in records:
        by_key[rec["key"]][rec["backend"]] = rec
        backends.add(rec["backend"])
    print(f"loaded: {len(records)} records / {len(by_key)} keys / "
          f"backends={sorted(backends)}")

    ref_name = "memory" if "memory" in backends else sorted(backends)[0]
    cmp_names = sorted(b for b in backends if b != ref_name)

    # Per-backend error count
    err_count = Counter()
    for rec in records:
        if rec.get("status") == "error":
            err_count[rec["backend"]] += 1
    print(f"\nerror rates (reference = {ref_name}):")
    for b in sorted(backends):
        print(f"   {b:<10} errors={err_count[b]}")

    divergences: list[dict] = []
    bucket_counter: Counter = Counter()
    class_counter: Counter = Counter()
    per_backend_div = Counter()

    for key, by_backend in by_key.items():
        ref = by_backend.get(ref_name)
        if ref is None:
            continue
        for cmp_name in cmp_names:
            cmp = by_backend.get(cmp_name)
            if cmp is None:
                # Composer / partial-coverage backends legitimately
                # omit some keys (e.g. concatenate can't handle a
                # matrix with both axes in the concat set). Treat
                # such absences as out-of-scope, not a divergence.
                continue
            d = diff_pair(ref, cmp)
            if d is None:
                continue
            divergences.append(d)
            per_backend_div[cmp_name] += 1
            sc = d["shape_class"]
            storage = d["storage"] or "?"
            mdt = d["manifest_dtype"] or "?"
            kind = d["kind"]
            for c in d["classes"]:
                class_counter[c] += 1
                bucket = (cmp_name, kind, mdt, storage, sc, c)
                bucket_counter[bucket] += 1

    print(f"\ntotal divergences: {len(divergences)}")
    print("per-backend divergence count:")
    for b in cmp_names:
        print(f"   {b:<10} {per_backend_div[b]}")
    print("\nby divergence class:")
    for cls, n in class_counter.most_common():
        print(f"   {cls:<10} {n}")

    if bucket_counter:
        print("\nbuckets (>= 1):  cmp_backend / kind / manifest_dtype / "
              "storage / shape_class / class -> count")
        for k, n in bucket_counter.most_common():
            cb, kind, mdt, sto, sc, cls = k
            print(f"   {n:>4}  {cb:<7} {kind:<6} {mdt:<7} {sto:<6} "
                  f"{sc:<14} {cls}")

    out_path = in_path.parent / (in_path.stem + "_divergences.jsonl")
    with out_path.open("w") as f:
        for d in divergences:
            f.write(json.dumps(d, ensure_ascii=False) + "\n")
    print(f"\nwrote divergences -> {out_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
