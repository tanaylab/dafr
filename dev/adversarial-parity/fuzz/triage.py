#!/usr/bin/env python3
"""Triage Round-6 fuzzer divergences by bucket.

Reads julia_out6.jsonl / r_out6.jsonl, classifies each query, and
groups divergences by a canonical "bucket" string so we surface novel
patterns without drowning in 200 instances of the same bug.

Usage:
  python3 triage.py [--show-examples N]

Buckets:
  - For jl_err vs r_ok / r_err vs jl_ok: the first 2-3 keywords of the
    erroring side's message
  - For match: counted, never shown
  - For both_error with mismatched messages: report (jl-keyword vs r-keyword)
  - For value diffs (scalar/vector/matrix): the value shape difference
"""

import json
import re
import sys
from collections import defaultdict
from pathlib import Path


ROOT = Path("/home/aviezerl/src/dafr-native/dev/adversarial-parity")


def load(p):
    out = {}
    for line in p.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        r = json.loads(line)
        r["query"] = r["query"].strip()
        out[r["query"]] = r
    return out


def err_keywords(msg, n=6):
    """Extract the first n non-stopword tokens from an error message."""
    if not msg:
        return ""
    # Strip Julia stack-trace tail and R "in: ..." location pointer.
    msg = msg.split("\nin:", 1)[0]
    msg = msg.split("\nat", 1)[0]
    msg = msg.split("in the query:", 1)[0]
    msg = msg.split("for the daf", 1)[0]
    # First line + a bit of context.
    first = msg.strip().split("\n")[0].strip()
    # Drop quoted values to make the bucket coarser.
    # Normalize ASCII quotes, fancy quotes (R's sQuote default), and
    # numeric magnitudes to coarse placeholders. Julia uses
    # plain "X"; R's sQuote uses ‘X’ / ’X’ (U+2018 / U+2019).
    first = re.sub(r"\"[^\"]*\"", '"X"', first)
    first = re.sub(r"'[^']*'", '"X"', first)
    first = re.sub(r"[‘’][^‘’]*[‘’]", '"X"', first)
    first = re.sub(r"-?\d+(\.\d+)?(e[+-]?\d+)?", "N", first)
    tokens = first.split()
    return " ".join(tokens[:n])


def bucket_for(jl, r):
    js = jl.get("status")
    rs = r.get("status")
    if js == "error" and rs == "error":
        jk = err_keywords(jl.get("error", ""))
        rk = err_keywords(r.get("error", ""))
        if jk == rk:
            return ("both_error_aligned", jk)
        return ("both_error_mismatch", f"jl={jk!r} :: r={rk!r}")
    if js == "error" and rs == "ok":
        return ("jl_err_r_ok", err_keywords(jl.get("error", "")))
    if js == "ok" and rs == "error":
        return ("jl_ok_r_err", err_keywords(r.get("error", "")))
    # Both OK -> compare results
    jr = jl.get("result") or {}
    rr = r.get("result") or {}
    jk = jr.get("kind")
    rk = rr.get("kind")
    if jk != rk:
        return ("kind_mismatch", f"jl={jk} | r={rk}")
    if jk == "scalar":
        if str(jr.get("value")) == str(rr.get("value")):
            return ("match", "")
        try:
            if abs(float(jr.get("value")) - float(rr.get("value"))) < 1e-6:
                return ("match", "")
        except Exception:
            pass
        return ("scalar_diff", f"jl_type={jr.get('type')} r_type={rr.get('type')}")
    if jk == "vector":
        jvals = jr.get("values", [])
        rvals = rr.get("values", [])
        if len(jvals) != len(rvals):
            return ("vector_len_diff", f"jl={len(jvals)} r={len(rvals)}")
        # Coarse: any value diff
        for a, b in zip(jvals, rvals):
            try:
                if abs(float(a) - float(b)) < 1e-6:
                    continue
            except Exception:
                pass
            if str(a) != str(b):
                return ("vector_val_diff", "")
        # Name diff: normalise single-string (jsonlite auto_unbox) to
        # 1-elem list so a length-1 R name "A" compares equal to
        # Julia's ["A"].
        jn = jr.get("names")
        rn = rr.get("names")
        if jn == "NA" or jn is None:
            jn = None
        elif isinstance(jn, str):
            jn = [jn]
        if rn == "NA" or rn is None:
            rn = None
        elif isinstance(rn, str):
            rn = [rn]
        if jn != rn and not (jn is None and rn is None):
            return ("vector_name_diff", "")
        return ("match", "")
    if jk == "matrix":
        # Fuzzy-compare matrix values to absorb Float32 vs Float64
        # serialisation precision drift (Julia's JSON3.write emits 0.1
        # for Float32(0.1); R's jsonlite emits 0.10000000149... since
        # R has no Float32 storage).
        jvals = jr.get("values", [])
        rvals = rr.get("values", [])
        if len(jvals) != len(rvals):
            return ("matrix_diff", "shape")
        for jrow, rrow in zip(jvals, rvals):
            if len(jrow) != len(rrow):
                return ("matrix_diff", "shape")
            for a, b in zip(jrow, rrow):
                try:
                    if abs(float(a) - float(b)) < 1e-5:
                        continue
                except Exception:
                    pass
                if str(a) != str(b):
                    return ("matrix_diff", "")
        return ("match", "")
    if jk == "set_or_strvec":
        return ("match" if sorted(jr.get("values") or []) ==
                sorted(rr.get("values") or []) else "set_diff", "")
    return ("unknown_kind", str(jk))


def main():
    show_examples = 1
    for i, arg in enumerate(sys.argv):
        if arg == "--show-examples":
            show_examples = int(sys.argv[i + 1])
        if arg.startswith("--show-examples="):
            show_examples = int(arg.split("=", 1)[1])
    jl = load(ROOT / "julia_out6.jsonl")
    r = load(ROOT / "r_out6.jsonl")
    all_q = list(jl.keys()) + [q for q in r if q not in jl]
    buckets = defaultdict(list)
    summary = defaultdict(int)
    for q in all_q:
        j = jl.get(q)
        rr = r.get(q)
        if j is None or rr is None:
            summary["missing"] += 1
            continue
        klass, detail = bucket_for(j, rr)
        summary[klass] += 1
        if klass not in ("match", "both_error_aligned"):
            buckets[(klass, detail)].append(q)
    print("=== SUMMARY ===")
    for k, v in sorted(summary.items(), key=lambda kv: -kv[1]):
        print(f"  {k:25s} {v}")
    print(f"  TOTAL: {sum(summary.values())}")
    print()
    print("=== NOVEL DIVERGENCE BUCKETS ===")
    sorted_buckets = sorted(
        buckets.items(), key=lambda kv: (-len(kv[1]), kv[0])
    )
    for (klass, detail), examples in sorted_buckets:
        print(f"\n[{klass}] ({len(examples)} queries) :: {detail}")
        for ex in examples[:show_examples]:
            print(f"    {ex}")
            if klass in ("jl_err_r_ok",):
                msg = jl[ex].get("error", "")
                print(f"        jl: {msg.splitlines()[0]}")
                rmsg = (r[ex].get("result") or {})
                print(f"        r : ok ({rmsg.get('kind')})")
            elif klass in ("jl_ok_r_err",):
                msg = r[ex].get("error", "")
                print(f"        r : {msg.splitlines()[0]}")
            elif klass == "both_error_mismatch":
                jm = jl[ex].get("error", "").splitlines()[0]
                rm = r[ex].get("error", "").splitlines()[0]
                print(f"        jl: {jm}")
                print(f"        r : {rm}")


if __name__ == "__main__":
    main()
