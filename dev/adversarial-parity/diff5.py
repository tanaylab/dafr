#!/usr/bin/env python3
"""Compare jl_out.jsonl vs r_out.jsonl line by line and emit a divergence report."""
import json, sys
from pathlib import Path

ROOT = Path("/home/aviezerl/src/dafr-native/dev/adversarial-parity")

def load(p):
    out = {}
    for line in p.read_text().splitlines():
        line = line.strip()
        if not line: continue
        r = json.loads(line)
        # Strip leading/trailing whitespace from query to match Julia/R
        r['query'] = r['query'].strip()
        out[r['query']] = r
    return out

def norm_vec(v):
    """Normalize a value list for fuzzy numeric comparison."""
    out = []
    for x in v:
        if isinstance(x, list):
            out.append(norm_vec(x))
        elif isinstance(x, str):
            # Recognize NaN/Inf strings
            if x.lower() in ('nan', 'inf', '-inf', 'infinity', '-infinity'):
                out.append('NaN' if 'nan' in x.lower() else x.lower())
            else:
                out.append(x)
        elif isinstance(x, bool):
            out.append(bool(x))
        elif isinstance(x, (int, float)):
            try:
                out.append(round(float(x), 6))
            except Exception:
                out.append(x)
        else:
            out.append(x)
    return out

def keyset(d, name):
    v = d.get(name)
    if v is None or v == 'NA': return None
    if isinstance(v, str): return [v]
    return list(v)

def compare(jl, r):
    j_status = jl.get('status')
    r_status = r.get('status')
    if j_status == 'error' and r_status == 'error':
        return ('both_error', f"jl={jl.get('error','')[:80]} || r={r.get('error','')[:80]}")
    if j_status == 'error' and r_status == 'ok':
        return ('jl_err_r_ok', f"jl={jl.get('error','')[:120]}")
    if j_status == 'ok' and r_status == 'error':
        return ('jl_ok_r_err', f"r={r.get('error','')[:200]}")
    # Both OK - compare results
    jr = jl.get('result', {})
    rr = r.get('result', {})
    jk = jr.get('kind'); rk = rr.get('kind')
    if jk != rk:
        return ('kind_mismatch', f"jl_kind={jk} type={jr.get('type')} | r_kind={rk} type={rr.get('type')}")
    if jk == 'scalar':
        jv = jr.get('value'); rv = rr.get('value')
        try:
            if abs(float(jv) - float(rv)) < 1e-6: return ('match', '')
        except Exception:
            pass
        if str(jv) == str(rv): return ('match', '')
        return ('scalar_diff', f"jl={jv} | r={rv}")
    if jk == 'vector':
        jvals = norm_vec(jr.get('values', []))
        rvals = norm_vec(rr.get('values', []))
        if jvals != rvals:
            return ('vector_diff', f"jl_len={len(jvals)} r_len={len(rvals)} jl={jvals[:6]} r={rvals[:6]}")
        jn = keyset(jr, 'names'); rn = keyset(rr, 'names')
        if jn is None and rn is None: return ('match', '')
        if jn != rn:
            return ('names_diff', f"jl={jn} | r={rn}")
        return ('match', '')
    if jk == 'matrix':
        jvals = norm_vec(jr.get('values', []))
        rvals = norm_vec(rr.get('values', []))
        if jvals != rvals:
            return ('matrix_diff', f"jl_dim={jr.get('dim')} r_dim={rr.get('dim')}")
        if keyset(jr,'rownames') != keyset(rr,'rownames') or keyset(jr,'colnames') != keyset(rr,'colnames'):
            return ('matrix_names_diff', '')
        return ('match', '')
    if jk == 'set_or_strvec':
        jv = sorted([str(x) for x in (jr.get('values') or [])])
        rv = sorted([str(x) for x in (rr.get('values') or [])])
        if jv == rv: return ('match', '')
        return ('set_diff', f"jl={jv} | r={rv}")
    return ('unknown_kind', f"kind={jk}")

def main():
    jl = load(ROOT / "julia_out5.jsonl")
    r  = load(ROOT / "r_out5.jsonl")
    # Use ordered union
    queries = []
    seen = set()
    for q in list(jl.keys()) + list(r.keys()):
        if q not in seen:
            queries.append(q); seen.add(q)
    rows = []
    for q in queries:
        j = jl.get(q); rr = r.get(q)
        if j is None:
            rows.append((q, 'missing_jl', '')); continue
        if rr is None:
            rows.append((q, 'missing_r', '')); continue
        verdict, detail = compare(j, rr)
        rows.append((q, verdict, detail))
    # Summary
    from collections import Counter
    c = Counter(v for _, v, _ in rows)
    print("=== SUMMARY ===")
    for k, v in c.most_common():
        print(f"  {k:25s} {v}")
    print(f"  TOTAL: {len(rows)}")
    print()
    print("=== DIVERGENCES ===")
    for q, v, d in rows:
        if v in ('match', 'both_error'): continue
        print(f"[{v}] {q}")
        if d: print(f"    {d}")
    print()
    print("=== BOTH_ERROR (parity OK, both rejected) ===")
    for q, v, d in rows:
        if v == 'both_error':
            print(f"[ok] {q}")

if __name__ == "__main__":
    main()
