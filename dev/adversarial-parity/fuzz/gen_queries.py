#!/usr/bin/env python3
"""Grammar-driven query fuzzer for the dafr/DAF.jl query DSL.

Emits N random valid query strings to stdout (one per line) for diff
against Julia. The grammar mirrors the dafr query parser and weights
choices toward edge-case patterns the round-N hand-crafted probes
under-sampled:

  - NaN/Inf/-Inf inputs through every reduction and eltwise
  - Random `type` tags on Sum/Mean/Min/Max/Mode/Quantile/Var/...
  - Integer-overflow-prone IfMissing defaults (` || 4294967296 Int8`)
  - Mask + comparator + groupby combinations
  - Empty-axis / empty-group paths
  - String inputs through numeric ops (parity-error probe)
  - Chained `=@`, lookup chains, `??` IfNot sentinels
  - Hex / binary / scientific notation literals

Usage:
  python3 gen_queries.py --n 500 --seed 42 > queries6.txt
"""

import argparse
import random
import sys

# --- Fixture schema (build_fixture.jl) ---------------------------------------
SCALARS = ["version", "title", "intver", "flag", "missing_scalar"]
AXES = ["cell", "gene", "batch", "type", "empty_axis"]

CELL_NUMERIC = [
    "age",        # Int64 [10..50]
    "score",      # Float64 [0.5, 1.5, 2.5, -1.0, 3.5]
    "neg_age",    # Int64 [-5, -1, 0, 1, 5]
    "all_zero",   # Float64 [0,0,0,0,0]
    "with_nan",   # Float64 [1, NaN, 3, NaN, 5]
    "all_nan",    # Float64 [NaN, NaN, NaN, NaN, NaN]
    "infs",       # Float64 [Inf, -Inf, NaN, 0, 1]
    "all_neg",    # Float64 [-1.5, ..., -5.5]
    "f32_score",  # Float32 like score
    "i8_age",     # Int8 [1..5]
    "i16_age",    # Int16 [100..500]
    "u32_count",  # UInt32 [10..50]
    "ties_int",   # Int32 [3, 0, 1, 0, 2]
]
CELL_BOOL = ["is_doublet", "is_low"]
CELL_STRING = ["type", "batch", "label", "subtype"]
CELL_MISSING = ["missing_prop"]
CELL_ALL = CELL_NUMERIC + CELL_BOOL + CELL_STRING + CELL_MISSING
GENE_NUMERIC = []
GENE_BOOL = ["is_lateral"]
GENE_STRING = ["marker"]
GENE_ALL = GENE_NUMERIC + GENE_BOOL + GENE_STRING
BATCH_ALL = ["donor"]
TYPE_ALL = ["color"]

PROPS_BY_AXIS = {
    "cell": CELL_ALL,
    "gene": GENE_ALL,
    "batch": BATCH_ALL,
    "type": TYPE_ALL,
    "empty_axis": [],
}

MATRICES = [
    ("cell", "gene", "UMIs", "Int32"),
    ("cell", "gene", "frac", "Float32"),
    ("cell", "gene", "sparse_umis", "Int32"),
    ("cell", "cell", "distance", "Int32"),
]

# Some entries known to exist on each axis (for `@ axis = entry` picks).
AXIS_ENTRIES = {
    "cell": ["A", "B", "C", "D", "E"],
    "gene": ["g1", "g2", "g3"],
    "batch": ["b1", "b2", "b3"],
    "type": ["U", "V", "W"],
}

# --- Operations grammar ------------------------------------------------------
ELTWISE_OPS = [
    "Abs", "Log", "Round", "Clamp", "Significant", "Convert", "Fraction"
]
ELTWISE_PARAMS = {
    "Log":         ["base", "eps"],
    "Clamp":       ["low", "high"],
    "Significant": ["high", "low"],
    "Convert":     ["type"],
    "Round":       ["digits", "type"],
    "Abs":         ["type"],
    "Fraction":    ["type"],
}

REDUCTION_OPS = [
    "Sum", "Mean", "Min", "Max", "Var", "Std", "VarN", "StdN",
    "GeoMean", "Median", "Quantile", "Mode", "Count"
]
REDUCTION_PARAMS = {
    "Quantile": ["p"],
    "VarN":     ["eps"],
    "StdN":     ["eps"],
    "GeoMean":  ["eps"],
    "Sum":      ["type"],
    "Mean":     ["type"],
    "Count":    ["type"],
    "Median":   ["type"],
    "Var":      ["type"],
    "Std":      ["type"],
    "VarN":     ["type", "eps"],
    "StdN":     ["type", "eps"],
}

# Numeric tokens with bias toward edge cases.
NUMERIC_LITERALS = [
    "0", "1", "-1", "0.5", "1.5", "-1.5", "10", "100",
    "0.0", "1.0", "-1.0", "1e10", "1e-10", "1e308",
    ".5", "5.",
    "NaN", "nan", "Inf", "-Inf", "inf", "Infinity",
    "127", "128", "255", "256", "-128",
    "32767", "65535", "-32768",
    "2147483647", "2147483648", "4294967295", "4294967296",
    "1_000", "0x10", "0b10",
]
NAMED_CONSTANTS = ["pi", "e"]

TYPE_TAGS = [
    "Float32", "Float64", "Int8", "Int16", "Int32", "Int64",
    "UInt8", "UInt16", "UInt32", "UInt64", "Bool",
    "float32", "float64", "int8", "int32", "bool",
    # The DSL accepts these lowercase aliases; the next group is invalid
    # in BOTH languages and should produce both_error.
    "String", "Symbol", "Char", "NotAType",
]

# Comparison operators (mask context only).
CMP_OPS = ["=", "!=", "<", "<=", ">", ">=", "~", "!~"]

# Boolean combinators inside masks.
LOGIC_OPS = ["&", "|", "^"]

# Reduction-to-vector (matrix axis collapse).
MATRIX_REDUCE_OPS = [">|", ">-"]

# Group-by direction on matrix.
MATRIX_GROUP_OPS = ["|/", "-/"]


# --- Generator helpers -------------------------------------------------------
def rchoice(seq, weights=None):
    if weights is None:
        return random.choice(seq)
    return random.choices(seq, weights=weights, k=1)[0]


def maybe(p=0.5):
    return random.random() < p


def gen_literal(allow_named=True):
    """Random scalar literal for IfMissing / comparators / params."""
    if allow_named and maybe(0.05):
        return rchoice(NAMED_CONSTANTS)
    return rchoice(NUMERIC_LITERALS)


def gen_type_tag(only_float=False, only_int=False):
    if only_float:
        return rchoice(["Float32", "Float64", "float32", "float64"])
    if only_int:
        return rchoice([
            "Int8", "Int16", "Int32", "Int64",
            "UInt8", "UInt16", "UInt32", "UInt64", "Bool"
        ])
    return rchoice(TYPE_TAGS)


def gen_eltwise(allow_chain=True):
    """Emit a `% Op [params]` fragment."""
    op = rchoice(ELTWISE_OPS)
    parts = ["%", op]
    for p in ELTWISE_PARAMS.get(op, []):
        if maybe(0.7):
            parts.append(p)
            if p == "type":
                parts.append(gen_type_tag())
            elif p == "digits":
                parts.append(rchoice(["0", "1", "2", "3"]))
            elif p in ("base", "eps", "low", "high", "p"):
                parts.append(gen_literal())
            else:
                parts.append(gen_literal())
    s = " ".join(parts)
    if allow_chain and maybe(0.15):
        s = s + " " + gen_eltwise(allow_chain=False)
    return s


def gen_reduction(matrix=False):
    """Emit a `>> Op`, `>- Op`, or `>| Op` fragment with random params."""
    op = rchoice(REDUCTION_OPS)
    if matrix:
        arrow = rchoice([">>", ">-", ">|"])
    else:
        arrow = ">>"
    parts = [arrow, op]
    for p in REDUCTION_PARAMS.get(op, []):
        if maybe(0.5):
            parts.append(p)
            if p == "type":
                parts.append(gen_type_tag())
            elif p == "p":
                parts.append(rchoice(
                    ["0", "0.25", "0.5", "0.75", "1", "-0.1", "1.1", "NaN"]
                ))
            elif p == "eps":
                parts.append(gen_literal())
            else:
                parts.append(gen_literal())
    return " ".join(parts)


def gen_if_missing():
    """Emit a `|| default [Type]` fragment."""
    val = gen_literal()
    if maybe(0.4):
        return f"|| {val} {gen_type_tag()}"
    if maybe(0.2):
        return f"|| '{rchoice(['hello world', 'a b', '', 'NaN', 'pi', '0', 'true'])}'"
    return f"|| {val}"


def gen_cmp_rhs(prop):
    """Pick a comparator RHS biased to the prop's type when known."""
    if prop in CELL_NUMERIC + CELL_BOOL:
        return rchoice(NUMERIC_LITERALS + ["0", "1", "true", "false"])
    if prop in CELL_STRING:
        return rchoice(["U", "V", "W", "b1", "Z", "''", "''", "a b"])
    return rchoice(NUMERIC_LITERALS)


def gen_mask(axis):
    """Emit a `[ <expr> ]` mask, possibly with logical combinators."""
    props = PROPS_BY_AXIS.get(axis, [])
    if not props:
        return ""
    n = rchoice([1, 1, 1, 2, 2, 3])
    clauses = []
    for _ in range(n):
        prop = rchoice(props + ["missing_prop"])
        op = rchoice(CMP_OPS)
        rhs = gen_cmp_rhs(prop)
        clauses.append(f"{prop} {op} {rhs}")
    # Combine with random logic ops, optional negation
    expr = clauses[0]
    if maybe(0.2):
        expr = "! " + expr
    for c in clauses[1:]:
        lop = rchoice(LOGIC_OPS)
        if maybe(0.2):
            c = "! " + c
        expr = f"{expr} {lop} {c}"
    return f"[ {expr} ]"


def gen_groupby(axis):
    """Emit a `/ group-prop [=@]` fragment (vector groupby)."""
    props = PROPS_BY_AXIS.get(axis, [])
    if not props:
        return ""
    prop = rchoice(props)
    suffix = " =@" if maybe(0.4) else ""
    return f"/ {prop}{suffix}"


def gen_vector_query():
    """Generate a vector-ish query: @ axis [mask] : prop [%op]... [/group] [>>op] [||default]."""
    axis = rchoice(["cell", "cell", "cell", "gene", "batch", "type", "empty_axis"])
    parts = [f"@ {axis}"]
    if PROPS_BY_AXIS.get(axis) and maybe(0.5):
        parts.append(gen_mask(axis))
    # Vector lookup
    if PROPS_BY_AXIS.get(axis):
        prop = rchoice(PROPS_BY_AXIS[axis] + ["missing_prop"])
        parts.append(f": {prop}")
        # Optional chained =@ then : prop2
        if maybe(0.15) and prop in CELL_STRING + ["batch"]:
            target_props = TYPE_ALL if prop == "type" else BATCH_ALL
            if target_props:
                parts.append("=@")
                parts.append(f": {rchoice(target_props)}")
                if maybe(0.3):
                    parts.append("=@")
        # Optional eltwise
        if maybe(0.5):
            parts.append(gen_eltwise())
        # Optional groupby
        if maybe(0.4):
            parts.append(gen_groupby(axis))
        # Optional reduction
        if maybe(0.5):
            parts.append(gen_reduction(matrix=False))
        # Optional IfMissing
        if maybe(0.25):
            parts.append(gen_if_missing())
    else:
        # empty_axis: just names query
        if maybe(0.5):
            parts.append(": ?")
    return " ".join(p for p in parts if p)


def gen_matrix_query():
    """Generate a matrix-ish query."""
    rows, cols, prop, _dtype = rchoice(MATRICES)
    parts = [f"@ {rows} @ {cols}"]
    if maybe(0.3):
        parts.append(gen_mask(rows))
    parts.append(f":: {prop}")
    if maybe(0.4):
        parts.append(gen_eltwise())
    if maybe(0.6):
        parts.append(gen_reduction(matrix=True))
    if maybe(0.2):
        parts.append(gen_if_missing())
    return " ".join(p for p in parts if p)


def gen_scalar_query():
    """Generate a scalar lookup."""
    sc = rchoice(SCALARS)
    parts = [f". {sc}"]
    if maybe(0.5):
        parts.append(gen_if_missing())
    if maybe(0.1):
        parts.append(gen_eltwise())
    return " ".join(parts)


def gen_names_query():
    forms = [
        ". ?",
        "@ ?",
        f"@ {rchoice(AXES)} : ?",
        f"@ {rchoice(['cell', 'gene'])} @ {rchoice(['cell', 'gene'])} :: ?",
    ]
    return rchoice(forms)


def gen_query():
    """Top-level generator: pick a shape and dispatch."""
    shape = random.choices(
        ["vector", "matrix", "scalar", "names"],
        weights=[10, 4, 2, 1], k=1,
    )[0]
    if shape == "vector":
        return gen_vector_query()
    if shape == "matrix":
        return gen_matrix_query()
    if shape == "scalar":
        return gen_scalar_query()
    return gen_names_query()


# --- Driver ------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=500)
    ap.add_argument("--seed", type=int, default=None)
    args = ap.parse_args()
    if args.seed is not None:
        random.seed(args.seed)
    seen = set()
    n = 0
    attempts = 0
    while n < args.n and attempts < args.n * 20:
        attempts += 1
        q = gen_query()
        # Deduplicate: many random combos repeat.
        if q in seen:
            continue
        seen.add(q)
        # Skip blatantly invalid (empty) outputs.
        if not q.strip():
            continue
        print(q)
        n += 1


if __name__ == "__main__":
    main()
