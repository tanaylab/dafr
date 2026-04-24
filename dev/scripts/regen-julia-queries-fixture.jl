# Regenerate the Julia query fixture for dafr Slice 3 tests.
#
# Run via:
#   conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
#     dev/scripts/regen-julia-queries-fixture.jl
#
# Produces:
#   tests/testthat/fixtures/julia-queries/fixture.json     — {query, canonical, kind, value}
#   tests/testthat/fixtures/julia-queries/example-daf/      — FilesDaf dump of example_cells_daf()
#
# Recon notes (DataAxesFormats.jl as of 2026-04):
#   - No `canonical_query` function; use string(parse_query(q)) for canonical form.
#   - copy_all! takes keyword args: copy_all!(source=src, destination=dst)
#   - Matrix queries use "::" (double colon), not ":"
#   - Eltwise Log syntax: "% Log base 2.0 eps 1.0" (no colons in named params)
#   - example_cells_daf() has:
#       axes: gene, experiment, donor, cell
#       scalars: organism, reference
#       gene vectors: is_lateral (Bool)
#       donor vectors: sex (String), age (UInt32)
#       cell vectors: experiment (String), donor (String)
#       matrices: gene x cell :: UMIs (also relayout as cell x gene)
#   - Grouped vector syntax (G1): "@ axis : numeric / group_vector >> ReductionOp"
#     E.g. "@ donor : age / sex >> Sum"
#   - Grouped matrix rows (G2): "@ row_axis @ col_axis :: matrix -/ group_vector >- ReductionOp"
#     E.g. "@ cell @ gene :: UMIs -/ experiment >- Sum"
#   - Grouped matrix cols (G3): "@ col_axis @ row_axis :: matrix |/ group_vector >| ReductionOp"
#     E.g. "@ gene @ cell :: UMIs |/ experiment >| Sum"
#   - Convert type names are Julia types: Int32, Float32, UInt8, etc. (not R names)
#   - Mode-on-char (string vector): G1 grouping where values are strings
#     E.g. "@ cell : experiment / donor >> Mode" — for each donor, most frequent experiment
#
# JSON is not used (unavailable without Manifest.toml); a minimal serialiser is inlined.

using DataAxesFormats
using DataAxesFormats.ExampleData
using DataAxesFormats.Queries
using NamedArrays
using SparseArrays

const DAFR_ROOT = dirname(dirname(@__DIR__))  # dev/scripts/ -> dev -> package root
const FIXTURE_DIR = joinpath(DAFR_ROOT, "tests", "testthat", "fixtures", "julia-queries")

mkpath(FIXTURE_DIR)

daf = example_cells_daf()

# ── Minimal JSON emitter ─────────────────────────────────────────────────────
# Only covers the subset of values produced by the queries below.

function json_str(s::AbstractString)
    buf = IOBuffer()
    write(buf, '"')
    for c in s
        if c == '"';  write(buf, "\\\"")
        elseif c == '\\'; write(buf, "\\\\")
        elseif c == '\n'; write(buf, "\\n")
        elseif c == '\r'; write(buf, "\\r")
        elseif c == '\t'; write(buf, "\\t")
        else write(buf, c)
        end
    end
    write(buf, '"')
    return String(take!(buf))
end

json_val(x::AbstractString, _indent=0) = json_str(x)
json_val(x::Bool, _indent=0)            = x ? "true" : "false"
json_val(x::Integer, _indent=0)         = string(x)
json_val(x::AbstractFloat, _indent=0)   = isfinite(x) ? string(x) : "null"
json_val(x::Number, _indent=0)          = string(x)

# Pretty-print arrays: each element on its own line, indented by (indent+2) spaces.
function json_val(x::AbstractVector, indent::Int=0)
    isempty(x) && return "[]"
    pad_item  = " " ^ (indent + 2)
    pad_close = " " ^ indent
    return "[\n" * pad_item * join(json_val.(x, indent + 2), ",\n" * pad_item) * "\n" * pad_close * "]"
end

# Pretty-print objects: each key:val on its own line, indented by (indent+2) spaces.
function json_obj(indent::Int, pairs::Pair...)
    items = String[]
    pad_item  = " " ^ (indent + 2)
    pad_close = " " ^ indent
    for (k, v) in pairs
        push!(items, pad_item * json_str(string(k)) * ": " * v)
    end
    return "{\n" * join(items, ",\n") * "\n" * pad_close * "}"
end

function json_arr(items::Vector{String})
    isempty(items) && return "[]"
    return "[\n  " * join(items, ",\n  ") * "\n]"
end

# ── Query set ────────────────────────────────────────────────────────────────
# Queries that exercise the DSL against example_cells_daf().
# Double-colon (::) is required for matrix lookup inside a two-axes scope.

const QUERIES = String[
    ". ?",
    ". organism",
    ". reference",
    "@ ?",
    "@ cell",
    "@ donor",
    "@ cell : ?",
    "@ donor : age",
    "@ donor : sex",
    "@ gene : is_lateral",
    "@ cell : donor",
    "@ donor [ age > 60 ]",
    "@ donor [ sex = M ]",
    "@ cell @ gene :: UMIs",
    "@ cell @ gene :: UMIs >| Sum",
    "@ cell @ gene :: UMIs >- Sum",
    "@ donor : age % Log base 2.0 eps 1.0",
    # Slice 7 additions (byte-parity cases for new eltwise + reduction ops;
    # Convert excluded — type-name vocabulary differs between Julia and R).
    "@ donor : age % Clamp min 20 max 60",
    "@ donor : age % Fraction",
    # Significant applied to Log-transformed ages rather than raw UInt32 ages.
    # Julia's `significant!` computes `-high .< vector`, which underflows when the
    # element type is an unsigned integer (`-UInt32(30)` wraps to a huge value), so
    # applying Significant directly to `age` preserves every value regardless of
    # threshold — a Julia bug we don't want R to replicate. Going through Log
    # first yields Float32 input, where -high behaves correctly.
    "@ donor : age % Log base 2.0 eps 1.0 % Significant high 6",
    "@ cell @ gene :: UMIs >| Var",
    "@ cell @ gene :: UMIs >| Std",
    "@ cell @ gene :: UMIs >| VarN eps 1",
    "@ cell @ gene :: UMIs >| StdN eps 1",
    "@ cell @ gene :: UMIs >| Median",
    "@ cell @ gene :: UMIs >| Quantile p 0.5",
    "@ cell @ gene :: UMIs >| GeoMean eps 1",
    "@ cell @ gene :: UMIs >| Mode",
    # Slice 9a additions:
    #
    # G1 grouped vector: "@ axis : numeric / group_vector >> ReductionOp"
    #   Produces a named vector of reduction results, one per group.
    "@ donor : age / sex >> Sum",
    "@ donor : age / sex >> Mean",
    "@ donor : age / sex >> Min",
    "@ donor : age / sex >> Max",
    "@ donor : age / sex >> Median",
    "@ donor : age / sex >> Quantile p 0.25",
    "@ donor : age / sex >> Var",
    "@ donor : age / sex >> Std",
    "@ donor : age / sex >> VarN eps 0.1",
    "@ donor : age / sex >> StdN eps 0.1",
    "@ donor : age / sex >> GeoMean",
    # G1 Mode: cell:experiment grouped by donor (string-valued column → Mode)
    "@ cell : experiment / donor >> Mode",
    # G2 grouped matrix rows: "-/ group_vector >- ReductionOp"
    #   Produces ngroups×ncols matrix (rows of the row-axis condensed into groups).
    "@ cell @ gene :: UMIs -/ experiment >- Sum",
    "@ cell @ gene :: UMIs -/ experiment >- Mean",
    "@ cell @ gene :: UMIs -/ experiment >- Max",
    "@ cell @ gene :: UMIs -/ experiment >- Var",
    # G3 grouped matrix cols: "|/ group_vector >| ReductionOp"
    #   Produces nrows×ngroups matrix (cols of the col-axis condensed into groups).
    "@ gene @ cell :: UMIs |/ experiment >| Sum",
    "@ gene @ cell :: UMIs |/ experiment >| Mean",
    "@ gene @ cell :: UMIs |/ experiment >| Max",
    # Convert eltwise: Int32 variants (byte-parity with R's integer type in 32-bit paths).
    #   Bool omitted intentionally: Julia's Bool() throws InexactError on values > 1,
    #   while R's as.logical() is permissive — intrinsic semantic divergence.
    "@ gene @ cell :: UMIs % Convert type Int32",
    # Int64 variants (byte-parity with R's integer64 / bit64)
    "@ donor : age % Convert type Int64",
    "@ gene : is_lateral % Convert type Int32",
    "@ gene @ cell :: UMIs % Convert type Int64",
]

# ── Serialise query results to a JSON-compatible value string ────────────────

function serialize_result(x::AbstractString)
    return "scalar", json_str(x)
end

function serialize_result(x::Bool)
    return "scalar", json_val(x)
end

function serialize_result(x::Number)
    return "scalar", json_val(x)
end

function serialize_result(x::Base.KeySet)
    sorted = sort(String.(collect(x)))
    return "names", json_val(sorted, 4)
end

function serialize_result(x::AbstractSet)
    sorted = sort(String.(collect(x)))
    return "names", json_val(sorted, 4)
end

function serialize_result(x::NamedArrays.NamedVector)
    nms    = String.(names(x, 1))
    vals   = collect(x)
    inner  = json_obj(4,
        "names"  => json_val(nms, 6),
        "values" => json_val(vals, 6),
    )
    return "vector", inner
end

function serialize_result(x::NamedArrays.NamedMatrix)
    m       = Matrix(x)
    rnms    = String.(names(x, 1))
    cnms    = String.(names(x, 2))
    flat    = [m[i, j] for i in axes(m, 1) for j in axes(m, 2)]
    inner   = json_obj(4,
        "rownames" => json_val(rnms, 6),
        "colnames" => json_val(cnms, 6),
        "values"   => json_val(flat, 6),
    )
    return "matrix", inner
end

function serialize_result(x::AbstractVector)
    nms  = string.(1:length(x))
    vals = collect(x)
    inner = json_obj(4,
        "names"  => json_val(nms, 6),
        "values" => json_val(vals, 6),
    )
    return "vector", inner
end

# ── Run queries and build records ────────────────────────────────────────────

records = String[]
for q_str in QUERIES
    @info "evaluating" q_str
    local result
    try
        result = get_query(daf, q_str)
    catch err
        @warn "skipping (get_query failed)" q_str err
        continue
    end

    local canon
    try
        canon = string(parse_query(q_str))
    catch err
        @warn "skipping (parse_query failed)" q_str err
        continue
    end

    local kind, value_json
    try
        kind, value_json = serialize_result(result)
    catch err
        @warn "skipping (serialize failed)" q_str err
        continue
    end

    rec = json_obj(2,
        "query"     => json_str(q_str),
        "canonical" => json_str(canon),
        "kind"      => json_str(kind),
        "value"     => value_json,
    )
    push!(records, rec)
end

fixture_path = joinpath(FIXTURE_DIR, "fixture.json")
open(fixture_path, "w") do f
    write(f, json_arr(records))
    write(f, "\n")
end

# ── Dump example_cells_daf() to FilesDaf ────────────────────────────────────
daf_path = joinpath(FIXTURE_DIR, "example-daf")
if isdir(daf_path)
    rm(daf_path; recursive = true)
end
fdaf = FilesDaf(daf_path, "w+")
copy_all!(source = daf, destination = fdaf)

@info "fixture written" path=FIXTURE_DIR n_queries=length(records) daf_path
