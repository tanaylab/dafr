# Regenerate the Julia adapter+computation fixture for dafr Slice 5 tests.
#
# Run via:
#   conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
#     dev/scripts/regen-julia-adapter-fixture.jl
#
# Produces:
#   tests/testthat/fixtures/julia-adapter/fixture.json
#
# JSON is not used (unavailable without Manifest.toml); a minimal serialiser is inlined.
# Mirrors dev/scripts/regen-julia-chains-fixture.jl's pattern.
#
# What this does:
#   1. Loads example_cells_daf() (856 cells x 683 genes, UMIs matrix UInt8).
#   2. Defines a @computation-wrapped sum_umis(adapted::DafWriter) that computes
#      per-cell UMI totals and stores them as the "total_umis" vector on "obs".
#   3. Calls adapter(daf; input_axes, input_data, output_axes, output_data)
#      renaming cell->obs, gene->var in the adapted view, and projecting
#      total_umis back as cell/total_umis in the base daf.
#   4. Emits fixture.json with daf_jl_head, result_returned, total_umis_length,
#      total_umis_values.

using DataAxesFormats

const DAFR_ROOT = dirname(dirname(@__DIR__))  # dev/scripts/ -> dev -> package root
const FIXTURE_DIR = joinpath(DAFR_ROOT, "tests", "testthat", "fixtures", "julia-adapter")

mkpath(FIXTURE_DIR)

# ── Minimal JSON emitter ─────────────────────────────────────────────────────
# Only covers the subset of values produced for this fixture.

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

json_val(x::AbstractString) = json_str(x)
json_val(x::Bool)            = x ? "true" : "false"
json_val(x::Integer)         = string(x)
json_val(x::AbstractFloat)   = isfinite(x) ? string(x) : "null"
json_val(x::Number)          = string(x)

function json_val(x::AbstractVector)
    return "[" * join(json_val.(x), ", ") * "]"
end

# ── Define the @computation ──────────────────────────────────────────────────

SUM_UMIS_CONTRACT = Contract(;
    axes = [
        "obs" => (RequiredInput, "renamed cell axis"),
        "var" => (RequiredInput, "renamed gene axis"),
    ],
    data = [
        ("obs", "var", "UMIs") => (RequiredInput, Union{UInt8, UInt16, UInt32, UInt64}, "UMI counts matrix"),
        ("obs", "total_umis") => (CreatedOutput, UInt32, "per-cell total UMI count"),
    ],
)

@computation SUM_UMIS_CONTRACT function sum_umis(adapted::DafWriter)::String
    m = get_matrix(adapted, "obs", "var", "UMIs")
    totals = UInt32.(vec(sum(Matrix(m); dims = 2)))
    set_vector!(adapted, "obs", "total_umis", totals)
    return "ok"
end

# ── Load example data and run adapter ────────────────────────────────────────

daf = example_cells_daf()

result = adapter(
    sum_umis,
    daf;
    input_axes = [
        "obs" => "@ cell",
        "var" => "@ gene",
        "cell" => nothing,
        "gene" => nothing,
    ],
    input_data = VIEW_ALL_DATA,
    output_axes = [
        "cell" => "@ obs",
        "gene" => "@ var",
        "obs" => nothing,
        "var" => nothing,
    ],
    output_data = [
        ALL_VECTORS => nothing,
        ALL_MATRICES => nothing,
        ("cell", "total_umis") => "=",
    ],
)

total_umis_vec = get_vector(daf, "cell", "total_umis")

# ── Emit JSON ────────────────────────────────────────────────────────────────

daf_jl_head = readchomp(`git -C $(pkgdir(DataAxesFormats)) rev-parse HEAD`)

out_path = joinpath(FIXTURE_DIR, "fixture.json")

open(out_path, "w") do io
    println(io, "{")
    println(io, "  ", json_str("daf_jl_head"), ": ", json_str(daf_jl_head), ",")
    println(io, "  ", json_str("result_returned"), ": ", json_str(string(result)), ",")
    println(io, "  ", json_str("total_umis_length"), ": ", json_val(length(total_umis_vec)), ",")
    println(io, "  ", json_str("total_umis_values"), ": ", json_val(collect(UInt32.(total_umis_vec))))
    println(io, "}")
end

println("Wrote ", out_path)
println("result_returned = ", result)
println("total_umis_length = ", length(total_umis_vec))
println("total_umis_values[1:5] = ", collect(UInt32.(total_umis_vec))[1:5])
