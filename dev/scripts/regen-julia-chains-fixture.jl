# Regenerate the Julia chain-reader fixture for dafr Slice 4 tests.
#
# Run via:
#   conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
#     dev/scripts/regen-julia-chains-fixture.jl
#
# Produces:
#   tests/testthat/fixtures/julia-chains/fixture.json
#
# JSON is not used (unavailable without Manifest.toml); a minimal serialiser is inlined.
# Mirrors dev/scripts/regen-julia-queries-fixture.jl's pattern.

using DataAxesFormats

const DAFR_ROOT = dirname(dirname(@__DIR__))  # dev/scripts/ -> dev -> package root
const FIXTURE_DIR = joinpath(DAFR_ROOT, "tests", "testthat", "fixtures", "julia-chains")

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

# ── Build the chain ──────────────────────────────────────────────────────────

d1 = MemoryDaf(; name = "first")
set_scalar!(d1, "version", Int64(1))
add_axis!(d1, "cell", ["A", "B", "C"])
set_vector!(d1, "cell", "age", Int64[10, 20, 30])

d2 = MemoryDaf(; name = "second")
set_scalar!(d2, "version", Int64(2))
set_scalar!(d2, "owner", "me")
add_axis!(d2, "cell", ["A", "B", "C"])
set_vector!(d2, "cell", "age", Int64[100, 200, 300])
set_vector!(d2, "cell", "donor", ["d1", "d2", "d1"])

read_chain = chain_reader([d1, d2]; name = "chain")

# ── Read chain values ────────────────────────────────────────────────────────

version_value = get_scalar(read_chain, "version")
owner_value = get_scalar(read_chain, "owner")
cell_axis = axis_vector(read_chain, "cell")
age_vec = get_vector(read_chain, "cell", "age").array
donor_vec = get_vector(read_chain, "cell", "donor").array

# ── Emit JSON ────────────────────────────────────────────────────────────────

out_path = joinpath(FIXTURE_DIR, "fixture.json")

open(out_path, "w") do io
    println(io, "{")
    println(io, "  ", json_str("chain_name"), ": ", json_str("chain"), ",")
    println(io, "  ", json_str("daf_names"), ": [", json_str("first"), ", ", json_str("second"), "],")
    println(io, "  ", json_str("scalars"), ": {")
    println(io, "    ", json_str("version"), ": {",
        json_str("value"), ": ", json_val(version_value), ", ",
        json_str("type"), ": ", json_str("integer"), "},")
    println(io, "    ", json_str("owner"), ": {",
        json_str("value"), ": ", json_val(owner_value), ", ",
        json_str("type"), ": ", json_str("character"), "}")
    println(io, "  },")
    println(io, "  ", json_str("axes"), ": {")
    println(io, "    ", json_str("cell"), ": ", json_val(collect(cell_axis)))
    println(io, "  },")
    println(io, "  ", json_str("vectors"), ": {")
    println(io, "    ", json_str("cell"), ": {")
    println(io, "      ", json_str("age"), ": ", json_val(collect(age_vec)), ",")
    println(io, "      ", json_str("donor"), ": ", json_val(collect(donor_vec)))
    println(io, "    }")
    println(io, "  }")
    println(io, "}")
end

println("Wrote ", out_path)
