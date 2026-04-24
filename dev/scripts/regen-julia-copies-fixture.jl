# Regenerate fixtures/julia-copies/*.json from DataAxesFormats.jl.
# Pre: git -C ~/src/DataAxesFormats.jl pull --ff-only && re-dev in
# conda env dafr-mcview.
# Run: conda run -n dafr-mcview julia --project=~/src/dafr-mcview \
#     dev/scripts/regen-julia-copies-fixture.jl
#
# Outputs are byte-comparable round-trips for the R port to verify.

using DataAxesFormats

const OUT_DIR = joinpath(@__DIR__, "..", "..", "tests", "testthat", "fixtures", "julia-copies")
mkpath(OUT_DIR)

# --- Tiny JSON emitter ---
json_str(s::AbstractString) = "\"" * replace(s, "\\" => "\\\\", "\"" => "\\\"") * "\""
json_val(x::Bool) = x ? "true" : "false"
json_val(x::Integer) = string(x)
json_val(x::AbstractFloat) = string(x)
json_val(x::AbstractString) = json_str(x)
json_val(xs::AbstractVector) = "[" * join(json_val.(xs), ",") * "]"
json_val(xs::Tuple) = "[" * join(json_val.(collect(xs)), ",") * "]"
function json_val(d::AbstractDict)
    parts = ["$(json_str(string(k))):$(json_val(v))" for (k, v) in d]
    "{" * join(parts, ",") * "}"
end

# --- copy_all fixture ---
function make_copy_all_fixture()
    src = MemoryDaf(name = "src")
    set_scalar!(src, "organism", "human")
    add_axis!(src, "cell", ["c1", "c2"])
    add_axis!(src, "gene", ["g1", "g2", "g3"])
    set_vector!(src, "cell", "age", Int32[10, 20])
    set_matrix!(src, "cell", "gene", "UMIs", Int32[1 2 3; 4 5 6])

    dest = MemoryDaf(name = "dest")
    add_axis!(dest, "cell", ["c1", "c2", "c3"])
    add_axis!(dest, "gene", ["g1", "g2", "g3"])
    copy_all!(
        destination = dest, source = src,
        empty = Dict(("cell", "age") => Int32(-1),
                     ("cell", "gene", "UMIs") => Int32(0)),
        relayout = false,
    )

    Dict(
        "scalars" => Dict(n => get_scalar(dest, n) for n in scalars_set(dest)),
        "axes" => Dict(ax => axis_vector(dest, ax) for ax in axes_set(dest)),
        "vectors" => Dict("cell|age" => collect(get_vector(dest, "cell", "age"))),
        "matrices" => Dict("cell|gene|UMIs" => [collect(r) for r in eachrow(get_matrix(dest, "cell", "gene", "UMIs"))]),
    )
end

# --- concatenate fixture ---
function make_concat_fixture()
    a = MemoryDaf(name = "A")
    add_axis!(a, "cell", ["c1", "c2"])
    set_vector!(a, "cell", "age", Int32[10, 20])
    b = MemoryDaf(name = "B")
    add_axis!(b, "cell", ["c1", "c3"])
    set_vector!(b, "cell", "age", Int32[30, 40])

    dest = MemoryDaf(name = "dest")
    concatenate!(dest, "cell", [a, b], prefix = true)

    Dict(
        "axes" => Dict(ax => axis_vector(dest, ax) for ax in axes_set(dest)),
        "vectors" => Dict(
            "cell|age" => collect(get_vector(dest, "cell", "age")),
            "cell|dataset" => collect(get_vector(dest, "cell", "dataset")),
        ),
    )
end

open(joinpath(OUT_DIR, "copy_all_fixture.json"), "w") do io
    write(io, json_val(make_copy_all_fixture()))
end
open(joinpath(OUT_DIR, "concat_fixture.json"), "w") do io
    write(io, json_val(make_concat_fixture()))
end

# Record HEAD for reproducibility.
daf_head = read(`git -C $(normpath(joinpath(pathof(DataAxesFormats), "..", ".."))) rev-parse HEAD`, String) |> strip
open(joinpath(OUT_DIR, "README.md"), "w") do io
    write(io, """
# Julia copies/concat fixture

Generated from DataAxesFormats.jl at `$daf_head`.

Regenerate:

```
conda run -n dafr-mcview julia --project=~/src/dafr-mcview \\
    dev/scripts/regen-julia-copies-fixture.jl
```

Payloads:

- `copy_all_fixture.json` — `copy_all!` roundtrip with cell-axis
  superset and empty fills for `cell|age` and `cell|gene|UMIs`.
- `concat_fixture.json` — `concatenate!` of two sources on the
  `cell` axis with `prefix = true`.
""")
end

println("Regen complete: wrote ", length(readdir(OUT_DIR)), " files to ", OUT_DIR)
