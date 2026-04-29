# Capture every jldoctest in DataAxesFormats.jl example_data.jl, readers.jl,
# and writers.jl as a JSON fixture for the dafr R parity tests.
#
# Each fixture contains the per-test expected value(s) in a shape that lets the
# R-side test_that() assert without relying on Julia's text formatting (so the
# parity is on values/shapes/names, not on byte-identical printed output).
#
# Usage (from the worktree root):
#   julia --project=. tools/julia-fixtures/capture_readers_writers.jl <out_dir>
# Or rely on JULIA_PROJECT pointing at a working dafr-mcview env.

using DataAxesFormats
using JSON3
using OrderedCollections

const OUT_DIR = length(ARGS) >= 1 ? ARGS[1] :
    joinpath(@__DIR__, "..", "..", "tests", "testthat", "fixtures", "julia-readers-writers")

mkpath(OUT_DIR)

function write_fixture(name::AbstractString, payload)::Nothing
    open(joinpath(OUT_DIR, name * ".json"), "w") do io
        JSON3.pretty(io, payload)
    end
    return nothing
end

# Snapshot the structural skeleton of a description() output so the R port can
# match it without caring about Julia-only type names like "MemoryDaf" or
# "x UInt8 in Columns (Dense)".
function snapshot_description(daf::DafReader)
    scalars = OrderedDict{String, Any}()
    if has_scalar(daf, "organism")
        scalars["organism"] = get_scalar(daf, "organism")
    end
    if has_scalar(daf, "reference")
        scalars["reference"] = get_scalar(daf, "reference")
    end

    axes = OrderedDict{String, Int}()
    for axis in sort(collect(String.(axes_set(daf))))
        axes[axis] = axis_length(daf, axis)
    end

    vectors = OrderedDict{String, Vector{String}}()
    for axis in sort(collect(String.(axes_set(daf))))
        vs = sort(collect(String.(vectors_set(daf, axis))))
        if !isempty(vs)
            vectors[axis] = vs
        end
    end

    matrices = OrderedDict{String, Vector{String}}()
    sorted_axes = sort(collect(String.(axes_set(daf))))
    for r in sorted_axes, c in sorted_axes
        ms = sort(collect(String.(matrices_set(daf, r, c; relayout = false))))
        if !isempty(ms)
            matrices["$(r),$(c)"] = ms
        end
    end

    return OrderedDict(
        "scalars"  => scalars,
        "axes"     => axes,
        "vectors"  => vectors,
        "matrices" => matrices,
    )
end

# ---- example_data.jl --------------------------------------------------------

cells     = example_cells_daf()
metacells = example_metacells_daf()
chain     = example_chain_daf()

write_fixture("example_data_cells_description", OrderedDict(
    "jl_line" => 22,
    "fn"      => "example_cells_daf",
    "structure" => snapshot_description(cells),
    "raw_text"  => description(cells),
))

write_fixture("example_data_metacells_description", OrderedDict(
    "jl_line" => 62,
    "fn"      => "example_metacells_daf",
    "structure" => snapshot_description(metacells),
    "raw_text"  => description(metacells),
))

write_fixture("example_data_chain_description", OrderedDict(
    "jl_line" => 205,
    "fn"      => "example_chain_daf",
    "structure" => snapshot_description(chain),
    "raw_text"  => description(chain),
))

# ---- readers.jl -------------------------------------------------------------

# 92  has_scalar(cells, "organism") -> true
write_fixture("readers_has_scalar_cells_organism", OrderedDict(
    "jl_line" => 92, "expr" => "has_scalar(cells, \"organism\")",
    "value" => has_scalar(example_cells_daf(), "organism"),
))

# 100 has_scalar(metacells, "organism") -> false
write_fixture("readers_has_scalar_metacells_organism", OrderedDict(
    "jl_line" => 100, "expr" => "has_scalar(metacells, \"organism\")",
    "value" => has_scalar(example_metacells_daf(), "organism"),
))

# 125 scalars_set(cells) -> ["organism", "reference"]
write_fixture("readers_scalars_set_cells", OrderedDict(
    "jl_line" => 125, "expr" => "scalars_set(cells)",
    "values" => sort(collect(String.(scalars_set(example_cells_daf())))),
))

# 157 get_scalar(cells, "organism") -> "human"
write_fixture("readers_get_scalar_cells_organism", OrderedDict(
    "jl_line" => 157, "expr" => "get_scalar(cells, \"organism\")",
    "value" => get_scalar(example_cells_daf(), "organism"),
))

# 165 get_scalar(metacells, "organism"; default = nothing) -> nothing
write_fixture("readers_get_scalar_metacells_default_nothing", OrderedDict(
    "jl_line" => 165,
    "expr"    => "get_scalar(metacells, \"organism\"; default = nothing)",
    "value"   => get_scalar(example_metacells_daf(), "organism"; default = nothing),
))

# 210 has_axis(cells, "metacell") -> false
write_fixture("readers_has_axis_cells_metacell", OrderedDict(
    "jl_line" => 210, "expr" => "has_axis(cells, \"metacell\")",
    "value" => has_axis(example_cells_daf(), "metacell"),
))

# 218 has_axis(metacells, "metacell") -> true
write_fixture("readers_has_axis_metacells_metacell", OrderedDict(
    "jl_line" => 218, "expr" => "has_axis(metacells, \"metacell\")",
    "value" => has_axis(example_metacells_daf(), "metacell"),
))

# 246 axis_version_counter sequence
let m = example_metacells_daf()
    before = Int(axis_version_counter(m, "type"))
    delete_axis!(m, "type")
    add_axis!(m, "type", ["Foo", "Bar", "Baz"])
    after = Int(axis_version_counter(m, "type"))
    write_fixture("readers_axis_version_counter_type", OrderedDict(
        "jl_line" => 246, "expr" => "axis_version_counter delete+add",
        "before" => before, "after" => after,
    ))
end

# 273 sort!(String.(axes_set(cells)))
write_fixture("readers_axes_set_cells", OrderedDict(
    "jl_line" => 273, "expr" => "axes_set(cells)",
    "values" => sort(collect(String.(axes_set(example_cells_daf())))),
))

# 308 String.(axis_vector(metacells, "type"))
write_fixture("readers_axis_vector_metacells_type", OrderedDict(
    "jl_line" => 308, "expr" => "axis_vector(metacells, \"type\")",
    "values" => collect(String.(axis_vector(example_metacells_daf(), "type"))),
))

# 353 axis_dict(metacells, "type")
let d = axis_dict(example_metacells_daf(), "type")
    keys_v = collect(String.(keys(d)))
    vals_v = [Int(d[k]) for k in keys_v]
    write_fixture("readers_axis_dict_metacells_type", OrderedDict(
        "jl_line" => 353, "expr" => "axis_dict(metacells, \"type\")",
        "keys" => keys_v, "values" => vals_v,
    ))
end

# 389 axis_indices(metacells, "type", ["MPP",""]; allow_empty=true) -> [4, 0]
write_fixture("readers_axis_indices_allow_empty", OrderedDict(
    "jl_line" => 389,
    "expr"    => "axis_indices(metacells, \"type\", [\"MPP\",\"\"]; allow_empty=true)",
    "values"  => Int.(axis_indices(example_metacells_daf(), "type",
                                   ["MPP", ""]; allow_empty = true)),
))

# 448 axis_entries(metacells, "type", [3, 0]; allow_empty=true) -> ["MEBEMP-L", ""]
write_fixture("readers_axis_entries_allow_empty", OrderedDict(
    "jl_line" => 448,
    "expr"    => "axis_entries(metacells, \"type\", [3,0]; allow_empty=true)",
    "values"  => collect(String.(axis_entries(example_metacells_daf(), "type",
                                              [3, 0]; allow_empty = true))),
))

# 487 axis_length(metacells, "type") -> 4
write_fixture("readers_axis_length_metacells_type", OrderedDict(
    "jl_line" => 487, "expr" => "axis_length(metacells, \"type\")",
    "value" => Int(axis_length(example_metacells_daf(), "type")),
))

# 525 has_vector(cells, "cell", "type") -> false
write_fixture("readers_has_vector_cells_cell_type", OrderedDict(
    "jl_line" => 525, "expr" => "has_vector(cells, \"cell\", \"type\")",
    "value" => has_vector(example_cells_daf(), "cell", "type"),
))

# 533 has_vector(metacells, "metacell", "type") -> true
write_fixture("readers_has_vector_metacells_metacell_type", OrderedDict(
    "jl_line" => 533, "expr" => "has_vector(metacells, \"metacell\", \"type\")",
    "value" => has_vector(example_metacells_daf(), "metacell", "type"),
))

# 566 vector_version_counter sequence (1 -> 2 after overwrite)
let m = example_metacells_daf()
    before = Int(vector_version_counter(m, "type", "color"))
    set_vector!(m, "type", "color", string.(collect(1:4)); overwrite = true)
    after = Int(vector_version_counter(m, "type", "color"))
    write_fixture("readers_vector_version_counter_color", OrderedDict(
        "jl_line" => 566, "expr" => "vector_version_counter overwrite",
        "before" => before, "after" => after,
    ))
end

# 595 sort!(String.(vectors_set(cells, "cell")))
write_fixture("readers_vectors_set_cells_cell", OrderedDict(
    "jl_line" => 595, "expr" => "vectors_set(cells, \"cell\")",
    "values" => sort(collect(String.(vectors_set(example_cells_daf(), "cell")))),
))

# 633 get_vector(metacells, "type", "color")
let v = get_vector(example_metacells_daf(), "type", "color")
    nm = collect(String.(names(v, 1)))
    vals = collect(String.(parent(v)))
    write_fixture("readers_get_vector_metacells_type_color", OrderedDict(
        "jl_line" => 633, "expr" => "get_vector(metacells, \"type\", \"color\")",
        "names" => nm, "values" => vals,
    ))
end

# 748 has_matrix(cells, "gene", "cell", "UMIs") -> true
write_fixture("readers_has_matrix_cells_gene_cell_UMIs", OrderedDict(
    "jl_line" => 748, "expr" => "has_matrix(cells, \"gene\", \"cell\", \"UMIs\")",
    "value" => has_matrix(example_cells_daf(), "gene", "cell", "UMIs"),
))

# 801 matrices_set(cells, "gene", "cell") -> ["UMIs"]
write_fixture("readers_matrices_set_cells_gene_cell", OrderedDict(
    "jl_line" => 801, "expr" => "matrices_set(cells, \"gene\", \"cell\")",
    "values" => sort(collect(String.(matrices_set(example_cells_daf(),
                                                  "gene", "cell")))),
))

# 933 get_matrix(metacells, "gene", "metacell", "fraction") -> 683x7
let m = get_matrix(example_metacells_daf(), "gene", "metacell", "fraction")
    rn = collect(String.(names(m, 1)))
    cn = collect(String.(names(m, 2)))
    base = parent(m)
    write_fixture("readers_get_matrix_metacells_gene_metacell_fraction", OrderedDict(
        "jl_line"   => 933,
        "expr"      => "get_matrix(metacells, \"gene\", \"metacell\", \"fraction\")",
        "rows"      => size(base, 1),
        "cols"      => size(base, 2),
        "row_names" => rn,
        "col_names" => cn,
        "head"      => Float64.(base[1:min(8, size(base, 1)), 1:min(7, size(base, 2))]),
        "checksum"  => Float64(sum(base)),
    ))
end

# 1088 matrix_version_counter sequence (1 -> 2 after overwrite)
let m = example_metacells_daf()
    before = Int(matrix_version_counter(m, "gene", "metacell", "fraction"))
    set_matrix!(m, "gene", "metacell", "fraction", rand(Float32, 683, 7);
                overwrite = true)
    after = Int(matrix_version_counter(m, "gene", "metacell", "fraction"))
    write_fixture("readers_matrix_version_counter_fraction", OrderedDict(
        "jl_line" => 1088, "expr" => "matrix_version_counter overwrite",
        "before" => before, "after" => after,
    ))
end

# 1170 description(example_chain_daf(); deep = true)
write_fixture("readers_description_chain_deep", OrderedDict(
    "jl_line" => 1170, "expr" => "description(example_chain_daf(); deep=true)",
    "structure" => snapshot_description(example_chain_daf()),
    "raw_text"  => description(example_chain_daf(); deep = true),
))

# ---- writers.jl -------------------------------------------------------------

# 63  set_scalar!(cells, "version", 1.0); ... set_scalar!(...; overwrite=true, 2.0)
let cells = example_cells_daf()
    set_scalar!(cells, "version", 1.0)
    v1 = get_scalar(cells, "version")
    set_scalar!(cells, "version", 2.0; overwrite = true)
    v2 = get_scalar(cells, "version")
    write_fixture("writers_set_scalar_overwrite", OrderedDict(
        "jl_line" => 63, "expr" => "set_scalar! overwrite=true",
        "first" => v1, "second" => v2,
    ))
end

# 109 delete_scalar!(cells, "organism")
let cells = example_cells_daf()
    before = has_scalar(cells, "organism")
    delete_scalar!(cells, "organism")
    after = has_scalar(cells, "organism")
    write_fixture("writers_delete_scalar_organism", OrderedDict(
        "jl_line" => 109, "expr" => "delete_scalar!(cells, \"organism\")",
        "before" => before, "after" => after,
    ))
end

# 161 add_axis!(cells, "block", ["B1","B2"])
let cells = example_cells_daf()
    before = has_axis(cells, "block")
    add_axis!(cells, "block", ["B1", "B2"])
    after = has_axis(cells, "block")
    write_fixture("writers_add_axis_block", OrderedDict(
        "jl_line" => 161, "expr" => "add_axis!(cells, \"block\", [B1,B2])",
        "before" => before, "after" => after,
    ))
end

# 217 delete_axis!(metacells, "type")
let m = example_metacells_daf()
    before = has_axis(m, "type")
    delete_axis!(m, "type")
    after = has_axis(m, "type")
    write_fixture("writers_delete_axis_type", OrderedDict(
        "jl_line" => 217, "expr" => "delete_axis!(metacells, \"type\")",
        "before" => before, "after" => after,
    ))
end

# 300 set_vector!(metacells, "type", "is_mebemp", [...])
let m = example_metacells_daf()
    s0 = has_vector(m, "type", "is_mebemp")
    set_vector!(m, "type", "is_mebemp", [true, true, false, false])
    s1 = has_vector(m, "type", "is_mebemp")
    set_vector!(m, "type", "is_mebemp", [true, true, true, false]; overwrite = true)
    s2 = has_vector(m, "type", "is_mebemp")
    final = collect(parent(get_vector(m, "type", "is_mebemp")))
    write_fixture("writers_set_vector_is_mebemp", OrderedDict(
        "jl_line" => 300, "expr" => "set_vector! is_mebemp",
        "stages"  => [s0, s1, s2],
        "final"   => final,
    ))
end

# 618 delete_vector!(metacells, "type", "color")
let m = example_metacells_daf()
    before = has_vector(m, "type", "color")
    delete_vector!(m, "type", "color")
    after = has_vector(m, "type", "color")
    write_fixture("writers_delete_vector_color", OrderedDict(
        "jl_line" => 618, "expr" => "delete_vector!(metacells, \"type\", \"color\")",
        "before" => before, "after" => after,
    ))
end

# 686 set_matrix! with relayout=false then overwrite=true
let m = example_metacells_daf()
    s0 = (has_matrix(m, "gene", "metacell", "confidence"),
          has_matrix(m, "gene", "metacell", "confidence"; relayout = false),
          has_matrix(m, "metacell", "gene", "confidence"; relayout = false))
    set_matrix!(m, "metacell", "gene", "confidence", rand(7, 683); relayout = false)
    s1 = (has_matrix(m, "gene", "metacell", "confidence"),
          has_matrix(m, "gene", "metacell", "confidence"; relayout = false),
          has_matrix(m, "metacell", "gene", "confidence"; relayout = false))
    set_matrix!(m, "metacell", "gene", "confidence", rand(7, 683); overwrite = true)
    s2 = (has_matrix(m, "gene", "metacell", "confidence"),
          has_matrix(m, "gene", "metacell", "confidence"; relayout = false),
          has_matrix(m, "metacell", "gene", "confidence"; relayout = false))
    write_fixture("writers_set_matrix_confidence", OrderedDict(
        "jl_line" => 686, "expr" => "set_matrix! confidence (relayout, overwrite)",
        "stage0"  => collect(s0), "stage1" => collect(s1), "stage2" => collect(s2),
    ))
end

# 1147 delete_matrix! with must_exist=false then relayout=false
let cells = example_cells_daf()
    s0 = (has_matrix(cells, "gene", "cell", "UMIs"),
          has_matrix(cells, "gene", "cell", "UMIs"; relayout = false),
          has_matrix(cells, "cell", "gene", "UMIs"; relayout = false))
    delete_matrix!(cells, "gene", "cell", "UMIs"; relayout = false)
    s1 = (has_matrix(cells, "gene", "cell", "UMIs"),
          has_matrix(cells, "gene", "cell", "UMIs"; relayout = false),
          has_matrix(cells, "cell", "gene", "UMIs"; relayout = false))
    delete_matrix!(cells, "gene", "cell", "UMIs"; must_exist = false)
    s2 = (has_matrix(cells, "gene", "cell", "UMIs"),
          has_matrix(cells, "gene", "cell", "UMIs"; relayout = false),
          has_matrix(cells, "cell", "gene", "UMIs"; relayout = false))
    write_fixture("writers_delete_matrix_UMIs", OrderedDict(
        "jl_line" => 1147, "expr" => "delete_matrix! UMIs (relayout, must_exist)",
        "stage0"  => collect(s0), "stage1" => collect(s1), "stage2" => collect(s2),
    ))
end

println("wrote $(length(readdir(OUT_DIR))) fixtures to $(OUT_DIR)")
