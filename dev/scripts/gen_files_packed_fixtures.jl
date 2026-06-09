# Regenerate committed packed-read FilesFormat fixtures. Run:
#   conda run -n dafr-mcview julia dev/scripts/gen_files_packed_fixtures.jl <out_dir>
#
# FilesDaf packed stores are the dual-format (ZIP + Zarr-index) shards that the
# dafr packed reader in R/files_packed.R consumes. Mirrors the ZarrDaf packed
# fixtures (dev/scripts/gen_packed_fixtures.jl) but writes the FilesFormat
# on-disk layout (`<name>.zip` / `<name>.<component>.zip`).
using DataAxesFormats
using SparseArrays
import DataAxesFormats.PackedFormat as PF

out = length(ARGS) >= 1 ? ARGS[1] : "tests/testthat/fixtures/daf030-files-packed"
rm(out; force=true, recursive=true); mkpath(out)

# SMALL but above the 8 KiB pack threshold so the dense vector, the dense
# matrix, and the sparse matrix's nzval/rowval all sustain sharding. The string
# `label` vector also packs (vlen-utf8 inner codec).
ncell, ngene = 1200, 8
cells = ["c$(i)" for i in 1:ncell]; genes = ["g$(j)" for j in 1:ngene]

function populate!(d)
    add_axis!(d, "cell", cells); add_axis!(d, "gene", genes)
    set_scalar!(d, "name", "files-packed!")
    set_vector!(d, "cell", "score", Float64.(1:ncell))          # 9600 B -> sharded
    set_vector!(d, "cell", "label", ["v$(i)" for i in 1:ncell]) # strings -> sharded
    set_matrix!(d, "cell", "gene", "dense",
                reshape(Float64.(1:(ncell*ngene)), ncell, ngene)) # sharded
    # 2000 DISTINCT (cell,gene) nonzeros so nzval/rowval shard. Entry k =
    # (cell ((k-1)%ncell)+1, gene ((k-1)div ncell)+1) holds k, so sparse[1,1]=1
    # and sparse[1,2]=ncell+1. colptr (9 elems) stays flat.
    N = 2000
    I = [((k - 1) % ncell) + 1 for k in 1:N]
    J = [((k - 1) ÷ ncell) + 1 for k in 1:N]
    V = Float64.(1:N)
    set_matrix!(d, "cell", "gene", "sparse", sparse(I, J, V, ncell, ngene))
end

for (codec, label) in [(:blosc_zstd_bitshuffle,"blosc_zstd_bitshuffle"),
                       (:blosc_lz4_bitshuffle,"blosc_lz4_bitshuffle"),
                       (:zstd,"zstd"), (:gzip,"gzip")]
    PF.DAF_PACKED_COMPRESSION = codec
    d = FilesDaf(joinpath(out, "$(label).files"), "w"; name=label, packed=true)
    populate!(d)
    println("WROTE $(label)")
end
