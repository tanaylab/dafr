# Regenerate committed packed-read fixtures. Run:
#   conda run -n dafr-mcview julia dev/scripts/gen_packed_fixtures.jl <out_dir>
using DataAxesFormats
using SparseArrays
import DataAxesFormats.PackedFormat as PF

out = length(ARGS) >= 1 ? ARGS[1] : "tests/testthat/fixtures/daf030-packed"
rm(out; force=true, recursive=true); mkpath(out)

# Keep fixtures SMALL but above the 8 KiB pack threshold so dense vector,
# dense matrix, and the sparse matrix's nzval/rowval all sustain sharding.
ncell, ngene = 1200, 8
cells = ["c$(i)" for i in 1:ncell]; genes = ["g$(j)" for j in 1:ngene]

function populate!(d)
    add_axis!(d, "cell", cells); add_axis!(d, "gene", genes)
    set_scalar!(d, "name", "packed!")
    set_vector!(d, "cell", "score", Float64.(1:ncell))          # 9600 B -> sharded
    set_vector!(d, "cell", "label", ["v$(i)" for i in 1:ncell]) # strings -> flat
    set_matrix!(d, "cell", "gene", "dense",
                reshape(Float64.(1:(ncell*ngene)), ncell, ngene)) # sharded
    # Dense-enough sparse: 2000 DISTINCT (cell,gene) nonzeros so nzval (16 KB)
    # shards. Entry k = (cell ((k-1)%ncell)+1, gene ((k-1)div ncell)+1) holds k,
    # so sparse[1,1]=1 and sparse[1,2]=ncell+1.
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
    d = ZarrDaf(joinpath(out, "$(label).daf.zarr"), "w"; name=label, packed=true)
    populate!(d)
    println("WROTE $(label)")
end
