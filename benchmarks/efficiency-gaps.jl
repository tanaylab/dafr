# Julia twin of benchmarks/efficiency-gaps.R. Reads the SAME .daf.zarr fixture
# and times the same four operations with DataAxesFormats.jl, so the two CSVs
# can be joined into an R-vs-Julia comparison.
#
#   conda run -n dafr-mcview julia -t 16 benchmarks/efficiency-gaps.jl
#
# (The fixture is created by the R driver; run that first.)

using DataAxesFormats
using SparseArrays

fixture = get(ENV, "DAFR_BENCH_FIXTURE", "/tmp/daf_bench/bench.daf.zarr")
reps    = parse(Int, get(ENV, "DAFR_BENCH_REPS", "7"))
out_csv = get(ENV, "DAFR_BENCH_JULIA_OUT", "/tmp/daf_bench/julia.csv")

function med_ms(fn)
    fn()  # warmup
    ts = Float64[]
    for _ in 1:reps
        GC.gc()
        push!(ts, @elapsed fn())
    end
    sort!(ts)
    1000 * ts[cld(length(ts), 2)]
end

read_ms = med_ms() do
    daf = ZarrDaf(fixture, "r"; name = "bench")
    sum(get_matrix(daf, "cell", "gene", "expr"))
end

dw = ZarrDaf(fixture, "r"; name = "bench")
get_matrix(dw, "cell", "gene", "expr")

reduce_ms = med_ms() do
    empty_cache!(dw; clear = QueryData)
    get_query(dw, q"@ cell @ gene :: expr >| Sum")
end
eltwise_ms = med_ms() do
    empty_cache!(dw; clear = QueryData)
    get_query(dw, q"@ cell @ gene :: expr % Abs")
end
log_ms = med_ms() do
    empty_cache!(dw; clear = QueryData)
    get_query(dw, q"@ cell @ gene :: expr % Log base 2.0 eps 1.0")
end

chk(q) = sum(get_query(dw, q))
open(out_csv, "w") do io
    println(io, "op,julia_ms,check")
    println(io, "zarr_read,$(round(read_ms; digits = 2)),NA")
    println(io, "reduce_sum,$(round(reduce_ms; digits = 2)),$(round(chk(q"@ cell @ gene :: expr >| Sum"); digits = 4))")
    println(io, "eltwise_abs,$(round(eltwise_ms; digits = 2)),$(round(chk(q"@ cell @ gene :: expr % Abs"); digits = 4))")
    println(io, "log,$(round(log_ms; digits = 2)),$(round(chk(q"@ cell @ gene :: expr % Log base 2.0 eps 1.0"); digits = 4))")
end
println("fixture=$(fixture)  reps=$(reps)  Julia threads=$(Threads.nthreads())")
println(read(out_csv, String))
