#!/usr/bin/env julia
# Julia-side bake-off runner.
#
# Usage:
#   julia --project=benchmarks/julia benchmarks/julia/run_bakeoff.jl --out OUT.csv
#       [--only id1,id2,...] [--fixture NAME] [--par]

using DataAxesFormats
using BenchmarkTools
using YAML
using CSV
using DataFrames
using SHA
using LinearAlgebra
using ArgParse
using Printf

function parse_args_()
    s = ArgParseSettings()
    @add_arg_table! s begin
        "--out";     arg_type = String; default = "/tmp/julia-times.csv"
        "--only";    arg_type = String; default = ""
        "--fixture"; arg_type = String; default = ""
        "--par";     action = :store_true
    end
    return parse_args(s)
end

const ARGS_ = parse_args_()

if !ARGS_["par"]
    BLAS.set_num_threads(1)
    # JULIA_NUM_THREADS is set at launch, not runtime. Just record what we got.
end

const FIXTURE_ROOT = joinpath(@__DIR__, "..", "fixture", "data")

function sha256_dir(path::AbstractString)
    # Mirror R's digest-based approach exactly:
    #   files <- sort(list.files(path, recursive=TRUE, full.names=TRUE))
    #   hashes <- vapply(files, function(f) digest::digest(file=f, algo="sha256"), character(1))
    #   digest::digest(paste(basename(files), hashes, collapse="\n"), algo="sha256")
    # paste(v1, v2, collapse="\n") produces "b1 h1\nb2 h2\n..." (space-sep pairs, \n between, no trailing \n).
    files = String[]
    for (root, _, fs) in walkdir(path)
        for f in fs
            push!(files, joinpath(root, f))
        end
    end
    sort!(files)
    pairs = String[]
    for f in files
        fhash = open(f) do io
            bytes2hex(SHA.sha256(io))
        end
        push!(pairs, "$(basename(f)) $fhash")
    end
    combined = join(pairs, "\n")
    return bytes2hex(SHA.sha256(Vector{UInt8}(combined)))[1:16]
end

function open_fixture(name::AbstractString)
    path = joinpath(FIXTURE_ROOT, name)
    if name == "chain_triple" || name == "view_renamed"
        return complete_daf(joinpath(path, "leaf"), "r"; name = name)
    else
        return FilesDaf(path, "r"; name = name)
    end
end

function main()
    queries_all = YAML.load_file(joinpath(@__DIR__, "..", "queries.yaml"))
    queries = queries_all
    if !isempty(ARGS_["only"])
        ids = Set(split(ARGS_["only"], ","))
        queries = filter(q -> q["id"] in ids, queries)
    end
    if !isempty(ARGS_["fixture"])
        queries = filter(q -> q["fixture"] == ARGS_["fixture"], queries)
    end
    println("running $(length(queries)) queries")

    fixtures_needed = sort(unique(q["fixture"] for q in queries))
    opened = Dict{String,Any}()
    for name in fixtures_needed
        opened[name] = open_fixture(name)
        println("opened fixture $name")
    end
    checksums = Dict(name => sha256_dir(joinpath(FIXTURE_ROOT, name))
                     for name in fixtures_needed)

    rows = DataFrame(query_id = String[], query_text = String[],
                     category = String[], fixture = String[],
                     median_time_ns = Float64[], min_time_ns = Float64[],
                     gc_count = Int[], allocations = Float64[],
                     n_iter = Int[])

    for (k, q) in enumerate(queries)
        text      = q["text"]
        name      = q["fixture"]
        reopen    = get(q, "reopen", false)

        bench = try
            if reopen
                @benchmark get_query(open_fixture($name), $text) samples=50 seconds=5 evals=1
            else
                daf = opened[name]
                @benchmark get_query($daf, $text) samples=50 seconds=5 evals=1
            end
        catch e
            @printf "  [%3d/%3d] %-30s %-22s FAIL: %s\n" k length(queries) q["id"] q["category"] sprint(showerror, e)
            push!(rows, (q["id"], text, q["category"], name,
                         NaN, NaN, 0, NaN, 0))
            continue
        end

        push!(rows, (q["id"], text, q["category"], name,
                     Float64(median(bench).time),
                     Float64(minimum(bench).time),
                     Int(length(filter(x -> x > 0, bench.gctimes))),
                     Float64(bench.allocs),
                     length(bench.times)))

        @printf "  [%3d/%3d] %-30s %-22s ok\n" k length(queries) q["id"] q["category"]
    end

    commit = try
        strip(read(`git rev-parse HEAD`, String))
    catch
        "unknown"
    end

    # Sorted fixtures line for join-match with R runner
    fixture_line = join(["$k=$(checksums[k])" for k in sort(collect(keys(checksums)))], "; ")

    blas_info = try
        string(BLAS.get_config())
    catch
        "unknown"
    end

    header = [
        "# runner: Julia",
        "# dafr_commit: $commit",
        "# julia_version: $(VERSION)",
        "# platform: $(Sys.MACHINE)",
        "# JULIA_NUM_THREADS: $(get(ENV, "JULIA_NUM_THREADS", "default"))",
        "# BLAS: $blas_info",
        "# fixtures: $fixture_line",
    ]

    mkpath(dirname(ARGS_["out"]))
    open(ARGS_["out"], "w") do io
        for line in header
            println(io, line)
        end
        CSV.write(io, rows, append = true, writeheader = true)
    end
    println("\nwrote $(ARGS_["out"]) ($(nrow(rows)) rows)")
end

main()
