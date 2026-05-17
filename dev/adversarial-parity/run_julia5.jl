using DataAxesFormats
using JSON3

const FIX = "/home/aviezerl/src/dafr-native/dev/adversarial-parity/fixture.daf"
const QFILE = "/home/aviezerl/src/dafr-native/dev/adversarial-parity/queries5.txt"
const OUT = "/home/aviezerl/src/dafr-native/dev/adversarial-parity/julia_out5.jsonl"

daf = FilesDaf(FIX, "r")

function serialize_result(x)
    if x === nothing
        return Dict("kind" => "nothing")
    elseif isa(x, AbstractString) || isa(x, Symbol)
        return Dict("kind" => "scalar", "type" => string(typeof(x)), "value" => string(x))
    elseif isa(x, Bool) || isa(x, Integer) || isa(x, AbstractFloat)
        v = isa(x, AbstractFloat) && (isnan(x) || isinf(x)) ? string(x) : x
        return Dict("kind" => "scalar", "type" => string(typeof(x)), "value" => v)
    elseif isa(x, AbstractVector)
        nms = try; names(x, 1); catch; nothing; end
        vals = collect(x)
        return Dict(
            "kind" => "vector",
            "type" => string(typeof(x)),
            "length" => length(vals),
            "names" => nms,
            "values" => [isa(v, AbstractFloat) && (isnan(v) || isinf(v)) ? string(v) : v for v in vals],
        )
    elseif isa(x, AbstractMatrix)
        rn = try; names(x, 1); catch; nothing; end
        cn = try; names(x, 2); catch; nothing; end
        return Dict(
            "kind" => "matrix",
            "type" => string(typeof(x)),
            "dim" => collect(size(x)),
            "rownames" => rn,
            "colnames" => cn,
            "values" => [[isa(v, AbstractFloat) && (isnan(v) || isinf(v)) ? string(v) : v for v in row] for row in eachrow(x)],
        )
    elseif isa(x, AbstractSet) || isa(x, AbstractVector{<:AbstractString})
        return Dict("kind" => "set_or_strvec", "type" => string(typeof(x)), "values" => sort(collect(string.(x))))
    else
        return Dict("kind" => "other", "type" => string(typeof(x)), "value" => string(x))
    end
end

open(OUT, "w") do io
    for (i, raw) in enumerate(eachline(QFILE))
        line = strip(raw)
        if isempty(line) || startswith(line, "#")
            continue
        end
        rec = Dict("idx" => i, "query" => line)
        try
            r = get_query(daf, parse_query(line))
            rec["status"] = "ok"
            rec["result"] = serialize_result(r)
        catch e
            rec["status"] = "error"
            rec["error"] = sprint(showerror, e)
            rec["error_type"] = string(typeof(e))
        end
        println(io, JSON3.write(rec))
    end
end

println("WROTE: ", OUT)
