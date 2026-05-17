using DataAxesFormats

const FIX = "/home/aviezerl/src/dafr-native/dev/adversarial-parity/fixture.daf"
isdir(FIX) && rm(FIX; recursive = true)

daf = FilesDaf(FIX, "w"; name = "adv")

set_scalar!(daf, "version", 1.0)
set_scalar!(daf, "title", "adversarial")
set_scalar!(daf, "intver", Int32(7))
set_scalar!(daf, "flag", true)

add_axis!(daf, "cell", ["A", "B", "C", "D", "E"])
add_axis!(daf, "gene", ["g1", "g2", "g3"])
add_axis!(daf, "batch", ["b1", "b2", "b3"])
add_axis!(daf, "type", ["U", "V", "W"])
add_axis!(daf, "empty_axis", String[])

# cell vectors
set_vector!(daf, "cell", "age",   [10, 20, 30, 40, 50])
set_vector!(daf, "cell", "score", [0.5, 1.5, 2.5, -1.0, 3.5])
set_vector!(daf, "cell", "is_doublet", [true, false, true, false, true])
set_vector!(daf, "cell", "is_low", [true, true, false, false, false])
set_vector!(daf, "cell", "type",  ["U", "V", "U", "W", "V"])
set_vector!(daf, "cell", "batch", ["b1", "b2", "b1", "b3", "b2"])
set_vector!(daf, "cell", "label", ["", "x", "y\\z", "a b", "Z"])
set_vector!(daf, "cell", "neg_age", [-5, -1, 0, 1, 5])
set_vector!(daf, "cell", "all_zero", [0.0, 0.0, 0.0, 0.0, 0.0])
set_vector!(daf, "cell", "with_nan", [1.0, NaN, 3.0, NaN, 5.0])
set_vector!(daf, "cell", "all_nan",  [NaN, NaN, NaN, NaN, NaN])
set_vector!(daf, "cell", "infs",     [Inf, -Inf, NaN, 0.0, 1.0])
set_vector!(daf, "cell", "all_neg",  [-1.5, -2.5, -3.5, -4.5, -5.5])
set_vector!(daf, "cell", "f32_score", Float32[0.5, 1.5, 2.5, -1.0, 3.5])
set_vector!(daf, "cell", "i8_age",    Int8[1, 2, 3, 4, 5])
set_vector!(daf, "cell", "i16_age",   Int16[100, 200, 300, 400, 500])
set_vector!(daf, "cell", "u32_count", UInt32[10, 20, 30, 40, 50])
# tie vector: every value occurs once except 0 occurs twice -> Mode tie (smallest? first?)
set_vector!(daf, "cell", "ties_int", Int32[3, 0, 1, 0, 2])
# subtype only uses U and V from the type axis - W has no members for =@ broadcast empty-group probe
set_vector!(daf, "cell", "subtype",  ["U", "V", "U", "V", "V"])

# gene vectors
set_vector!(daf, "gene", "is_lateral", [true, false, false])
set_vector!(daf, "gene", "marker", ["lo", "hi", "lo"])

# batch vectors
set_vector!(daf, "batch", "donor", ["dA", "dB", "dC"])

# type vectors
set_vector!(daf, "type", "color", ["red", "green", "blue"])

# sparse-friendly matrix (mostly zeros) - Int32 to compare sparse-path divergence if any
set_matrix!(daf, "cell", "gene", "sparse_umis",
    Int32[0 0 3;
          0 0 0;
          7 0 0;
          0 0 0;
          0 4 0])

# cell x gene matrix
set_matrix!(daf, "cell", "gene", "UMIs",
    Int32[1 2 3;
          4 5 6;
          7 8 9;
          0 1 2;
          3 4 5])
set_matrix!(daf, "cell", "gene", "frac",
    Float32[0.1 0.2 0.3;
            0.4 0.5 0.6;
            0.7 0.8 0.9;
            0.0 0.1 0.2;
            0.3 0.4 0.5])

# square: cell x cell distance
set_matrix!(daf, "cell", "cell", "distance",
    Int32[0 1 2 3 4;
          1 0 1 2 3;
          2 1 0 1 2;
          3 2 1 0 1;
          4 3 2 1 0])

println("WROTE: ", FIX)
