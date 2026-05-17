# dev/backend-parity/build_fixture_R.R
#
# Builds the reference fixture for the cross-backend round-trip audit.
# Source-of-truth is R. Dtypes that R cannot produce natively (Float32,
# Int8/16, UInt8/16/32/64) are out of scope here - they require a
# Julia-seeded fixture (see NEXT_AUDIT.md "Out of scope" notes).
#
# Two entry points:
#   - build_fixture(daf): write the whole fixture into a passed DafWriter.
#   - fixture_manifest():  return a list of items that round-trip
#                          runners iterate over (one record per read).
#
# The two are kept in lockstep by construction: build_fixture iterates
# the same manifest, so adding an item in one place automatically
# makes it visible to the runners.

suppressMessages(library(bit64))
suppressMessages(library(Matrix))

# ---------------------------------------------------------------------
# Manifest
# ---------------------------------------------------------------------

# Each entry is one read target. Keys uniquely identify the item so
# runners can emit "key" in JSONL and the diff tool can join across
# backends.
#
# Fields:
#   key        : unique id (used in JSONL)
#   kind       : "scalar" | "vector" | "matrix" | "axis"
#   axis       : axis name (vector / axis-entries / matrix.row)
#   cols_axis  : columns axis (matrix only)
#   name       : prop name
#   dtype      : expected on-disk eltype (Bool/Int32/Int64/Float64/String)
#   shape      : integer vector (e.g. c(5) or c(5, 3) or c() for scalar)
#   storage    : "dense" | "sparse" (matrices)
#   notes      : free text (what edge case this exercises)

fixture_manifest <- function() {
    M <- list()
    add <- function(rec) M[[length(M) + 1L]] <<- rec
    sc <- function(name, dtype, notes = "")
        add(list(key = paste("scalar", name, sep = "|"),
                 kind = "scalar", name = name,
                 dtype = dtype, shape = integer(0), notes = notes))
    ax <- function(axis, len, notes = "")
        add(list(key = paste("axis", axis, sep = "|"),
                 kind = "axis", axis = axis, name = axis,
                 dtype = "String", shape = len, notes = notes))
    ve <- function(axis, name, dtype, len, notes = "")
        add(list(key = paste("vector", axis, name, sep = "|"),
                 kind = "vector", axis = axis, name = name,
                 dtype = dtype, shape = len, notes = notes))
    mx <- function(ra, ca, name, dtype, nr, nc,
                   storage = "dense", notes = "")
        add(list(key = paste("matrix", ra, ca, name, sep = "|"),
                 kind = "matrix", axis = ra, cols_axis = ca,
                 name = name, dtype = dtype,
                 shape = c(nr, nc), storage = storage, notes = notes))

    # ---- Scalars ----
    sc("flag_true",   "Bool",    "true bool")
    sc("flag_false",  "Bool",    "false bool")
    sc("intver",      "Int32",   "small positive int")
    sc("int_neg",     "Int32",   "negative int")
    sc("int_zero",    "Int32",   "zero int")
    sc("int_max",     "Int32",   ".Machine$integer.max")
    sc("int64_big",   "Int64",   "2^40 via bit64")
    sc("int64_neg",   "Int64",   "negative int64")
    sc("int64_zero",  "Int64",   "zero int64")
    sc("version",     "Float64", "double 1.0")
    sc("pi_val",      "Float64", "high-precision double")
    sc("dbl_neg",     "Float64", "negative double")
    sc("dbl_zero",    "Float64", "0.0")
    sc("dbl_nan",     "Float64", "NaN - edge (was bug A, fixed)")
    sc("dbl_inf",     "Float64", "+Inf - edge")
    sc("dbl_neg_inf", "Float64", "-Inf - edge")
    sc("dbl_tiny",    "Float64", "near-underflow double")
    sc("dbl_huge",    "Float64", "near-overflow double")
    sc("title",       "String",  "plain ASCII")
    sc("empty_str",   "String",  "empty string")
    sc("unicode_str", "String",  "non-ASCII")
    sc("special_str", "String",  "quote / backslash / newline")

    # ---- Axes ----
    ax("cell",         5L,  "small named axis")
    ax("gene",         3L,  "small named axis")
    ax("batch",        3L,  "small named axis")
    ax("type",         3L,  "small named axis")
    ax("single_axis",  1L,  "single entry axis")
    ax("empty_axis",   0L,  "zero-entry axis")
    ax("big_axis",     10L, "10-entry axis")
    ax("unicode_axis", 3L,  "axis with non-ASCII entries")

    # ---- Cell vectors (n=5) ----
    ve("cell", "age",            "Int32",   5L, "small int")
    ve("cell", "neg_age",        "Int32",   5L, "negatives + zero")
    ve("cell", "ties",           "Int32",   5L, "tie pattern for Mode")
    ve("cell", "int_extremes",   "Int32",   5L, "Int32 min/max")
    ve("cell", "bignum",         "Int64",   5L, "bit64 small")
    ve("cell", "int64_extremes", "Int64",   5L, "Int64 wide range")
    ve("cell", "score",          "Float64", 5L, "small doubles")
    ve("cell", "all_zero",       "Float64", 5L, "all zero")
    ve("cell", "with_nan",       "Float64", 5L, "some NaN")
    ve("cell", "all_nan",        "Float64", 5L, "all NaN")
    ve("cell", "infs",           "Float64", 5L, "Inf / -Inf / NaN mix")
    ve("cell", "all_neg",        "Float64", 5L, "all negative")
    ve("cell", "dbl_extremes",   "Float64", 5L, "near underflow/overflow")
    ve("cell", "is_doublet",     "Bool",    5L, "mixed bool")
    ve("cell", "all_true",       "Bool",    5L, "all TRUE")
    ve("cell", "all_false",      "Bool",    5L, "all FALSE")
    ve("cell", "type",           "String",  5L, "axis-cross-ref string")
    ve("cell", "batch",          "String",  5L, "axis-cross-ref string")
    ve("cell", "label",          "String",  5L, "special chars (space, bs)")
    ve("cell", "unicode_label",  "String",  5L, "non-ASCII strings")
    ve("cell", "named_subset",   "Float64", 5L, "set via named subset")

    # ---- Gene vectors (n=3) ----
    ve("gene", "is_lateral", "Bool",   3L)
    ve("gene", "marker",     "String", 3L)

    # ---- Batch / type vectors ----
    ve("batch", "donor", "String", 3L)
    ve("type",  "color", "String", 3L)

    # ---- single / big / unicode axis vectors ----
    ve("single_axis",  "single_int", "Int32",   1L, "length-1 vector")
    ve("single_axis",  "single_dbl", "Float64", 1L, "length-1 vector")
    ve("single_axis",  "single_chr", "String",  1L, "length-1 vector")
    ve("big_axis",     "big_int",    "Int32",   10L)
    ve("big_axis",     "big_dbl",    "Float64", 10L)
    ve("unicode_axis", "tag",        "String",  3L)

    # ---- Empty axis vectors (length 0) ----
    ve("empty_axis", "empty_int",   "Int32",   0L)
    ve("empty_axis", "empty_dbl",   "Float64", 0L)
    ve("empty_axis", "empty_chr",   "String",  0L)
    ve("empty_axis", "empty_lgl",   "Bool",    0L)
    ve("empty_axis", "empty_int64", "Int64",   0L)

    # ---- Matrices (cell x gene = 5 x 3) ----
    mx("cell", "gene", "UMIs_int",   "Int32",   5L, 3L,
       storage = "dense", notes = "dense int")
    mx("cell", "gene", "frac",       "Float64", 5L, 3L,
       storage = "dense", notes = "dense double")
    mx("cell", "gene", "is_present", "Bool",    5L, 3L,
       storage = "dense", notes = "dense bool")
    mx("cell", "gene", "sparse_umis",  "Int32",   5L, 3L,
       storage = "sparse", notes = "sparse int")
    mx("cell", "gene", "sparse_frac",  "Float64", 5L, 3L,
       storage = "sparse", notes = "sparse double")
    mx("cell", "gene", "all_zero_sparse", "Float64", 5L, 3L,
       storage = "sparse", notes = "sparse all-zero (drop0)")
    mx("cell", "gene", "alternating_sparse", "Float64", 5L, 3L,
       storage = "sparse", notes = "checkerboard sparse")
    mx("cell", "gene", "frac_with_nan", "Float64", 5L, 3L,
       storage = "dense", notes = "dense with NaN cells")

    # ---- Square (cell x cell = 5 x 5) ----
    mx("cell", "cell", "distance",   "Int32",   5L, 5L,
       storage = "dense", notes = "square int dense")
    mx("cell", "cell", "similarity", "Float64", 5L, 5L,
       storage = "dense", notes = "square double dense")
    mx("cell", "cell", "dist_nan",   "Float64", 5L, 5L,
       storage = "dense", notes = "square double with NaN")

    # ---- Cross-shape edges ----
    mx("single_axis", "gene",        "single_row_int",  "Int32",   1L, 3L,
       storage = "dense", notes = "1 x N matrix")
    mx("cell",        "single_axis", "single_col_int",  "Int32",   5L, 1L,
       storage = "dense", notes = "N x 1 matrix")
    mx("empty_axis",  "gene",        "empty_rows_int",  "Int32",   0L, 3L,
       storage = "dense", notes = "0 x N matrix")
    mx("cell",        "empty_axis",  "empty_cols_int",  "Int32",   5L, 0L,
       storage = "dense", notes = "N x 0 matrix")
    mx("empty_axis",  "empty_axis",  "empty_sq_int",    "Int32",   0L, 0L,
       storage = "dense", notes = "0 x 0 matrix")

    M
}

# ---------------------------------------------------------------------
# Writer
# ---------------------------------------------------------------------

# Build a sparse dgCMatrix of given dims with i,j,x triples (1-based).
.sparse <- function(nr, nc, i = integer(0), j = integer(0),
                    x = numeric(0)) {
    if (length(i) == 0L) {
        sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                     dims = c(nr, nc))
    } else {
        sparseMatrix(i = i, j = j, x = x, dims = c(nr, nc))
    }
}

# Storage helpers ------------------------------------------------------
.int32_extremes <- function() {
    c(-.Machine$integer.max, -1L, 0L, 1L, .Machine$integer.max)
}
.int64_extremes <- function() {
    bit64::as.integer64(c("-4611686018427387904", "0", "1",
                          "4294967296", "4611686018427387904"))
}
.double_extremes <- function() {
    c(.Machine$double.xmin, -1, 0, 1, .Machine$double.xmax)
}

# Write everything into `daf`. Returns invisibly.
build_fixture <- function(daf) {
    # Scalars ----------------------------------------------------------
    set_scalar(daf, "flag_true",   TRUE)
    set_scalar(daf, "flag_false",  FALSE)
    set_scalar(daf, "intver",      7L)
    set_scalar(daf, "int_neg",     -42L)
    set_scalar(daf, "int_zero",    0L)
    set_scalar(daf, "int_max",     .Machine$integer.max)
    set_scalar(daf, "int64_big",   bit64::as.integer64("1099511627776"))
    set_scalar(daf, "int64_neg",   bit64::as.integer64(-1))
    set_scalar(daf, "int64_zero",  bit64::as.integer64(0))
    set_scalar(daf, "version",     1.0)
    set_scalar(daf, "pi_val",      3.141592653589793)
    set_scalar(daf, "dbl_neg",     -2.5)
    set_scalar(daf, "dbl_zero",    0.0)
    set_scalar(daf, "dbl_nan",     NaN)
    set_scalar(daf, "dbl_inf",     Inf)
    set_scalar(daf, "dbl_neg_inf", -Inf)
    set_scalar(daf, "dbl_tiny",    1e-300)
    set_scalar(daf, "dbl_huge",    1e300)
    set_scalar(daf, "title",       "backend-parity")
    set_scalar(daf, "empty_str",   "")
    set_scalar(daf, "unicode_str", "alpha-beta-gamma-☃-\U0001F31F")
    set_scalar(daf, "special_str", "q\"b\\n\nl")

    # Axes -------------------------------------------------------------
    add_axis(daf, "cell",        c("A", "B", "C", "D", "E"))
    add_axis(daf, "gene",        c("g1", "g2", "g3"))
    add_axis(daf, "batch",       c("b1", "b2", "b3"))
    add_axis(daf, "type",        c("U", "V", "W"))
    add_axis(daf, "single_axis", c("only"))
    add_axis(daf, "empty_axis",  character(0))
    add_axis(daf, "big_axis",    sprintf("e%02d", 1:10))
    add_axis(daf, "unicode_axis", c("à", "ß", "☃"))

    # Cell vectors -----------------------------------------------------
    set_vector(daf, "cell", "age",            c(10L, 20L, 30L, 40L, 50L))
    set_vector(daf, "cell", "neg_age",        c(-5L, -1L, 0L, 1L, 5L))
    set_vector(daf, "cell", "ties",           c(3L, 0L, 1L, 0L, 2L))
    set_vector(daf, "cell", "int_extremes",   .int32_extremes())
    set_vector(daf, "cell", "bignum",
               bit64::as.integer64(c(1, 2, 3, 4, 5)))
    set_vector(daf, "cell", "int64_extremes", .int64_extremes())
    set_vector(daf, "cell", "score",          c(0.5, 1.5, 2.5, -1.0, 3.5))
    set_vector(daf, "cell", "all_zero",       c(0, 0, 0, 0, 0))
    set_vector(daf, "cell", "with_nan",       c(1.0, NaN, 3.0, NaN, 5.0))
    set_vector(daf, "cell", "all_nan",        rep(NaN, 5))
    set_vector(daf, "cell", "infs",           c(Inf, -Inf, NaN, 0.0, 1.0))
    set_vector(daf, "cell", "all_neg",        c(-1.5, -2.5, -3.5, -4.5, -5.5))
    set_vector(daf, "cell", "dbl_extremes",   .double_extremes())
    set_vector(daf, "cell", "is_doublet",     c(TRUE, FALSE, TRUE, FALSE, TRUE))
    set_vector(daf, "cell", "all_true",       rep(TRUE, 5))
    set_vector(daf, "cell", "all_false",      rep(FALSE, 5))
    set_vector(daf, "cell", "type",           c("U", "V", "U", "W", "V"))
    set_vector(daf, "cell", "batch",          c("b1", "b2", "b1", "b3", "b2"))
    set_vector(daf, "cell", "label",          c("", "x", "y\\z", "a b", "Z"))
    set_vector(daf, "cell", "unicode_label",
               c("á", "ß", "γ", "☃", "\U0001F31F"))
    # Named subset (input order != axis order) — should round-trip in axis order.
    set_vector(daf, "cell", "named_subset",
               c(C = 2.5, A = 0.5, B = 1.5, E = 3.5, D = -1.0))

    # Gene vectors -----------------------------------------------------
    set_vector(daf, "gene", "is_lateral", c(TRUE, FALSE, FALSE))
    set_vector(daf, "gene", "marker",     c("lo", "hi", "lo"))

    # Batch / type / single / big / unicode vectors --------------------
    set_vector(daf, "batch", "donor", c("dA", "dB", "dC"))
    set_vector(daf, "type",  "color", c("red", "green", "blue"))
    set_vector(daf, "single_axis", "single_int", 42L)
    set_vector(daf, "single_axis", "single_dbl", -3.14)
    set_vector(daf, "single_axis", "single_chr", "only_value")
    set_vector(daf, "big_axis",    "big_int", 1:10)
    set_vector(daf, "big_axis",    "big_dbl", as.double(1:10) / 3)
    set_vector(daf, "unicode_axis", "tag",
               c("first", "second", "third"))

    # Empty axis vectors (length 0) ------------------------------------
    set_vector(daf, "empty_axis", "empty_int",   integer(0))
    set_vector(daf, "empty_axis", "empty_dbl",   numeric(0))
    set_vector(daf, "empty_axis", "empty_chr",   character(0))
    set_vector(daf, "empty_axis", "empty_lgl",   logical(0))
    set_vector(daf, "empty_axis", "empty_int64", bit64::as.integer64(integer(0)))

    # Matrices (cell x gene = 5 x 3) -----------------------------------
    set_matrix(daf, "cell", "gene", "UMIs_int",
               matrix(c(1L, 4L, 7L, 0L, 3L,
                        2L, 5L, 8L, 1L, 4L,
                        3L, 6L, 9L, 2L, 5L),
                      nrow = 5L, ncol = 3L))
    set_matrix(daf, "cell", "gene", "frac",
               matrix(c(0.1, 0.4, 0.7, 0.0, 0.3,
                        0.2, 0.5, 0.8, 0.1, 0.4,
                        0.3, 0.6, 0.9, 0.2, 0.5),
                      nrow = 5L, ncol = 3L))
    set_matrix(daf, "cell", "gene", "is_present",
               matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE,
                        FALSE, TRUE, FALSE, TRUE, FALSE,
                        TRUE, TRUE, FALSE, FALSE, TRUE),
                      nrow = 5L, ncol = 3L))
    set_matrix(daf, "cell", "gene", "sparse_umis",
               as(matrix(c(0L, 0L, 7L, 0L, 0L,
                           0L, 0L, 0L, 0L, 4L,
                           3L, 0L, 0L, 0L, 0L),
                         nrow = 5L, ncol = 3L), "CsparseMatrix"))
    set_matrix(daf, "cell", "gene", "sparse_frac",
               as(matrix(c(0, 0, 1.5, 0, 0,
                           0, 0, 0, 0, 2.5,
                           0.5, 0, 0, 0, 0),
                         nrow = 5L, ncol = 3L), "CsparseMatrix"))
    set_matrix(daf, "cell", "gene", "all_zero_sparse",
               .sparse(5L, 3L))
    # Checkerboard: nonzeros at (i+j) even
    .alt_mat <- matrix(0, nrow = 5L, ncol = 3L)
    for (i in seq_len(5L)) for (j in seq_len(3L))
        if ((i + j) %% 2L == 0L) .alt_mat[i, j] <- i + 10 * j
    set_matrix(daf, "cell", "gene", "alternating_sparse",
               as(.alt_mat, "CsparseMatrix"))
    set_matrix(daf, "cell", "gene", "frac_with_nan",
               matrix(c(0.1, NaN, 0.7, 0.0, 0.3,
                        NaN, 0.5, 0.8, NaN, 0.4,
                        0.3, 0.6, NaN, 0.2, 0.5),
                      nrow = 5L, ncol = 3L))

    # Square (cell x cell = 5 x 5) -------------------------------------
    set_matrix(daf, "cell", "cell", "distance",
               matrix(as.integer(c(0, 1, 2, 3, 4,
                                   1, 0, 1, 2, 3,
                                   2, 1, 0, 1, 2,
                                   3, 2, 1, 0, 1,
                                   4, 3, 2, 1, 0)),
                      nrow = 5L, ncol = 5L))
    set_matrix(daf, "cell", "cell", "similarity",
               matrix(c(1.0, 0.9, 0.8, 0.7, 0.6,
                        0.9, 1.0, 0.9, 0.8, 0.7,
                        0.8, 0.9, 1.0, 0.9, 0.8,
                        0.7, 0.8, 0.9, 1.0, 0.9,
                        0.6, 0.7, 0.8, 0.9, 1.0),
                      nrow = 5L, ncol = 5L))
    set_matrix(daf, "cell", "cell", "dist_nan",
               matrix(c(0,   1,   NaN, 3,   4,
                        1,   0,   1,   2,   NaN,
                        NaN, 1,   0,   1,   2,
                        3,   2,   1,   0,   1,
                        4,   NaN, 2,   1,   0),
                      nrow = 5L, ncol = 5L))

    # Cross-shape edges ------------------------------------------------
    set_matrix(daf, "single_axis", "gene", "single_row_int",
               matrix(c(11L, 22L, 33L), nrow = 1L, ncol = 3L))
    set_matrix(daf, "cell", "single_axis", "single_col_int",
               matrix(c(1L, 2L, 3L, 4L, 5L), nrow = 5L, ncol = 1L))
    set_matrix(daf, "empty_axis", "gene", "empty_rows_int",
               matrix(integer(0), nrow = 0L, ncol = 3L))
    set_matrix(daf, "cell", "empty_axis", "empty_cols_int",
               matrix(integer(0), nrow = 5L, ncol = 0L))
    set_matrix(daf, "empty_axis", "empty_axis", "empty_sq_int",
               matrix(integer(0), nrow = 0L, ncol = 0L))

    invisible(daf)
}
