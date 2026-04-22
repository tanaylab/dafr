# Slice 8 Task 10: grouped-reduction rewrite tests
#
# Covers the four grouped patterns (G1/G2/G3/G4) end-to-end via the query DSL
# plus R-level type-sniffing fallback + Mode-on-character special case.

test_that("G1 grouped-vector works for all numeric builtins", {
    d <- memory_daf(name = "t")
    add_axis(d, "ax", paste0("e", 1:6))
    set_vector(d, "ax", "x", c(1, 2, 3, 4, 5, 6))
    set_vector(d, "ax", "g", c("a", "a", "b", "b", "c", "c"))

    # Sum: a=1+2=3, b=3+4=7, c=5+6=11
    expect_equal(
        unname(get_query(d, "@ ax : x / g >| Sum")),
        c(3, 7, 11)
    )
    # Mean
    expect_equal(
        unname(get_query(d, "@ ax : x / g >| Mean")),
        c(1.5, 3.5, 5.5)
    )
    # Min / Max
    expect_equal(unname(get_query(d, "@ ax : x / g >| Min")), c(1, 3, 5))
    expect_equal(unname(get_query(d, "@ ax : x / g >| Max")), c(2, 4, 6))
    # Var (population) — each pair (k, k+1) has mean k+0.5, var = 0.25
    expect_equal(unname(get_query(d, "@ ax : x / g >| Var")),
        c(0.25, 0.25, 0.25), tolerance = 1e-12)
    expect_equal(unname(get_query(d, "@ ax : x / g >| Std")),
        c(0.5, 0.5, 0.5), tolerance = 1e-12)
    # VarN / StdN with eps=0.1
    mu <- c(1.5, 3.5, 5.5); v <- c(0.25, 0.25, 0.25)
    expect_equal(unname(get_query(d, "@ ax : x / g >| VarN eps 0.1")),
        v / (mu + 0.1), tolerance = 1e-12)
    expect_equal(unname(get_query(d, "@ ax : x / g >| StdN eps 0.1")),
        sqrt(v) / (mu + 0.1), tolerance = 1e-12)
    # GeoMean (positive values; eps = 0)
    expect_equal(unname(get_query(d, "@ ax : x / g >| GeoMean")),
        c(sqrt(2), sqrt(12), sqrt(30)), tolerance = 1e-12)
    # Median / Quantile
    expect_equal(unname(get_query(d, "@ ax : x / g >| Median")),
        c(1.5, 3.5, 5.5), tolerance = 1e-12)
    expect_equal(unname(get_query(d, "@ ax : x / g >| Quantile p 0.25")),
        c(1.25, 3.25, 5.25), tolerance = 1e-12)
    # Mode (numeric)
    v_mode <- get_query(d, "@ ax : x / g >| Mode")
    expect_length(v_mode, 3L)
    expect_type(v_mode, "double")
})

test_that("G1 grouped-vector preserves group-appearance order (factor levels)", {
    d <- memory_daf(name = "t")
    add_axis(d, "ax", paste0("e", 1:6))
    set_vector(d, "ax", "x", c(10, 20, 30, 40, 50, 60))
    # Groups in scramble order so alphabetical != first-appearance
    set_vector(d, "ax", "g", c("z", "z", "a", "a", "m", "m"))
    v <- get_query(d, "@ ax : x / g >| Sum")
    expect_equal(names(v), c("z", "a", "m"))
    expect_equal(unname(v), c(30, 70, 110))
})

test_that("G1 Mode on character vector returns character output", {
    d <- memory_daf(name = "t")
    add_axis(d, "ax", paste0("e", 1:6))
    set_vector(d, "ax", "color", c("red", "red", "blue", "blue", "red", "blue"))
    set_vector(d, "ax", "g", c("a", "a", "b", "b", "a", "b"))
    # a = (red, red, red) -> red; b = (blue, blue, blue) -> blue
    out <- get_query(d, "@ ax : color / g >| Mode")
    expect_type(out, "character")
    expect_length(out, 2L)
    expect_equal(unname(out), c("red", "blue"))
    expect_equal(names(out), c("a", "b"))
})

test_that("G1 fallback preserves integer output from custom reduction", {
    register_reduction("Slice10Len",
        function(v, ...) length(v), overwrite = TRUE)
    d <- memory_daf(name = "t")
    add_axis(d, "ax", paste0("e", 1:10))
    set_vector(d, "ax", "x", 1:10)
    set_vector(d, "ax", "g", rep(c("a", "b"), 5L))
    out <- get_query(d, "@ ax : x / g >| Slice10Len")
    expect_type(out, "integer")
    expect_equal(unname(out), c(5L, 5L))
})

test_that("G1 fallback preserves logical output from custom reduction", {
    register_reduction("Slice10AnyPos",
        function(v, ...) any(v > 0), overwrite = TRUE)
    d <- memory_daf(name = "t")
    add_axis(d, "ax", paste0("e", 1:4))
    set_vector(d, "ax", "x", c(-1, -2, 1, -3))
    set_vector(d, "ax", "g", c("a", "a", "b", "b"))
    out <- get_query(d, "@ ax : x / g >| Slice10AnyPos")
    expect_type(out, "logical")
    expect_equal(unname(out), c(FALSE, TRUE))
})

test_that("G2 row-grouped + ReduceToColumn: sparse -> ngroups x ncol", {
    skip_if_not_installed("Matrix")
    set.seed(49)
    m <- Matrix::rsparsematrix(30L, 20L, density = 0.4,
        rand.x = function(n) runif(n, 0.1, 3))
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    row_g <- rep(c("a", "b", "c"), each = 10L)
    set_vector(d, "r", "rg", row_g)

    for (op in c("Sum", "Mean", "Min", "Max", "Var", "Std")) {
        out <- get_query(d, sprintf("@ r @ c :: x -/ rg >- %s", op))
        expect_equal(dim(out), c(3L, 20L), info = op)
        expect_equal(rownames(out), c("a", "b", "c"), info = op)
        expect_equal(colnames(out), paste0("c", 1:20), info = op)
    }
    # Content check for Sum: compare to Matrix::rowsum of dense matrix.
    out_sum <- get_query(d, "@ r @ c :: x -/ rg >- Sum")
    dense <- as.matrix(m)
    expected <- rowsum(dense, row_g)
    expect_equal(out_sum, expected[c("a", "b", "c"), ], tolerance = 1e-9,
        ignore_attr = TRUE)
})

test_that("G2 row-grouped + ReduceToColumn: dense -> ngroups x ncol", {
    set.seed(50)
    m <- matrix(runif(30L * 20L, 0.1, 3), 30L, 20L)
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    row_g <- rep(c("a", "b", "c"), each = 10L)
    set_vector(d, "r", "rg", row_g)

    for (op in c("Sum", "Mean", "Min", "Max", "Var", "Std", "Median", "Mode")) {
        out <- get_query(d, sprintf("@ r @ c :: x -/ rg >- %s", op))
        expect_equal(dim(out), c(3L, 20L), info = op)
    }
    # Content check
    out_mean <- get_query(d, "@ r @ c :: x -/ rg >- Mean")
    expected <- rowsum(m, row_g) / as.integer(table(row_g))
    expect_equal(out_mean, expected[c("a", "b", "c"), ],
        tolerance = 1e-9, ignore_attr = TRUE)
})

test_that("G3 col-grouped + ReduceToRow: sparse -> nrow x ngroups", {
    skip_if_not_installed("Matrix")
    set.seed(51)
    m <- Matrix::rsparsematrix(30L, 20L, density = 0.4,
        rand.x = function(n) runif(n, 0.1, 3))
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    col_g <- rep(c("x", "y", "z", "w"), each = 5L)
    set_vector(d, "c", "cg", col_g)

    for (op in c("Sum", "Mean", "Min", "Max", "Var", "Std")) {
        out <- get_query(d, sprintf("@ r @ c :: x |/ cg >| %s", op))
        expect_equal(dim(out), c(30L, 4L), info = op)
        expect_equal(colnames(out), c("x", "y", "z", "w"), info = op)
    }
    # Content check for Sum: column-groupsum of dense matrix.
    out_sum <- get_query(d, "@ r @ c :: x |/ cg >| Sum")
    dense <- as.matrix(m)
    expected <- t(rowsum(t(dense), col_g))[, c("x", "y", "z", "w")]
    expect_equal(out_sum, expected, tolerance = 1e-9,
        ignore_attr = TRUE)
})

test_that("G3 col-grouped + ReduceToRow: dense -> nrow x ngroups", {
    set.seed(52)
    m <- matrix(runif(30L * 20L, 0.1, 3), 30L, 20L)
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    col_g <- rep(c("x", "y", "z", "w"), each = 5L)
    set_vector(d, "c", "cg", col_g)

    for (op in c("Sum", "Mean", "Min", "Max", "Median")) {
        out <- get_query(d, sprintf("@ r @ c :: x |/ cg >| %s", op))
        expect_equal(dim(out), c(30L, 4L), info = op)
    }
})

test_that("G4a row-grouped + ReduceToRow: vector[ngroups]", {
    set.seed(53)
    m <- matrix(runif(30L * 20L, 0.1, 3), 30L, 20L)
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    row_g <- rep(c("a", "b", "c"), each = 10L)
    set_vector(d, "r", "rg", row_g)

    out <- get_query(d, "@ r @ c :: x -/ rg >| Sum")
    # G4a decomposes as: inner ReduceToRow (per-col Sum) -> vector[ncol]
    # then grouped-vector-reduce on... wait — inner reduces first along the
    # non-grouped axis, giving a per-col vector, but the groups (row groups)
    # don't apply to that vector. Instead G4 is: the inner reduce collapses
    # the ungrouped axis, and the outer reduce collapses the grouped axis.
    #
    # For row-grouped + ReduceToRow: inner = per-col reduce across all rows
    # within each group -> matrix ngroups x ncol, then reduce across cols
    # in each group row -> vector[ngroups].
    # Easier: it's ReduceToRow applied to an already row-grouped matrix, i.e.
    # compute row sums grouped, then sum them. For Sum it's just sum per group.
    expected <- vapply(split(seq_len(30L), row_g),
        function(i) sum(m[i, , drop = FALSE]), numeric(1))
    expect_equal(unname(out), unname(expected[c("a", "b", "c")]),
        tolerance = 1e-9)
    expect_equal(names(out), c("a", "b", "c"))
})

test_that("G4b col-grouped + ReduceToColumn: vector[ngroups]", {
    set.seed(54)
    m <- matrix(runif(30L * 20L, 0.1, 3), 30L, 20L)
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    col_g <- rep(c("x", "y", "z", "w"), each = 5L)
    set_vector(d, "c", "cg", col_g)

    out <- get_query(d, "@ r @ c :: x |/ cg >- Sum")
    expected <- vapply(split(seq_len(20L), col_g),
        function(j) sum(m[, j, drop = FALSE]), numeric(1))
    expect_equal(unname(out), unname(expected[c("x", "y", "z", "w")]),
        tolerance = 1e-9)
    expect_equal(names(out), c("x", "y", "z", "w"))
})

test_that("G2 sparse Median and Quantile route through dedicated kernel", {
    skip_if_not_installed("Matrix")
    set.seed(55)
    m <- Matrix::rsparsematrix(30L, 20L, density = 0.4, rand.x = rnorm)
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    row_g <- rep(c("a", "b", "c"), each = 10L)
    set_vector(d, "r", "rg", row_g)

    out_med <- get_query(d, "@ r @ c :: x -/ rg >- Median")
    expect_equal(dim(out_med), c(3L, 20L))
    # Compare to split+apply.
    dense <- as.matrix(m)
    expected <- matrix(0, 3L, 20L)
    idx <- split(seq_len(30L), row_g)
    for (gi in seq_len(3L)) {
        g <- c("a", "b", "c")[gi]
        for (j in seq_len(20L)) {
            expected[gi, j] <- stats::quantile(
                dense[idx[[g]], j], 0.5, type = 7L, names = FALSE)
        }
    }
    expect_equal(unname(out_med), expected, tolerance = 1e-9)

    out_q <- get_query(d, "@ r @ c :: x -/ rg >- Quantile p 0.25")
    expect_equal(dim(out_q), c(3L, 20L))
})

test_that("G2 sparse Mode routes through dedicated kernel", {
    skip_if_not_installed("Matrix")
    set.seed(56)
    vals <- sample(c(0, 1, 2), 200L, replace = TRUE, prob = c(0.7, 0.2, 0.1))
    m <- Matrix::sparseMatrix(
        i = rep(seq_len(20L), 10L),
        j = rep(seq_len(10L), each = 20L),
        x = vals, dims = c(20L, 10L))
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:20))
    add_axis(d, "c", paste0("c", 1:10))
    set_matrix(d, "r", "c", "x", m)
    row_g <- rep(c("a", "b"), each = 10L)
    set_vector(d, "r", "rg", row_g)
    out <- get_query(d, "@ r @ c :: x -/ rg >- Mode")
    expect_equal(dim(out), c(2L, 10L))
})

test_that("G2 dense Median works via matrixStats fallback", {
    set.seed(57)
    m <- matrix(runif(30L * 20L), 30L, 20L)
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:30))
    add_axis(d, "c", paste0("c", 1:20))
    set_matrix(d, "r", "c", "x", m)
    row_g <- rep(c("a", "b", "c"), each = 10L)
    set_vector(d, "r", "rg", row_g)
    out <- get_query(d, "@ r @ c :: x -/ rg >- Median")
    expect_equal(dim(out), c(3L, 20L))
    dense <- m
    idx <- split(seq_len(30L), row_g)
    expected <- matrix(0, 3L, 20L)
    for (gi in seq_along(c("a", "b", "c"))) {
        g <- c("a", "b", "c")[gi]
        for (j in seq_len(20L)) {
            expected[gi, j] <- stats::quantile(
                dense[idx[[g]], j], 0.5, type = 7L, names = FALSE)
        }
    }
    expect_equal(unname(out), expected, tolerance = 1e-9)
})

test_that("G2 lgCMatrix input falls back to split+apply path", {
    skip_if_not_installed("Matrix")
    # lgCMatrix: Matrix::nnzero-based with logical @x -> not dgCMatrix.
    set.seed(58)
    m <- Matrix::sparseMatrix(
        i = c(1L, 3L, 5L, 7L, 2L, 4L, 6L, 8L),
        j = c(1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L),
        x = TRUE, dims = c(8L, 2L))
    expect_s4_class(m, "lgCMatrix")
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:8))
    add_axis(d, "c", paste0("c", 1:2))
    set_matrix(d, "r", "c", "x", m)
    row_g <- rep(c("a", "b"), each = 4L)
    set_vector(d, "r", "rg", row_g)
    # Sum over logical: should succeed via fallback path
    out <- get_query(d, "@ r @ c :: x -/ rg >- Sum")
    expect_equal(dim(out), c(2L, 2L))
})

test_that("G2 fallback with custom int-returning reduction preserves type", {
    set.seed(59)
    register_reduction("Slice10RowLen",
        function(v, ...) length(v), overwrite = TRUE)
    m <- matrix(1:60, 6L, 10L)
    d <- memory_daf(name = "t")
    add_axis(d, "r", paste0("r", 1:6))
    add_axis(d, "c", paste0("c", 1:10))
    set_matrix(d, "r", "c", "x", m)
    set_vector(d, "r", "rg", rep(c("a", "b"), each = 3L))
    out <- get_query(d, "@ r @ c :: x -/ rg >- Slice10RowLen")
    expect_equal(dim(out), c(2L, 10L))
    expect_type(out, "integer")
    # Each group has 3 rows; length of row slice = ncol = 10; no wait: for
    # G2 ReduceToColumn, for each (group, col) pair we reduce the column
    # values within that group -> length = 3.
    expect_equal(unname(out)[1L, 1L], 3L)
})

test_that("G4 preserves integer output for user reductions", {
    m <- matrix(1:12, 3, 4)
    daf <- memory_daf("t")
    add_axis(daf, "r", c("a", "b", "c"))
    add_axis(daf, "c", c("w", "x", "y", "z"))
    set_matrix(daf, "r", "c", "m", m)
    set_vector(daf, "r", "rg", c("g1", "g1", "g2"))
    register_reduction("Slice10TestG4LenInt",
        function(v, ...) length(v), overwrite = TRUE)
    # G4a: row-grouped, ReduceToRow (double reduction)
    # Inner G2 gives 2 x 4 matrix; outer length() on each 4-element row -> 4L
    out <- get_query(daf, "@ r @ c :: m -/ rg >| Slice10TestG4LenInt")
    expect_type(out, "integer")
    expect_equal(unname(out), c(4L, 4L))
})

test_that("G4 supports logical and character user reductions without vapply error", {
    m <- matrix(1:12, 3, 4)
    daf <- memory_daf("t")
    add_axis(daf, "r", c("a", "b", "c"))
    add_axis(daf, "c", c("w", "x", "y", "z"))
    set_matrix(daf, "r", "c", "m", m)
    set_vector(daf, "r", "rg", c("g1", "g1", "g2"))

    register_reduction("Slice10TestG4AnyPos",
        function(v, ...) any(v > 0), overwrite = TRUE)
    out_log <- get_query(daf, "@ r @ c :: m -/ rg >| Slice10TestG4AnyPos")
    expect_type(out_log, "logical")
    expect_length(out_log, 2L)

    register_reduction("Slice10TestG4FirstChar",
        function(v, ...) as.character(v[1L]), overwrite = TRUE)
    out_char <- get_query(daf, "@ r @ c :: m -/ rg >| Slice10TestG4FirstChar")
    expect_type(out_char, "character")
    expect_length(out_char, 2L)
})
