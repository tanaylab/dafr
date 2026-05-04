# Format-API named-return contract: every backend's format_get_vector
# returns a named atomic vector (names = axis entries in axis order),
# and every format_get_matrix returns a matrix / dgCMatrix / lgCMatrix
# whose dimnames are list(rows-axis entries, cols-axis entries).

.fixture_named_memory_daf <- function() {
    d <- memory_daf(name = "names-fixture")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("gA", "gB"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    set_matrix(d, "cell", "gene", "expr",
        matrix(c(1.0, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    )
    set_matrix(d, "cell", "gene", "expr_sparse",
        Matrix::sparseMatrix(
            i = c(1L, 3L), j = c(1L, 2L), x = c(7, 9),
            dims = c(3L, 2L), repr = "C"
        )
    )
    d
}

test_that("MemoryDaf format_get_vector returns named atomic", {
    d <- .fixture_named_memory_daf()
    v <- format_get_vector(d, "cell", "donor")$value
    expect_equal(names(v), c("c1", "c2", "c3"))
    expect_equal(unname(v), c("d1", "d2", "d1"))
})

test_that("MemoryDaf format_get_matrix returns dense with axis dimnames", {
    d <- .fixture_named_memory_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr")$value
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("MemoryDaf format_get_matrix returns sparse with @Dimnames", {
    d <- .fixture_named_memory_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")$value
    expect_s4_class(m, "dgCMatrix")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})

.fixture_named_files_daf <- function(envir = parent.frame()) {
    src <- .fixture_named_memory_daf()
    root <- tempfile(pattern = "dafr-names-")
    dir.create(root)
    withr::defer(unlink(root, recursive = TRUE), envir = envir)
    dst <- files_daf(root, mode = "w+")
    copy_all(dst, src)
    dst
}

test_that("FilesDaf format_get_vector returns named atomic", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    v <- format_get_vector(d, "cell", "donor")$value
    expect_equal(names(v), c("c1", "c2", "c3"))
    expect_equal(unname(v), c("d1", "d2", "d1"))
})

test_that("FilesDaf format_get_matrix returns dense with axis dimnames", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr")$value
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("FilesDaf format_get_matrix returns sparse with @Dimnames", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")$value
    expect_s4_class(m, "dgCMatrix")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})

test_that("FilesDafReadOnly inherits the named contract", {
    skip_if_not_installed("withr")
    d <- read_only(.fixture_named_files_daf())
    expect_equal(names(format_get_vector(d, "cell", "donor")$value),
                 c("c1", "c2", "c3"))
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")$value
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})

test_that("ReadOnlyChainDaf format_get_* delegates with names", {
    a <- .fixture_named_memory_daf()
    b <- memory_daf(name = "overlay")
    add_axis(b, "cell", c("c1", "c2", "c3"))
    add_axis(b, "gene", c("gA", "gB"))
    set_vector(b, "cell", "donor_alt", c("dA", "dB", "dA"))
    ch <- chain_reader(list(a, b))

    expect_equal(names(format_get_vector(ch, "cell", "donor")$value),
                 c("c1", "c2", "c3"))
    expect_equal(names(format_get_vector(ch, "cell", "donor_alt")$value),
                 c("c1", "c2", "c3"))
    m <- format_get_matrix(ch, "cell", "gene", "expr")$value
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("ContractDaf format_get_* delegates with names", {
    skip_if_not_installed("withr")
    base <- .fixture_named_memory_daf()
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        ct <- Contract(
            axes = list(
                cell = list(RequiredInput, "per-cell axis"),
                gene = list(RequiredInput, "per-gene axis")
            ),
            data = list(
                contract_vector("cell", "donor",
                    RequiredInput, "character", "donor id"),
                contract_matrix("cell", "gene", "expr",
                    RequiredInput, "numeric", "cell-gene expression")
            )
        )
        cd <- contractor("names_test", ct, base)
        expect_equal(names(format_get_vector(cd, "cell", "donor")$value),
                     c("c1", "c2", "c3"))
        m <- format_get_matrix(cd, "cell", "gene", "expr")$value
        expect_equal(rownames(m), c("c1", "c2", "c3"))
        expect_equal(colnames(m), c("gA", "gB"))
    })
})

test_that("ViewDaf format_get_* preserves names through identity view", {
    base <- .fixture_named_memory_daf()
    v <- viewer(base, axes = list(VIEW_ALL_AXES))
    expect_equal(names(format_get_vector(v, "cell", "donor")$value),
                 c("c1", "c2", "c3"))
    m <- format_get_matrix(v, "cell", "gene", "expr")$value
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("names survive memory -> files -> read_only -> get_query roundtrip", {
    skip_if_not_installed("withr")
    src <- .fixture_named_memory_daf()
    root <- withr::local_tempfile(pattern = "dafr-rt-")
    dir.create(root)
    files <- files_daf(root, mode = "w+", name = "rt-files")
    copy_all(files, src)
    files_ro <- read_only(files)

    # Public API: get_vector / get_matrix / get_axis
    v <- get_vector(files_ro, "cell", "donor")
    expect_equal(names(v), c("c1", "c2", "c3"))
    m <- get_matrix(files_ro, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
    expect_equal(axis_vector(files_ro, "cell"), c("c1", "c2", "c3"))

    # get_query: matrix lookup
    qm <- get_query(files_ro, "@ cell @ gene :: expr")
    expect_equal(rownames(qm), c("c1", "c2", "c3"))
    expect_equal(colnames(qm), c("gA", "gB"))

    # get_query: vector lookup
    qv <- get_query(files_ro, "@ cell : donor")
    expect_equal(names(qv), c("c1", "c2", "c3"))
})

test_that("names survive memory -> as_anndata roundtrip", {
    skip_if_not_installed("anndata")
    src <- .fixture_named_memory_daf()
    ad <- as_anndata(src, obs_axis = "cell", var_axis = "gene",
                     x_name = "expr")
    # obs row names are cell entries; var row names are gene entries.
    expect_equal(rownames(ad$obs), c("c1", "c2", "c3"))
    expect_equal(rownames(ad$var), c("gA", "gB"))
    # X matrix carries dimnames
    expect_equal(rownames(ad$X), c("c1", "c2", "c3"))
    expect_equal(colnames(ad$X), c("gA", "gB"))
})
