# Logical mask query builders (Phase E): 6 string-op exports —
# AndMask, AndNegatedMask, OrMask, OrNegatedMask, XorMask, XorNegatedMask.

.prior_mask_query <- function(src = "[ type") {
    DafrQuery(ast = parse_query(src), canonical = src)
}

# ---- AndMask (string-op) ---------------------------------------------------

test_that("AndMask builds a & <prop> query fragment", {
    q <- AndMask("prop")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "& prop")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("AndMask composes after BeginMask via pipe", {
    q <- BeginMask("prop1") |> AndMask("prop2")
    expect_identical(q@canonical, "[ prop1 & prop2")
    expect_identical(q@ast, parse_query("[ prop1 & prop2"))
})

test_that("AndMask quotes property names with spaces", {
    q <- AndMask("cell type")
    expect_identical(q@canonical, "& \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("AndMask rejects non-character property", {
    expect_error(AndMask(42), "character scalar")
})

# ---- AndNegatedMask (string-op) --------------------------------------------

test_that("AndNegatedMask builds a & ! <prop> query fragment", {
    q <- AndNegatedMask("prop")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "& ! prop")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("AndNegatedMask composes after BeginMask via pipe", {
    q <- BeginMask("prop1") |> AndNegatedMask("prop2")
    expect_identical(q@canonical, "[ prop1 & ! prop2")
    expect_identical(q@ast, parse_query("[ prop1 & ! prop2"))
})

test_that("AndNegatedMask quotes property names with spaces", {
    q <- AndNegatedMask("cell type")
    expect_identical(q@canonical, "& ! \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("AndNegatedMask rejects non-character property", {
    expect_error(AndNegatedMask(42), "character scalar")
})

# ---- OrMask (string-op) ----------------------------------------------------

test_that("OrMask builds a | <prop> query fragment", {
    q <- OrMask("prop")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "| prop")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("OrMask composes after BeginMask via pipe", {
    q <- BeginMask("prop1") |> OrMask("prop2")
    expect_identical(q@canonical, "[ prop1 | prop2")
    expect_identical(q@ast, parse_query("[ prop1 | prop2"))
})

test_that("OrMask quotes property names with spaces", {
    q <- OrMask("cell type")
    expect_identical(q@canonical, "| \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("OrMask rejects non-character property", {
    expect_error(OrMask(42), "character scalar")
})

# ---- OrNegatedMask (string-op) ---------------------------------------------

test_that("OrNegatedMask builds a | ! <prop> query fragment", {
    q <- OrNegatedMask("prop")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "| ! prop")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("OrNegatedMask composes after BeginMask via pipe", {
    q <- BeginMask("prop1") |> OrNegatedMask("prop2")
    expect_identical(q@canonical, "[ prop1 | ! prop2")
    expect_identical(q@ast, parse_query("[ prop1 | ! prop2"))
})

test_that("OrNegatedMask quotes property names with spaces", {
    q <- OrNegatedMask("cell type")
    expect_identical(q@canonical, "| ! \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("OrNegatedMask rejects non-character property", {
    expect_error(OrNegatedMask(42), "character scalar")
})

# ---- XorMask (string-op) ---------------------------------------------------

test_that("XorMask builds a ^ <prop> query fragment", {
    q <- XorMask("prop")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "^ prop")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("XorMask composes after BeginMask via pipe", {
    q <- BeginMask("prop1") |> XorMask("prop2")
    expect_identical(q@canonical, "[ prop1 ^ prop2")
    expect_identical(q@ast, parse_query("[ prop1 ^ prop2"))
})

test_that("XorMask quotes property names with spaces", {
    q <- XorMask("cell type")
    expect_identical(q@canonical, "^ \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("XorMask rejects non-character property", {
    expect_error(XorMask(42), "character scalar")
})

# ---- XorNegatedMask (string-op) --------------------------------------------

test_that("XorNegatedMask builds a ^ ! <prop> query fragment", {
    q <- XorNegatedMask("prop")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "^ ! prop")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("XorNegatedMask composes after BeginMask via pipe", {
    q <- BeginMask("prop1") |> XorNegatedMask("prop2")
    expect_identical(q@canonical, "[ prop1 ^ ! prop2")
    expect_identical(q@ast, parse_query("[ prop1 ^ ! prop2"))
})

test_that("XorNegatedMask quotes property names with spaces", {
    q <- XorNegatedMask("cell type")
    expect_identical(q@canonical, "^ ! \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("XorNegatedMask rejects non-character property", {
    expect_error(XorNegatedMask(42), "character scalar")
})

# ---- Cross-cutting: multi-mask composition order --------------------------

test_that("mask composition order is respected in canonical", {
    q <- BeginMask("a") |> AndMask("b") |> OrNegatedMask("c") |> EndMask()
    expect_identical(q@canonical, "[ a & b | ! c ]")
    expect_identical(q@ast, parse_query("[ a & b | ! c ]"))
})

test_that("all six logical masks can chain together", {
    q <- BeginMask("a") |>
        AndMask("b") |>
        AndNegatedMask("c") |>
        OrMask("d") |>
        OrNegatedMask("e") |>
        XorMask("f") |>
        XorNegatedMask("g") |>
        EndMask()
    expect_identical(
        q@canonical,
        "[ a & b & ! c | d | ! e ^ f ^ ! g ]"
    )
})
