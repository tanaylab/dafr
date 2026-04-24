# Tests exercising dplyr helpers that should work via our verb
# delegation (we call dplyr::mutate/summarise/select on the realized
# tibble). These confirm the delegation path doesn't break common
# dplyr idioms.

# ---- window functions in mutate ---------------------------------------

test_that("window fns inside mutate: lag/lead", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(10L, 20L, 30L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(prev = dplyr::lag(x), nxt = dplyr::lead(x)) |>
        dplyr::collect()
    expect_identical(df$prev, c(NA_integer_, 10L, 20L))
    expect_identical(df$nxt, c(20L, 30L, NA_integer_))
})

test_that("window fns inside mutate: cumsum and row_number", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(10L, 20L, 30L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(
            cx = cumsum(x),
            rn = dplyr::row_number()
        ) |>
        dplyr::collect()
    expect_identical(df$cx, c(10L, 30L, 60L))
    expect_identical(df$rn, 1:3)
})

test_that("window fns: min_rank and ntile", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "x", c(30L, 10L, 20L, 40L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(
            r = dplyr::min_rank(x),
            q = dplyr::ntile(x, 2L)
        ) |>
        dplyr::collect()
    expect_identical(df$r, c(3L, 1L, 2L, 4L))
    expect_identical(df$q, c(2L, 1L, 1L, 2L))
})

test_that("grouped mutate evaluates window fns within group", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "A", "B", "B"))
    set_vector(d, "cell", "x", c(10L, 30L, 20L, 40L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor) |>
        dplyr::mutate(rn = dplyr::row_number()) |>
        dplyr::collect()
    expect_identical(df$rn[df$donor == "A"], c(1L, 2L))
    expect_identical(df$rn[df$donor == "B"], c(1L, 2L))
})

# ---- scalar helpers inside mutate -------------------------------------

test_that("if_else() and case_when() inside mutate", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(1L, 5L, 10L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(
            parity = dplyr::if_else(x %% 2L == 0L, "even", "odd"),
            bucket = dplyr::case_when(
                x < 3L ~ "small",
                x < 8L ~ "medium",
                TRUE ~ "big"
            )
        ) |>
        dplyr::collect()
    expect_identical(df$parity, c("odd", "odd", "even"))
    expect_identical(df$bucket, c("small", "medium", "big"))
})

test_that("coalesce() inside mutate", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(1L, NA_integer_, 3L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(y = dplyr::coalesce(x, 0L)) |>
        dplyr::collect()
    expect_identical(df$y, c(1L, 0L, 3L))
})

# ---- reduction helpers inside summarise -------------------------------

test_that("n_distinct() / first() / last() inside summarise", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "A", "B", "B"))
    set_vector(d, "cell", "x", c(10, 20, 10, 40))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::summarise(
            nd = dplyr::n_distinct(donor),
            fst = dplyr::first(x),
            lst = dplyr::last(x)
        )
    expect_identical(out$nd, 2L)
    expect_identical(out$fst, 10)
    expect_identical(out$lst, 40)
})

# ---- across() ---------------------------------------------------------

test_that("across(where(is.numeric), mean) inside summarise", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(1, 2, 3))
    set_vector(d, "cell", "b", c(10, 20, 30))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), mean))
    expect_identical(out$a, 2)
    expect_identical(out$b, 20)
})

test_that("across() inside mutate scales multiple columns", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(1, 2, 3))
    set_vector(d, "cell", "b", c(10, 20, 30))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(dplyr::across(c(a, b), ~ .x * 2, .names = "{.col}_x2")) |>
        dplyr::collect()
    expect_identical(df$a_x2, c(2, 4, 6))
    expect_identical(df$b_x2, c(20, 40, 60))
})

# ---- tidyselect helpers in select ------------------------------------

test_that("select(starts_with/contains/where) dispatches through our select", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "age_yr", c(1L, 2L))
    set_vector(d, "cell", "age_mo", c(12L, 24L))
    set_vector(d, "cell", "donor", c("A", "B"))
    df1 <- dplyr::tbl(d, "cell") |>
        dplyr::select(tidyselect::starts_with("age")) |>
        dplyr::collect()
    expect_setequal(colnames(df1), c("name", "age_yr", "age_mo"))

    df2 <- dplyr::tbl(d, "cell") |>
        dplyr::select(tidyselect::contains("yr")) |>
        dplyr::collect()
    expect_setequal(colnames(df2), c("name", "age_yr"))

    df3 <- dplyr::tbl(d, "cell") |>
        dplyr::select(tidyselect::where(is.integer)) |>
        dplyr::collect()
    expect_setequal(colnames(df3), c("name", "age_yr", "age_mo"))
})

# ---- multi-column group_by -------------------------------------------

test_that("group_by(a, b) + summarise returns a plain tibble", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "A", "B", "B"))
    set_vector(d, "cell", "sex", c("F", "M", "F", "M"))
    set_vector(d, "cell", "x", c(1, 2, 3, 4))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor, sex) |>
        dplyr::summarise(m = mean(x), .groups = "drop")
    # Two group vars, so we don't tie back — plain tibble.
    expect_s3_class(out, "tbl_df")
    expect_false(inherits(out, "daf_axis_tbl"))
    expect_identical(nrow(out), 4L)
})

test_that("group_by(a) preserves second group_by(b, .add = TRUE)", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c("x", "y"))
    set_vector(d, "cell", "b", c("p", "q"))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(a) |>
        dplyr::group_by(b, .add = TRUE)
    expect_identical(attr(out, "groups"), c("a", "b"))
})

# ---- mutate(.keep = "none") ------------------------------------------

test_that("mutate(.keep = 'none') mirrors transmute", {
    skip_if_not_installed("dplyr")
    skip_if(packageVersion("dplyr") < "1.0.0")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c(1, 2))
    set_vector(d, "cell", "b", c(10, 20))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(s = a + b, .keep = "none") |>
        dplyr::collect()
    # With .keep = "none", only newly-created 's' should appear
    # (besides our synthetic 'name').
    expect_true("s" %in% colnames(df))
    expect_false("a" %in% colnames(df))
    expect_false("b" %in% colnames(df))
})
