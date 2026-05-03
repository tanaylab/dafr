# Parity port of DataAxesFormats.jl test/queries.jl > vector > lookup
# leaves: as_axis named, if_not chained, if_not missing.

# ---- vector lookup as_axis named ------------------------------------------

test_that(": manual =@ type : color renames the pivot axis", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "type", c("U", "V"))
    set_vector(d, "type", "color", c("red", "green"))
    set_vector(d, "cell", "manual", c("V", "U"))
    expect_equal(
        get_query(d, "@ cell : manual =@ type : color"),
        c(X = "green", Y = "red")
    )
})

# ---- vector lookup if_not chained ------------------------------------------

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B", "C"))
        set_vector(d, "cell", "metacell", c("X", "Y", ""))
        add_axis(d, "metacell", c("X", "Y"))
        set_vector(d, "metacell", "type", c("U", ""))
        add_axis(d, "type", c("U", "V"))
        set_vector(d, "type", "color", c("red", "green"))
        d
    }

    test_that("?? : ?? : drops both empty pivot rows", {
        d <- setup()
        expect_equal(
            get_query(d, "@ cell : metacell ?? : type ?? : color"),
            c(A = "red")
        )
    })

    test_that("?? : ?? <default> : substitutes default at second hop", {
        d <- setup()
        expect_equal(
            get_query(d, "@ cell : metacell ?? : type ?? blue : color"),
            c(A = "red", B = "blue")
        )
    })

    test_that("?? <default> : ?? : substitutes default at first hop", {
        d <- setup()
        expect_equal(
            get_query(d, "@ cell : metacell ?? blue : type ?? : color"),
            c(A = "red", C = "blue")
        )
    })

    test_that("?? <d1> : ?? <d2> : keeps both substitutions for distinct entries", {
        d <- setup()
        expect_equal(
            get_query(d, "@ cell : metacell ?? blue : type ?? magenta : color"),
            c(A = "red", B = "magenta", C = "blue")
        )
    })
})

# ---- vector lookup if_not missing -----------------------------------------

test_that("?? <d> : <missing-vector> || <default> uses IfMissing for missing chain target", {
    # Julia: queries.jl > vector > lookup > if_not > missing
    # `@ metacell : type ?? 0 : phase || 1` → for X (type=U) phase missing,
    # use ||1 → 1; for Y (type="") apply ?? 0 → 0.
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "metacell", c("X", "Y", ""))
    add_axis(d, "metacell", c("X", "Y"))
    set_vector(d, "metacell", "type", c("U", ""))
    add_axis(d, "type", c("U", "V"))
    set_vector(d, "type", "color", c("red", "green"))
    expect_equal(
        get_query(d, "@ metacell : type ?? 0 : phase || 1"),
        c(X = 1L, Y = 0L)
    )
})
