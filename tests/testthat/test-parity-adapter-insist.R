# Parity fix: adapter()'s copy-back of outputs must use copy_all's default
# insist=TRUE (as Julia's adapter does), so a result that collides with a
# pre-existing base value (overwrite=FALSE) ERRORS rather than being silently
# dropped. dafr hard-coded insist=FALSE, silently discarding the adapter's
# output and keeping the old value (silent data loss). Audit probe
# adapter-insist-collision-scalar.

test_that("adapter copy-back errors on a pre-existing scalar collision (insist)", {
    d <- memory_daf(name = "base")
    set_scalar(d, "kept", 1L)
    fn <- function(adapted) "ok"   # no-op; 'kept' is exposed read-through
    expect_error(
        adapter(d, fn,
                input_data = list(list("kept", "=")),
                output_data = list(list("kept", "=")),
                overwrite = FALSE),
        "existing scalar"
    )
    # base value untouched (the failed copy-back never overwrote it)
    expect_identical(get_scalar(d, "kept"), 1L)
})
