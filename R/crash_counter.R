# Crash-counter helper for reorder testing. Mirrors upstream
# `DataAxesFormats.jl::Reorder` FLAKY-TESTED machinery
# (`src/reorder.jl::tick_crash_counter!`).
#
# A crash counter is a one-element environment ref: `e$n` is a
# non-negative integer. Each `tick_crash_counter(e)` decrements `e$n`;
# when `e$n` hits zero, throws a `SimulatedCrash` condition. Reorder
# code calls `tick_crash_counter` at each commit-able decision point;
# tests inject `crash_counter = new_crash_counter(N)` to verify
# recovery after a crash at the N-th tick point.

# Internal helpers — not exported.

new_crash_counter <- function(n) {
    e <- new.env(parent = emptyenv())
    e$n <- as.integer(n)
    e
}

tick_crash_counter <- function(crash_counter) {
    if (is.null(crash_counter)) return(invisible())
    crash_counter$n <- crash_counter$n - 1L
    if (crash_counter$n <= 0L) {
        cnd <- structure(
            class = c("SimulatedCrash", "error", "condition"),
            list(message = "simulated crash", call = sys.call())
        )
        stop(cnd)
    }
    invisible()
}
