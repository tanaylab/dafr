# tests/testthat/helper-assertions.R
# Assert that an expression does not call `as.matrix` on any object.
# Used to verify sparse fast paths do not densify their input.
#
# Implementation notes vs the plan spec:
#   The spec traced base::as.matrix, but two dispatch paths exist and the
#   S3 dispatch system caches method lookups, requiring different strategies:
#
#   1. When the Matrix package IS on the search path (interactive sessions,
#      standalone Rscript with library(Matrix)), as.matrix is an S4
#      standardGeneric. We use trace("as.matrix", signature = "Matrix").
#      trace() calls setMethod() internally, which invalidates the S4
#      dispatch cache, so the traced method fires reliably.
#
#   2. When Matrix is imported but not attached (e.g., inside devtools::test),
#      as.matrix is a base S3 generic (UseMethod). The S3 dispatch cache
#      can hold a reference to the pre-trace as.matrix.Matrix function
#      making trace() unreliable after cache warm. We replace the binding
#      directly via assignInNamespace(), which always bypasses the cache.
#
#   The tracer cannot reference a local variable by name (trace() evaluates
#   it in a different frame), so we use a process-unique global counter
#   updated via bquote() and globalenv().
assert_no_densify_during <- function(expr) {
    env <- parent.frame()
    requireNamespace("Matrix", quietly = TRUE)
    counter_name <- paste0(".__dafr_densify_count_", Sys.getpid(), "__.")
    assign(counter_name, 0L, envir = globalenv())

    if ("package:Matrix" %in% search()) {
        # S4 context: trace() + setMethod() invalidates the S4 dispatch cache.
        tracer_expr <- bquote(
            assign(.(counter_name),
                   get(.(counter_name), envir = globalenv()) + 1L,
                   envir = globalenv())
        )
        suppressMessages(
            trace("as.matrix", signature = "Matrix",
                  tracer = tracer_expr, print = FALSE)
        )
        on.exit({
            suppressMessages(untrace("as.matrix", signature = "Matrix"))
            rm(list = counter_name, envir = globalenv())
        }, add = TRUE)
    } else {
        # S3 context: replace the binding directly to bypass the S3 cache.
        mat_ns <- getNamespace("Matrix")
        orig_s3 <- get("as.matrix.Matrix", envir = mat_ns)
        patched_body <- bquote({
            assign(.(counter_name),
                   get(.(counter_name), envir = globalenv()) + 1L,
                   envir = globalenv())
            as(x, "matrix")
        })
        patched <- function(x, ...) {}
        body(patched) <- patched_body
        formals(patched) <- formals(orig_s3)
        environment(patched) <- environment(orig_s3)
        assignInNamespace("as.matrix.Matrix", patched, ns = "Matrix")
        on.exit({
            assignInNamespace("as.matrix.Matrix", orig_s3, ns = "Matrix")
            rm(list = counter_name, envir = globalenv())
        }, add = TRUE)
    }

    result <- eval(expr, envir = env)
    calls <- get(counter_name, envir = globalenv())
    testthat::expect_equal(calls, 0L,
        info = "as.matrix was called during expression — sparse path densified"
    )
    result
}
