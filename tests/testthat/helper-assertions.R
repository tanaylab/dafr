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
#      as.matrix is a base S3 generic (UseMethod). We intercept by directly
#      replacing the binding in Matrix's namespace AND the cached entry in
#      base's .__S3MethodsTable__. (both are consulted by UseMethod). We do
#      NOT use assignInNamespace() because in R 4.5 + Matrix 1.7-4 its S3-
#      remap code path calls `methods::slot(genfun, "default")@methods$ANY`,
#      which fails with "no slot of name 'methods' for this object of class
#      'derivedDefaultMethod'" — an upstream utils/methods interaction bug.
#
#      In addition, kernel code using the namespace-qualified form
#      Matrix::as.matrix(m) bypasses the S3 slot because Matrix::as.matrix
#      is a separate S4 standardGeneric dispatched inside the Matrix namespace.
#      To close this gap we ALSO trace the S4 method table entry inside the
#      Matrix namespace, using a shared depth counter to handle reentrant
#      calls (see reentrance notes below).
#
#   Reentrance: assert_no_densify_during may be called inside itself (nested).
#   To avoid counter collisions, each call uses a process-unique AND call-unique
#   counter name. To avoid S4 trace slot collisions (the namespace slot can
#   only hold one tracer at a time), the S3 branch uses a global depth counter
#   (.__dafr_s4_trace_depth__) and a global counter-name registry
#   (.__dafr_s4_ctr_stack__). The S4 slot is traced only when depth goes
#   0 -> 1, and untraced only when depth goes 1 -> 0. A single tracer
#   increments ALL currently-active call counters on each as.matrix invocation.
#
#   The tracer cannot reference local variables (trace() evaluates in a
#   different frame), so we use a process-unique global counter updated via
#   bquote() and globalenv().
assert_no_densify_during <- function(expr) {
    env <- parent.frame()
    requireNamespace("Matrix", quietly = TRUE)

    # Make counter name unique per-process AND per-call to prevent reentrance
    # counter collisions when assert_no_densify_during is nested.
    counter_name <- paste0(
        ".__dafr_densify_count_",
        Sys.getpid(), "_",
        format(as.numeric(Sys.time()) * 1e6, scientific = FALSE),
        "_", sample.int(.Machine$integer.max, 1L),
        "__."
    )
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
        # S3 context: replace the namespace binding AND the base S3 methods
        # table entry directly. We bypass assignInNamespace() because its S3
        # remap path is broken under newer R/methods (see header comment).
        mat_ns <- getNamespace("Matrix")
        s3_table <- getNamespace("base")$.__S3MethodsTable__.
        orig_s3 <- get("as.matrix.Matrix", envir = mat_ns)
        has_tbl <- exists("as.matrix.Matrix", envir = s3_table,
                          inherits = FALSE)
        orig_tbl <- if (has_tbl) s3_table$as.matrix.Matrix else NULL
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
        unlockBinding("as.matrix.Matrix", mat_ns)
        assign("as.matrix.Matrix", patched, envir = mat_ns)
        lockBinding("as.matrix.Matrix", mat_ns)
        if (has_tbl) {
            assign("as.matrix.Matrix", patched, envir = s3_table)
        }
        on.exit({
            unlockBinding("as.matrix.Matrix", mat_ns)
            assign("as.matrix.Matrix", orig_s3, envir = mat_ns)
            lockBinding("as.matrix.Matrix", mat_ns)
            if (has_tbl) {
                assign("as.matrix.Matrix", orig_tbl, envir = s3_table)
            }
            rm(list = counter_name, envir = globalenv())
        }, add = TRUE)

        # Additionally intercept Matrix::as.matrix(m) (namespace-qualified),
        # which dispatches through the S4 standardGeneric inside the Matrix
        # namespace and bypasses the S3 slot patched above.
        #
        # Reentrance guard: only trace/untrace the S4 slot when nesting depth
        # transitions between 0 and 1.  A single shared tracer reads the live
        # stack of active counter names and increments all of them.
        depth_name <- ".__dafr_s4_trace_depth__."
        stack_name <- ".__dafr_s4_ctr_stack__."
        if (!exists(depth_name, envir = globalenv(), inherits = FALSE)) {
            assign(depth_name, 0L, envir = globalenv())
        }
        if (!exists(stack_name, envir = globalenv(), inherits = FALSE)) {
            assign(stack_name, character(0), envir = globalenv())
        }

        # Push this call's counter onto the shared stack.
        assign(stack_name,
               c(get(stack_name, envir = globalenv()), counter_name),
               envir = globalenv())

        cur_depth <- get(depth_name, envir = globalenv())
        assign(depth_name, cur_depth + 1L, envir = globalenv())

        if (cur_depth == 0L) {
            # Outermost call: install the shared S4 tracer.
            shared_tracer <- bquote({
                for (.__nm. in get(.(stack_name), envir = globalenv())) {
                    assign(.__nm.,
                           get(.__nm., envir = globalenv()) + 1L,
                           envir = globalenv())
                }
            })
            suppressMessages(
                trace("as.matrix", signature = "Matrix",
                      tracer = shared_tracer, print = FALSE,
                      where = asNamespace("Matrix"))
            )
        }

        on.exit({
            new_depth <- get(depth_name, envir = globalenv()) - 1L
            assign(depth_name, new_depth, envir = globalenv())
            # Pop this call's counter from the stack.
            stk <- get(stack_name, envir = globalenv())
            assign(stack_name, stk[stk != counter_name], envir = globalenv())
            # Innermost exit: remove the shared S4 tracer.
            if (new_depth == 0L) {
                suppressMessages(
                    untrace("as.matrix", signature = "Matrix",
                            where = asNamespace("Matrix"))
                )
            }
        }, add = TRUE)
    }

    result <- eval(expr, envir = env)
    calls <- get(counter_name, envir = globalenv())
    testthat::expect_equal(calls, 0L,
        info = "as.matrix was called during expression — sparse path densified"
    )
    result
}
