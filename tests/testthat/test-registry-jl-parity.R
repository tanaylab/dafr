# Literal port of registry.jl. 2 leaves.
#
# Julia tests register_query_operation()'s conflicting-registration
# error path. dafr exposes register_query_operation(kind, name, fn) and
# emits the same `conflicting registrations for the <kind> operation:`
# template with `first in: <file>:<line>` / `second in: <file>:<line>`.

test_that("registry / conflicting / eltwise", {
    nm <- "TestEltwiseOp_CR1"
    register_query_operation("eltwise", nm, function(x, ...) x,
                             source = "first.R:6")
    on.exit({
        .ops_env <- get(".ops_env", envir = asNamespace("dafr"))
        .ops_env$eltwise[[nm]] <- NULL
        .ops_env$sources_eltwise[[nm]] <- NULL
    }, add = TRUE)
    expect_error(
        register_query_operation("eltwise", nm, function(x, ...) x,
                                 source = "second.R:-1"),
        regexp = paste0(
            "conflicting registrations for the eltwise operation: ",
            nm, "\\s*\\n*first in: first.R:6\\s*\\n*second in: second.R:-1"
        )
    )
})

test_that("registry / conflicting / reduction", {
    nm <- "TestReductionOp_CR1"
    register_query_operation("reduction", nm, function(x, ...) sum(x),
                             source = "first.R:9")
    on.exit({
        .ops_env <- get(".ops_env", envir = asNamespace("dafr"))
        .ops_env$reductions[[nm]] <- NULL
        .ops_env$sources_reductions[[nm]] <- NULL
    }, add = TRUE)
    expect_error(
        register_query_operation("reduction", nm, function(x, ...) sum(x),
                                 source = "second.R:-1"),
        regexp = paste0(
            "conflicting registrations for the reduction operation: ",
            nm, "\\s*\\n*first in: first.R:9\\s*\\n*second in: second.R:-1"
        )
    )
})
