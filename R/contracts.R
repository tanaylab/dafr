#' @include classes.R format_api.R utils.R
NULL

#' @name expectation-constants
#' @title Contract expectation constants
#' @description String literals used in `contract_scalar()` /
#'   `contract_vector()` / `contract_matrix()` + axis specs.
#' @return Character scalar.
NULL

#' @rdname expectation-constants
#' @export
RequiredInput <- "RequiredInput"
#' @rdname expectation-constants
#' @export
OptionalInput <- "OptionalInput"
#' @rdname expectation-constants
#' @export
CreatedOutput <- "CreatedOutput"
#' @rdname expectation-constants
#' @export
GuaranteedOutput <- "GuaranteedOutput"
#' @rdname expectation-constants
#' @export
OptionalOutput <- "OptionalOutput"

.VALID_EXPECTATIONS <- c(
    RequiredInput, OptionalInput, CreatedOutput, GuaranteedOutput, OptionalOutput
)

.assert_expectation <- function(x, arg) {
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !(x %in% .VALID_EXPECTATIONS)) {
        stop(sprintf(
            "unknown expectation for `%s`: %s",
            arg, if (is.character(x)) sQuote(x) else sQuote(toString(x))
        ), call. = FALSE)
    }
    invisible()
}

.assert_type <- function(type, arg) {
    if (!is.character(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
        stop(sprintf("`%s` must be a non-empty character scalar (R class name)", arg),
            call. = FALSE
        )
    }
    invisible()
}

#' A contract describing a computation's inputs and outputs.
#' @param name Optional name.
#' @param is_relaxed If TRUE, unknown properties don't error.
#' @param axes Named list: axis -> list(expectation, description).
#' @param data List of contract_scalar()/contract_vector()/contract_matrix() records.
#' @export
Contract <- S7::new_class(
    name = "Contract",
    package = "dafr",
    properties = list(
        name       = S7::new_property(S7::class_character, default = ""),
        is_relaxed = S7::new_property(S7::class_logical, default = FALSE),
        axes       = S7::new_property(S7::class_list, default = list()),
        data       = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        for (a in names(self@axes)) {
            spec <- self@axes[[a]]
            if (!is.list(spec) || length(spec) != 2L) {
                return(sprintf("axis %s spec must be list(expectation, description)", a))
            }
            .assert_expectation(spec[[1L]], sprintf("axis %s", a))
            if (!is.character(spec[[2L]]) || length(spec[[2L]]) != 1L) {
                return(sprintf("axis %s description must be character scalar", a))
            }
        }
        for (i in seq_along(self@data)) {
            rec <- self@data[[i]]
            if (!is.list(rec) || !("kind" %in% names(rec))) {
                return(sprintf("data[[%d]] must be a record with $kind", i))
            }
            .assert_expectation(rec$expectation, sprintf("data[[%d]] expectation", i))
            .assert_type(rec$type, sprintf("data[[%d]] type", i))
        }
        NULL
    }
)

#' @export
contract_scalar <- function(name, expectation, type, description) {
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    list(
        kind        = "scalar", name = name, expectation = expectation,
        type        = type, description = description
    )
}

#' @export
contract_vector <- function(axis, name, expectation, type, description) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    list(
        kind = "vector", axis = axis, name = name,
        expectation = expectation, type = type, description = description
    )
}

#' @export
contract_matrix <- function(rows_axis, columns_axis, name, expectation, type, description) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    list(
        kind = "matrix", rows_axis = rows_axis, columns_axis = columns_axis,
        name = name, expectation = expectation, type = type,
        description = description
    )
}

#' @export
ContractDaf <- S7::new_class(
    name = "ContractDaf",
    package = "dafr",
    parent = DafWriter,
    properties = list(
        computation = S7::class_character,
        is_relaxed  = S7::class_logical,
        overwrite   = S7::class_logical,
        base        = DafReader,
        axes        = S7::class_environment,   # env(axis -> tracker env)
        data        = S7::class_environment    # env(key -> tracker env)
    )
)

.enforcement_enabled <- function() {
    opt <- getOption("dafr.enforce_contracts", NULL)
    if (!is.null(opt)) return(isTRUE(opt))
    env <- Sys.getenv("DAF_ENFORCE_CONTRACTS", unset = NA_character_)
    if (is.na(env)) return(FALSE)
    tolower(env) %in% c("1", "true", "t", "yes", "y")
}

.new_tracker <- function(expectation, type = NA_character_) {
    t <- new.env(parent = emptyenv())
    t$expectation <- expectation
    t$type <- type
    t$accessed <- FALSE
    t
}

.data_key <- function(rec) {
    switch(rec$kind,
        scalar = sprintf("scalar:%s", rec$name),
        vector = sprintf("vector:%s:%s", rec$axis, rec$name),
        matrix = sprintf("matrix:%s:%s:%s", rec$rows_axis, rec$columns_axis, rec$name),
        stop("unknown data kind")
    )
}

#' @export
contractor <- function(computation, contract, daf,
                       name = NULL, overwrite = FALSE) {
    if (!.enforcement_enabled()) {
        return(daf)
    }
    if (!S7::S7_inherits(daf, DafReader)) {
        stop("`daf` must be a DafReader", call. = FALSE)
    }
    if (is.null(name)) {
        name <- paste0(S7::prop(daf, "name"), ".", computation)
    }
    axes_env <- new.env(parent = emptyenv())
    for (ax in names(S7::prop(contract, "axes"))) {
        spec <- S7::prop(contract, "axes")[[ax]]
        axes_env[[ax]] <- .new_tracker(spec[[1L]])
    }
    data_env <- new.env(parent = emptyenv())
    for (rec in S7::prop(contract, "data")) {
        data_env[[.data_key(rec)]] <- .new_tracker(rec$expectation, rec$type)
    }
    ContractDaf(
        name = name,
        internal = new_internal_env(),
        cache = S7::prop(daf, "cache"),
        axis_version_counter = S7::prop(daf, "axis_version_counter"),
        vector_version_counter = S7::prop(daf, "vector_version_counter"),
        matrix_version_counter = S7::prop(daf, "matrix_version_counter"),
        computation = computation,
        is_relaxed = isTRUE(S7::prop(contract, "is_relaxed")),
        overwrite = overwrite,
        base = daf,
        axes = axes_env,
        data = data_env
    )
}
