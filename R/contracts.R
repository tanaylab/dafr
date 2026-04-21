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

# -- Access tracking helpers --------------------------------------------

.access_key_scalar <- function(name) sprintf("scalar:%s", name)
.access_key_vector <- function(axis, name) sprintf("vector:%s:%s", axis, name)
.access_key_matrix <- function(ra, ca, name) sprintf("matrix:%s:%s:%s", ra, ca, name)

.IMMUTABLE_EXPECTATIONS <- c(RequiredInput, OptionalInput)

.is_immutable <- function(expectation, is_for_modify) {
    is_for_modify && expectation %in% .IMMUTABLE_EXPECTATIONS
}

.access_scalar <- function(cd, name, is_for_modify) {
    key <- .access_key_scalar(name)
    tracker <- S7::prop(cd, "data")[[key]]
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed"))) return(invisible())
        stop(sprintf(
            "accessing non-contract scalar: %s for the computation: %s on the daf data: %s",
            name, S7::prop(cd, "computation"), S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s scalar: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

.access_axis <- function(cd, axis, is_for_modify) {
    tracker <- S7::prop(cd, "axes")[[axis]]
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed"))) return(invisible())
        stop(sprintf(
            "accessing non-contract axis: %s for the computation: %s on the daf data: %s",
            axis, S7::prop(cd, "computation"), S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, axis, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

.access_vector <- function(cd, axis, name, is_for_modify) {
    # Axis access is non-modifying even on a vector write (Julia semantics).
    .access_axis(cd, axis, FALSE)
    key <- .access_key_vector(axis, name)
    tracker <- S7::prop(cd, "data")[[key]]
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed")) || name %in% c("name", "index")) {
            return(invisible())
        }
        stop(sprintf(
            "accessing non-contract vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            name, axis, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, axis, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

.access_matrix <- function(cd, ra, ca, name, is_for_modify) {
    .access_axis(cd, ra, FALSE)
    .access_axis(cd, ca, FALSE)
    key <- .access_key_matrix(ra, ca, name)
    tracker <- S7::prop(cd, "data")[[key]]
    if (is.null(tracker)) {
        # Try flipped
        tracker <- S7::prop(cd, "data")[[.access_key_matrix(ca, ra, name)]]
    }
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed"))) return(invisible())
        stop(sprintf(
            "accessing non-contract matrix: %s of the rows axis: %s and the columns axis: %s for the computation: %s on the daf data: %s",
            name, ra, ca, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s matrix: %s of the rows_axis: %s and the columns_axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, ra, ca, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

# -- format_* dispatch --------------------------------------------------

S7::method(
    format_has_scalar,
    list(ContractDaf, S7::class_character)
) <- function(daf, name) format_has_scalar(S7::prop(daf, "base"), name)

S7::method(
    format_get_scalar,
    list(ContractDaf, S7::class_character)
) <- function(daf, name) {
    .access_scalar(daf, name, is_for_modify = FALSE)
    format_get_scalar(S7::prop(daf, "base"), name)
}

S7::method(format_scalars_set, ContractDaf) <- function(daf) {
    format_scalars_set(S7::prop(daf, "base"))
}

S7::method(
    format_set_scalar,
    list(ContractDaf, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    .access_scalar(daf, name, is_for_modify = TRUE)
    format_set_scalar(S7::prop(daf, "base"), name, value, overwrite)
}

S7::method(
    format_delete_scalar,
    list(ContractDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    .access_scalar(daf, name, is_for_modify = TRUE)
    format_delete_scalar(S7::prop(daf, "base"), name, must_exist)
}

S7::method(
    format_has_axis,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_has_axis(S7::prop(daf, "base"), axis)

S7::method(format_axes_set, ContractDaf) <- function(daf) {
    format_axes_set(S7::prop(daf, "base"))
}

S7::method(
    format_axis_array,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) {
    .access_axis(daf, axis, is_for_modify = FALSE)
    format_axis_array(S7::prop(daf, "base"), axis)
}

S7::method(
    format_axis_length,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_axis_length(S7::prop(daf, "base"), axis)

S7::method(
    format_axis_dict,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_axis_dict(S7::prop(daf, "base"), axis)

S7::method(
    format_add_axis,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, entries) {
    .access_axis(daf, axis, is_for_modify = TRUE)
    format_add_axis(S7::prop(daf, "base"), axis, entries)
}

S7::method(
    format_delete_axis,
    list(ContractDaf, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    .access_axis(daf, axis, is_for_modify = TRUE)
    format_delete_axis(S7::prop(daf, "base"), axis, must_exist)
}

S7::method(
    format_has_vector,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) format_has_vector(S7::prop(daf, "base"), axis, name)

S7::method(
    format_get_vector,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    .access_vector(daf, axis, name, is_for_modify = FALSE)
    format_get_vector(S7::prop(daf, "base"), axis, name)
}

S7::method(
    format_vectors_set,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_vectors_set(S7::prop(daf, "base"), axis)

S7::method(
    format_set_vector,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, axis, name, vec, overwrite) {
    .access_vector(daf, axis, name, is_for_modify = TRUE)
    format_set_vector(S7::prop(daf, "base"), axis, name, vec, overwrite)
}

S7::method(
    format_delete_vector,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, axis, name, must_exist) {
    .access_vector(daf, axis, name, is_for_modify = TRUE)
    format_delete_vector(S7::prop(daf, "base"), axis, name, must_exist)
}

S7::method(
    format_has_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    format_has_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name)
}

S7::method(
    format_get_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = FALSE)
    format_get_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name)
}

S7::method(
    format_matrices_set,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    format_matrices_set(S7::prop(daf, "base"), rows_axis, columns_axis)
}

S7::method(
    format_set_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = TRUE)
    format_set_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name, mat, overwrite)
}

S7::method(
    format_delete_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, must_exist) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = TRUE)
    format_delete_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name, must_exist)
}

S7::method(
    format_relayout_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = FALSE)
    format_relayout_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name)
}
