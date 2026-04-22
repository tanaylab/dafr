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

#' Contract entry constructors
#'
#' Build records describing a scalar/vector/matrix that a computation
#' consumes or produces. Records are stored in the `data` slot of a
#' [Contract()] and consumed by [contractor()].
#'
#' @param name Property name (character scalar).
#' @param axis Axis name for a vector property.
#' @param rows_axis,columns_axis Axis names for a matrix property.
#' @param expectation One of the [expectation-constants] (e.g.
#'   `RequiredInput`, `CreatedOutput`).
#' @param type R class name (e.g. `"integer"`, `"numeric"`, `"character"`).
#' @param description Free-text description (character scalar).
#' @return A list record with `$kind`, `$name`, `$expectation`, `$type`,
#'   `$description`, and kind-specific axis fields.
#' @name contract-entries
#' @examples
#' s <- contract_scalar("organism", RequiredInput, "character", "species name")
#' v <- contract_vector("cell", "donor", RequiredInput, "character", "donor id")
#' m <- contract_matrix("cell", "gene", "UMIs", RequiredInput, "integer", "UMI counts")
#' # These records go into Contract(data = ...):
#' c <- Contract(
#'     axes = list(cell = list(RequiredInput, "per-cell axis"),
#'                 gene = list(RequiredInput, "per-gene axis")),
#'     data = list(s, v, m)
#' )
#' length(c@data)
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

#' @rdname contract-entries
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

#' @rdname contract-entries
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

#' Contract-enforcing daf wrapper
#'
#' S7 class returned by [contractor()] when enforcement is enabled.
#' Intercepts `format_*` reads/writes on an underlying [DafReader]
#' to track access and reject operations outside the contract.
#'
#' @inheritParams DafReader
#' @param computation Name of the computation being guarded.
#' @param is_relaxed If `TRUE`, accesses to properties outside the
#'   contract are allowed (matching `Contract(is_relaxed = TRUE)`).
#' @param overwrite If `TRUE`, pre-existing `CreatedOutput` properties
#'   are allowed.
#' @param base Underlying `DafReader` / `DafWriter`.
#' @param axes Per-axis access-tracking environment (keyed by axis
#'   name).
#' @param data Per-property access-tracking environment (keyed by
#'   `<kind>:<axes>:<name>`).
#' @return An S7 class object; users normally obtain instances via
#'   [contractor()] rather than calling the constructor directly.
#' @seealso [contractor()], [verify_input()], [verify_output()].
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

#' Wrap a daf with a contract for a computation
#'
#' When contract enforcement is enabled (env `DAF_ENFORCE_CONTRACTS=1`
#' or `options(dafr.enforce_contracts = TRUE)`), returns a
#' [ContractDaf] that tracks access and rejects operations outside
#' `contract`. Otherwise returns `daf` unchanged.
#'
#' @param computation Name of the computation (character scalar).
#' @param contract A [Contract()] describing expected inputs and outputs.
#' @param daf The underlying [DafReader] / [DafWriter].
#' @param name Optional name for the wrapper; defaults to
#'   `<daf-name>.<computation>`.
#' @param overwrite If `TRUE`, pre-existing `CreatedOutput` properties
#'   are allowed.
#' @return `daf` itself, or a [ContractDaf] wrapping it.
#' @examples
#' withr::with_options(list(dafr.enforce_contracts = TRUE), {
#'     c <- Contract(
#'         axes = list(cell = list(RequiredInput, "per-cell axis")),
#'         data = list(contract_vector("cell", "donor",
#'             RequiredInput, "character", "donor id"))
#'     )
#'     d <- memory_daf()
#'     add_axis(d, "cell", c("c1", "c2"))
#'     set_vector(d, "cell", "donor", c("d1", "d2"))
#'     cd <- contractor("demo", c, d)
#'     class(cd)[[1]]
#' })
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

# -- verify_input / verify_output ---------------------------------------

.is_mandatory <- function(expectation, is_for_output) {
    (is_for_output && expectation == CreatedOutput) ||
        (!is_for_output && expectation == RequiredInput)
}

.is_forbidden <- function(expectation, is_for_output, overwrite) {
    !is_for_output && expectation == CreatedOutput && !overwrite
}

.direction_name <- function(is_for_output) if (is_for_output) "output" else "input"

.type_ok <- function(value, type_name) {
    switch(type_name,
        integer   = is.integer(value),
        numeric   = is.numeric(value),
        double    = is.double(value),
        character = is.character(value),
        logical   = is.logical(value),
        # fall back to class check for user-defined types
        inherits(value, type_name)
    )
}

.vector_type_ok <- function(v, type_name) {
    switch(type_name,
        integer   = is.integer(v),
        numeric   = is.numeric(v),
        double    = is.double(v),
        character = is.character(v),
        logical   = is.logical(v),
        inherits(v, type_name)
    )
}

.matrix_type_ok <- function(m, type_name) {
    switch(type_name,
        integer   = .is_integer_valued(m),
        numeric   = is.numeric(m[1L]),
        double    = is.double(m[1L]),
        logical   = is.logical(m[1L]) || .is_logical_valued_sparse(m),
        character = is.character(m[1L]),
        inherits(m, type_name)
    )
}

.is_integer_valued <- function(m) {
    if (!is.null(dim(m)) && length(m) == 0L) return(TRUE)
    if (is.integer(m[1L])) return(TRUE)
    if (methods::is(m, "dgCMatrix")) {
        if (length(m@x) == 0L) return(TRUE)
        return(all(m@x == floor(m@x)) &&
               max(abs(m@x)) < .Machine$integer.max)
    }
    FALSE
}

.is_logical_valued_sparse <- function(m) {
    methods::is(m, "dgCMatrix") &&
        (length(m@x) == 0L || all(m@x %in% c(0, 1)))
}

.verify_scalar_data <- function(cd, rec, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    name <- rec$name
    exists_ <- format_has_scalar(base, name)
    tracker <- S7::prop(cd, "data")[[.data_key(rec)]]
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s scalar: %s with type: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), name, tracker$type, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s scalar: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, comp, dname
        ), call. = FALSE)
    }
    value <- format_get_scalar(base, name)
    if (!.type_ok(value, tracker$type)) {
        stop(sprintf(
            "unexpected type: %s instead of type: %s for the %s scalar: %s for the computation: %s on the daf data: %s",
            class(value)[[1L]], tracker$type,
            .direction_name(is_for_output), name, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_vector_data <- function(cd, rec, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    axis <- rec$axis; name <- rec$name
    tracker <- S7::prop(cd, "data")[[.data_key(rec)]]
    exists_ <- format_has_axis(base, axis) && format_has_vector(base, axis, name)
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s vector: %s of the axis: %s with element type: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), name, axis, tracker$type, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, axis, comp, dname
        ), call. = FALSE)
    }
    v <- format_get_vector(base, axis, name)
    if (!.vector_type_ok(v, tracker$type)) {
        stop(sprintf(
            "unexpected type: %s instead of type: %s for the %s vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            class(v)[[1L]], tracker$type,
            .direction_name(is_for_output), name, axis, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_matrix_data <- function(cd, rec, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    ra <- rec$rows_axis; ca <- rec$columns_axis; name <- rec$name
    tracker <- S7::prop(cd, "data")[[.data_key(rec)]]
    exists_ <- format_has_axis(base, ra) && format_has_axis(base, ca) &&
        format_has_matrix(base, ra, ca, name)
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s matrix: %s of the rows axis: %s and the columns axis: %s with element type: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), name, ra, ca, tracker$type, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s matrix: %s of the rows axis: %s and the columns axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, ra, ca, comp, dname
        ), call. = FALSE)
    }
    m <- format_get_matrix(base, ra, ca, name)
    if (!.matrix_type_ok(m, tracker$type)) {
        stop(sprintf(
            "unexpected type: %s instead of type: %s for the %s matrix: %s of the rows axis: %s and the columns axis: %s for the computation: %s on the daf data: %s",
            class(m)[[1L]], tracker$type,
            .direction_name(is_for_output), name, ra, ca, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_axis_data <- function(cd, axis, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    tracker <- S7::prop(cd, "axes")[[axis]]
    exists_ <- format_has_axis(base, axis)
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s axis: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), axis, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, axis, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_access <- function(cd) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    for (ax in ls(S7::prop(cd, "axes"), all.names = TRUE)) {
        tracker <- S7::prop(cd, "axes")[[ax]]
        if (format_has_axis(base, ax) && !tracker$accessed &&
            identical(tracker$expectation, RequiredInput)) {
            stop(sprintf(
                "unused RequiredInput axis: %s of the computation: %s on the daf data: %s",
                ax, comp, dname
            ), call. = FALSE)
        }
    }
    for (key in ls(S7::prop(cd, "data"), all.names = TRUE)) {
        tracker <- S7::prop(cd, "data")[[key]]
        if (!identical(tracker$expectation, RequiredInput) || tracker$accessed) next
        parts <- strsplit(key, ":", fixed = TRUE)[[1L]]
        kind <- parts[[1L]]
        if (kind == "scalar") {
            if (format_has_scalar(base, parts[[2L]])) {
                stop(sprintf(
                    "unused RequiredInput scalar: %s of the computation: %s on the daf data: %s",
                    parts[[2L]], comp, dname
                ), call. = FALSE)
            }
        } else if (kind == "vector") {
            if (format_has_axis(base, parts[[2L]]) &&
                format_has_vector(base, parts[[2L]], parts[[3L]])) {
                stop(sprintf(
                    "unused RequiredInput vector: %s of the axis: %s of the computation: %s on the daf data: %s",
                    parts[[3L]], parts[[2L]], comp, dname
                ), call. = FALSE)
            }
        } else if (kind == "matrix") {
            if (format_has_axis(base, parts[[2L]]) &&
                format_has_axis(base, parts[[3L]]) &&
                format_has_matrix(base, parts[[2L]], parts[[3L]], parts[[4L]])) {
                stop(sprintf(
                    "unused RequiredInput matrix: %s of the rows axis: %s and the columns axis: %s of the computation: %s on the daf data: %s",
                    parts[[4L]], parts[[2L]], parts[[3L]], comp, dname
                ), call. = FALSE)
            }
        }
    }
    invisible()
}

.verify_contract <- function(cd, is_for_output) {
    for (ax in ls(S7::prop(cd, "axes"), all.names = TRUE)) {
        .verify_axis_data(cd, ax, is_for_output)
    }
    for (key in ls(S7::prop(cd, "data"), all.names = TRUE)) {
        # Reconstruct the original record shape for verify_* helpers.
        parts <- strsplit(key, ":", fixed = TRUE)[[1L]]
        tracker <- S7::prop(cd, "data")[[key]]
        rec <- switch(parts[[1L]],
            scalar = list(kind = "scalar", name = parts[[2L]],
                          expectation = tracker$expectation, type = tracker$type),
            vector = list(kind = "vector", axis = parts[[2L]], name = parts[[3L]],
                          expectation = tracker$expectation, type = tracker$type),
            matrix = list(kind = "matrix", rows_axis = parts[[2L]],
                          columns_axis = parts[[3L]], name = parts[[4L]],
                          expectation = tracker$expectation, type = tracker$type)
        )
        switch(rec$kind,
            scalar = .verify_scalar_data(cd, rec, is_for_output),
            vector = .verify_vector_data(cd, rec, is_for_output),
            matrix = .verify_matrix_data(cd, rec, is_for_output)
        )
    }
    if (is_for_output) .verify_access(cd)
    invisible()
}

#' Verify contract inputs / outputs
#'
#' `verify_input()` should be called before running a computation and
#' `verify_output()` after. Both are no-ops when `daf` is not a
#' [ContractDaf] (i.e. enforcement is disabled). `verify_output()`
#' additionally fails on unused `RequiredInput` properties.
#'
#' @param daf A [DafReader] — if it is a [ContractDaf], the associated
#'   contract is verified; otherwise the call is a silent no-op.
#' @return `invisible()`.
#' @examples
#' withr::with_options(list(dafr.enforce_contracts = TRUE), {
#'     c <- Contract(
#'         axes = list(cell = list(RequiredInput, "per-cell axis")),
#'         data = list(contract_vector("cell", "donor",
#'             RequiredInput, "character", "donor id"))
#'     )
#'     d <- memory_daf()
#'     add_axis(d, "cell", c("c1", "c2"))
#'     set_vector(d, "cell", "donor", c("d1", "d2"))
#'     cd <- contractor("demo", c, d)
#'     verify_input(cd)
#'     get_vector(cd, "cell", "donor")
#'     verify_output(cd)
#' })
#' @export
verify_input <- function(daf) {
    if (!S7::S7_inherits(daf, ContractDaf)) return(invisible())
    .verify_contract(daf, is_for_output = FALSE)
}

#' @rdname verify_input
#' @export
verify_output <- function(daf) {
    if (!S7::S7_inherits(daf, ContractDaf)) return(invisible())
    .verify_contract(daf, is_for_output = TRUE)
}

.merge_expectations <- function(what, key, left, right) {
    if (identical(left, RequiredInput) && right %in% c(RequiredInput, OptionalInput)) {
        return(RequiredInput)
    }
    if (identical(left, OptionalInput) && right %in% c(RequiredInput, OptionalInput)) {
        return(right)
    }
    if (identical(left, CreatedOutput) && right %in% c(RequiredInput, OptionalInput)) {
        return(CreatedOutput)
    }
    if (identical(left, GuaranteedOutput) && right %in% c(RequiredInput, OptionalInput)) {
        return(GuaranteedOutput)
    }
    if (identical(left, OptionalOutput) && identical(right, OptionalInput)) {
        return(OptionalOutput)
    }
    stop(sprintf(
        "incompatible expectation: %s and expectation: %s for the contracts %s: %s",
        left, right, what, key
    ), call. = FALSE)
}

.TYPE_WIDTH_ORDER <- c("logical", "integer", "double", "numeric", "character")

.merge_types <- function(key, left, right) {
    if (identical(left, right)) return(left)
    li <- match(left,  .TYPE_WIDTH_ORDER, nomatch = NA_integer_)
    ri <- match(right, .TYPE_WIDTH_ORDER, nomatch = NA_integer_)
    if (is.na(li) || is.na(ri)) {
        stop(sprintf(
            "incompatible type: %s and type: %s for the contracts data: %s",
            left, right, key
        ), call. = FALSE)
    }
    # Prefer the narrower type (smaller index).
    .TYPE_WIDTH_ORDER[[min(li, ri)]]
}

#' Merge two contracts.
#'
#' Mirrors Julia's `Contract |> Contract`. `left` is treated as the
#' earlier stage (upstream) and `right` as the later stage
#' (downstream); expectations and element types are resolved
#' accordingly and descriptions must match for overlapping axes /
#' properties.
#'
#' @param left,right Two [Contract()] objects to merge.
#' @return A new [Contract()] combining `left` and `right`.
#' @examples
#' upstream <- Contract(
#'     axes = list(cell = list(RequiredInput, "per-cell axis")),
#'     data = list(contract_vector("cell", "donor",
#'         RequiredInput, "character", "donor id"))
#' )
#' downstream <- Contract(
#'     axes = list(cell = list(RequiredInput, "per-cell axis")),
#'     data = list(contract_vector("cell", "score",
#'         CreatedOutput, "numeric", "computed score"))
#' )
#' merged <- merge_contracts(upstream, downstream)
#' length(merged@data)
#' @export
merge_contracts <- function(left, right) {
    merged_name <- if (identical(left@name, "")) right@name else
        if (identical(right@name, "")) left@name else
            paste0(left@name, "_", right@name)
    merged_axes <- left@axes
    for (a in names(right@axes)) {
        spec_r <- right@axes[[a]]
        spec_l <- merged_axes[[a]]
        if (is.null(spec_l)) {
            merged_axes[[a]] <- spec_r
        } else {
            if (!identical(spec_l[[2L]], spec_r[[2L]])) {
                stop(sprintf("different description for the axis: %s", a), call. = FALSE)
            }
            merged_axes[[a]] <- list(
                .merge_expectations("axis", a, spec_l[[1L]], spec_r[[1L]]),
                spec_l[[2L]]
            )
        }
    }
    # Data: match by .data_key()
    merged_data <- list()
    keys_l <- vapply(left@data, .data_key, character(1))
    keys_r <- vapply(right@data, .data_key, character(1))
    for (i in seq_along(left@data)) {
        rec_l <- left@data[[i]]
        j <- match(keys_l[[i]], keys_r)
        if (is.na(j)) {
            merged_data <- c(merged_data, list(rec_l))
        } else {
            rec_r <- right@data[[j]]
            if (!identical(rec_l$description, rec_r$description)) {
                stop(sprintf("different description for the data: %s",
                    keys_l[[i]]
                ), call. = FALSE)
            }
            merged <- rec_l
            merged$expectation <- .merge_expectations(
                "data", keys_l[[i]], rec_l$expectation, rec_r$expectation
            )
            merged$type <- .merge_types(keys_l[[i]], rec_l$type, rec_r$type)
            merged_data <- c(merged_data, list(merged))
        }
    }
    for (j in seq_along(right@data)) {
        if (!(keys_r[[j]] %in% keys_l)) {
            merged_data <- c(merged_data, list(right@data[[j]]))
        }
    }
    Contract(
        name = merged_name,
        is_relaxed = left@is_relaxed || right@is_relaxed,
        axes = merged_axes,
        data = merged_data
    )
}
