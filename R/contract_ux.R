#' @include classes.R contracts.R
NULL

# ---- axis_contract, tensor_contract record constructors --------------------

#' Axis-contract record.
#'
#' Builds an axis specification record for use in
#' [create_contract()]'s `axes` argument.
#'
#' @param name Axis name.
#' @param expectation One of the [expectation-constants] (e.g.
#'   [RequiredInput], [CreatedOutput]).
#' @param description Free-text description (character scalar).
#' @return A list record with class `"dafr_axis_contract"`.
#' @examples
#' axis_contract("cell", RequiredInput, "per-cell axis")
#' @seealso [create_contract()], [tensor_contract()], [expectation-constants]
#' @export
axis_contract <- function(name, expectation, description) {
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    if (!is.character(description) || length(description) != 1L) {
        stop("`description` must be a character scalar", call. = FALSE)
    }
    structure(
        list(
            kind = "axis",
            name = name,
            expectation = expectation,
            description = description
        ),
        class = "dafr_axis_contract"
    )
}

#' Tensor-contract record.
#'
#' Builds a tensor specification record for use in
#' [create_contract()]'s `tensors` argument. A tensor is a 3-D
#' structure stored as per-main-axis-entry matrices named
#' `<entry>_<name>` on `(rows_axis, columns_axis)`.
#'
#' @param main_axis Axis whose entries index the tensor slices.
#' @param rows_axis,columns_axis Axis names for each per-entry matrix.
#' @param name Tensor name; individual matrices will be
#'   `<main_axis_entry>_<name>`.
#' @param expectation One of the [expectation-constants].
#' @param type R class name of the matrix values
#'   (e.g. `"integer"`, `"numeric"`).
#' @param description Free-text description.
#' @return A list record with `$kind = "tensor"`.
#' @examples
#' tensor_contract("batch", "cell", "gene", "UMIs",
#'     RequiredInput, "integer", "per-batch UMI matrices")
#' @seealso [create_contract()], [axis_contract()]
#' @export
tensor_contract <- function(main_axis, rows_axis, columns_axis, name,
                            expectation, type, description) {
    .assert_name(main_axis, "main_axis")
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    if (!is.character(description) || length(description) != 1L) {
        stop("`description` must be a character scalar", call. = FALSE)
    }
    list(
        kind = "tensor",
        main_axis = main_axis,
        rows_axis = rows_axis,
        columns_axis = columns_axis,
        name = name,
        expectation = expectation,
        type = type,
        description = description
    )
}

# ---- create_contract -------------------------------------------------------

.assert_kind_list <- function(x, kind, arg) {
    if (!is.list(x)) {
        stop(sprintf("`%s` must be a list", arg), call. = FALSE)
    }
    for (i in seq_along(x)) {
        k <- x[[i]]$kind
        if (is.null(k) || !identical(k, kind)) {
            stop(sprintf(
                "`%s[[%d]]` must have kind %s (got %s)",
                arg, i, sQuote(kind),
                if (is.null(k)) "NULL" else sQuote(k)
            ), call. = FALSE)
        }
    }
    invisible()
}

#' Construct a [Contract()] from typed per-category argument lists.
#'
#' User-friendly constructor that concatenates `scalars`, `vectors`,
#' `matrices`, and `tensors` into the flat `data` slot of [Contract()],
#' and converts `axes` (a list of [axis_contract()] records) into the
#' named-list form the underlying class expects.
#'
#' @param scalars List of [contract_scalar()] records.
#' @param vectors List of [contract_vector()] records.
#' @param matrices List of [contract_matrix()] records.
#' @param tensors List of [tensor_contract()] records.
#' @param axes List of [axis_contract()] records.
#' @param is_relaxed Logical; if `TRUE`, accesses to properties
#'   outside the contract are allowed at enforcement time.
#' @return A [Contract()] object.
#' @examples
#' create_contract(
#'     axes = list(
#'         axis_contract("cell", RequiredInput, "per-cell axis"),
#'         axis_contract("gene", RequiredInput, "per-gene axis")
#'     ),
#'     scalars = list(contract_scalar("organism", RequiredInput, "character", "species")),
#'     vectors = list(contract_vector("cell", "donor", RequiredInput, "character", "donor id")),
#'     matrices = list(contract_matrix("cell", "gene", "UMIs", RequiredInput, "integer", "UMIs"))
#' )
#' @seealso [Contract()], [axis_contract()], [tensor_contract()],
#'   [verify_contract()], [contract_docs()]
#' @export
create_contract <- function(scalars = list(),
                            vectors = list(),
                            matrices = list(),
                            tensors = list(),
                            axes = list(),
                            is_relaxed = FALSE) {
    .assert_kind_list(scalars, "scalar", "scalars")
    .assert_kind_list(vectors, "vector", "vectors")
    .assert_kind_list(matrices, "matrix", "matrices")
    .assert_kind_list(tensors, "tensor", "tensors")
    .assert_kind_list(axes, "axis", "axes")
    if (!is.logical(is_relaxed) || length(is_relaxed) != 1L || is.na(is_relaxed)) {
        stop("`is_relaxed` must be TRUE or FALSE", call. = FALSE)
    }
    axes_named <- stats::setNames(
        lapply(axes, function(a) list(a$expectation, a$description)),
        vapply(axes, `[[`, character(1L), "name")
    )
    Contract(
        name       = "",
        is_relaxed = is_relaxed,
        axes       = axes_named,
        data       = c(scalars, vectors, matrices, tensors)
    )
}

# ---- contract_docs ---------------------------------------------------------

.format_expectation <- function(e) e

.render_axis_row <- function(a_name, spec, format) {
    sprintf("%s | %s | %s",
        a_name, .format_expectation(spec[[1L]]), spec[[2L]]
    )
}

.render_data_row <- function(rec, format) {
    key <- switch(rec$kind,
        scalar = rec$name,
        vector = sprintf("%s / %s", rec$axis, rec$name),
        matrix = sprintf("%s x %s / %s",
            rec$rows_axis, rec$columns_axis, rec$name),
        tensor = sprintf("%s: %s x %s / %s",
            rec$main_axis, rec$rows_axis, rec$columns_axis, rec$name)
    )
    sprintf("%s | %s | %s | %s",
        key, rec$kind, .format_expectation(rec$expectation),
        if (is.null(rec$description)) "" else rec$description
    )
}

#' Render a [Contract()] as human-readable documentation.
#'
#' Returns a single character string describing the axes and data
#' entries. `format = "markdown"` renders as pipe-delimited tables;
#' `format = "text"` uses indented lines.
#'
#' @param contract A [Contract()].
#' @param format One of `"markdown"` or `"text"`.
#' @return Character scalar.
#' @examples
#' c <- create_contract(
#'     axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
#'     vectors = list(contract_vector("cell", "donor", RequiredInput,
#'         "character", "donor id"))
#' )
#' cat(contract_docs(c), "\n")
#' @seealso [create_contract()], [verify_contract()]
#' @export
contract_docs <- function(contract, format = c("markdown", "text")) {
    format <- match.arg(format)
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract (see create_contract())", call. = FALSE)
    }
    axes_lines <- vapply(names(contract@axes),
        function(n) .render_axis_row(n, contract@axes[[n]], format),
        character(1L)
    )
    data_lines <- vapply(contract@data,
        function(r) .render_data_row(r, format),
        character(1L)
    )
    if (format == "markdown") {
        parts <- c(
            "## Axes",
            "",
            "name | expectation | description",
            "---- | ----------- | -----------",
            axes_lines,
            "",
            "## Data",
            "",
            "key | kind | expectation | description",
            "--- | ---- | ----------- | -----------",
            data_lines
        )
    } else {
        parts <- c(
            "Axes:",
            paste0("  ", axes_lines),
            "",
            "Data:",
            paste0("  ", data_lines)
        )
    }
    paste(parts, collapse = "\n")
}

# ---- verify_contract -------------------------------------------------------

#' Single-pass contract verification (input + output).
#'
#' Wraps `daf` in a fresh [contractor()] call using `contract`, then
#' runs [verify_input()] and [verify_output()]. Errors early with a
#' diagnostic on contract violation; returns `invisible(daf)` on
#' success.
#'
#' @param contract A [Contract()].
#' @param daf A [DafReader].
#' @return Invisibly `daf`.
#' @examples
#' withr::with_options(list(dafr.enforce_contracts = TRUE), {
#'     d <- memory_daf()
#'     add_axis(d, "cell", c("c1", "c2"))
#'     set_vector(d, "cell", "donor", c("A", "B"))
#'     c <- create_contract(
#'         axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
#'         vectors = list(contract_vector("cell", "donor", RequiredInput,
#'             "character", "donor id"))
#'     )
#'     verify_contract(c, d)
#' })
#' @seealso [create_contract()], [verify_input()], [verify_output()],
#'   [contractor()]
#' @export
verify_contract <- function(contract, daf) {
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract (see create_contract())", call. = FALSE)
    }
    if (!is_daf(daf)) {
        stop("`daf` must be a DafReader", call. = FALSE)
    }
    cd <- contractor("verify_contract", contract, daf, overwrite = FALSE)
    if (!S7::S7_inherits(cd, ContractDaf)) {
        # Enforcement disabled (options(dafr.enforce_contracts = FALSE));
        # contractor returns daf unchanged. Nothing to verify.
        return(invisible(daf))
    }
    verify_input(cd)
    # Static check only: we do not execute a computation here, so the
    # "unused RequiredInput" diagnostic (driven by tracker$accessed) is
    # not meaningful. Pre-mark all trackers as accessed so verify_output
    # performs existence/type checks without false-positive unused errors.
    ax_env <- S7::prop(cd, "axes")
    for (ax in ls(ax_env, all.names = TRUE)) {
        ax_env[[ax]]$accessed <- TRUE
    }
    data_env <- S7::prop(cd, "data")
    for (key in ls(data_env, all.names = TRUE)) {
        data_env[[key]]$accessed <- TRUE
    }
    verify_output(cd)
    invisible(daf)
}
