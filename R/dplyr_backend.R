#' @include classes.R queries.R readers.R writers.R dataframes.R
NULL

# ---- Internal constructor --------------------------------------------------

# State carried by a daf_axis_tbl:
#   daf         : DafReader — the backing daf
#   axis        : character — axis name
#   row_mask    : integer or NULL — positions into the full axis; NULL = full
#   col_select  : character or NULL — explicit column selection; NULL = default
#                 (show all stored vectors + all overrides)
#   overrides   : named list of vectors, each aligned to row_mask order
#   groups      : character — names of grouping variables
#
# Invariant: overrides[[nm]] is always aligned to the current row_mask.
# arrange() commits its permutation to row_mask (and reorders overrides).

new_daf_axis_tbl <- function(daf, axis,
                             row_mask = NULL,
                             col_select = NULL,
                             overrides = list(),
                             groups = character()) {
    if (!is_daf(daf)) stop("`daf` must be a DafReader", call. = FALSE)
    if (!has_axis(daf, axis)) {
        stop(sprintf("axis %s is not in the daf", sQuote(axis)), call. = FALSE)
    }
    structure(
        list(),
        daf = daf,
        axis = axis,
        row_mask = row_mask,
        col_select = col_select,
        overrides = overrides,
        groups = groups,
        class = "daf_axis_tbl"
    )
}

# ---- tbl() entry point -----------------------------------------------------

# S3 method bound to dplyr::tbl for class "dafr::DafReader" (the S7
# class string). Named without dots so roxygen doesn't confuse it for
# an S3 method declaration; the actual binding is done via
# registerS3method() in .onLoad.
#' @noRd
tbl_DafReader_axis <- function(src, axis, ...) {
    new_daf_axis_tbl(src, axis)
}

# ---- Realization -----------------------------------------------------------

# Apply row_mask, col_select, overrides, groups to produce a tibble.
# All vectors (stored and override) are emitted in row_mask order.
.realize <- function(x) {
    daf <- attr(x, "daf")
    axis <- attr(x, "axis")
    entries <- axis_entries(daf, axis)
    n <- length(entries)

    rm <- attr(x, "row_mask")
    if (is.null(rm)) rm <- seq_len(n)

    stored_cols <- vectors_set(daf, axis)
    sel <- attr(x, "col_select")
    sel_explicit <- !is.null(sel)
    if (!sel_explicit) sel <- stored_cols

    overrides <- attr(x, "overrides")

    cols <- list()
    emit <- function(nm) {
        if (nm == "name") {
            cols[["name"]] <<- entries[rm]
        } else if (nm %in% names(overrides)) {
            cols[[nm]] <<- unname(overrides[[nm]])
        } else if (nm %in% stored_cols) {
            cols[[nm]] <<- unname(get_vector(daf, axis, nm)[rm])
        }
    }
    if (sel_explicit) {
        # Explicit sel defines the full output including where "name"
        # lands. If user didn't include "name", it goes first.
        if (!("name" %in% sel)) emit("name")
        for (nm in sel) emit(nm)
    } else {
        # Default: name first, then stored cols, then extra overrides.
        emit("name")
        for (nm in sel) emit(nm)
        extra <- setdiff(names(overrides), c(sel, "name"))
        for (nm in extra) emit(nm)
    }

    out <- tibble::as_tibble(cols)

    grps <- attr(x, "groups")
    if (length(grps)) out <- dplyr::group_by(out, !!!rlang::syms(grps))

    out
}

# ---- dplyr infrastructure methods ------------------------------------------

# tbl_vars reports the column names — used by many dplyr internals
# (e.g. check_n_name in add_tally). We report name + all currently
# visible columns (honoring col_select).
#' @noRd
tbl_vars_daf_axis_tbl <- function(x) {
    daf <- attr(x, "daf")
    axis <- attr(x, "axis")
    stored <- vectors_set(daf, axis)
    overrides <- attr(x, "overrides")
    sel <- attr(x, "col_select")
    if (is.null(sel)) {
        out <- c("name", stored, setdiff(names(overrides), stored))
    } else {
        out <- if ("name" %in% sel) sel else c("name", sel)
    }
    unique(out)
}

# group_vars: which columns are grouping variables.
#' @noRd
group_vars_daf_axis_tbl <- function(x) attr(x, "groups") %||% character()

# ---- collect / as_tibble / print -------------------------------------------
#
# These S3 method bodies are named without dots so roxygen2 doesn't
# try to document them as methods. They are bound to their generics
# via registerS3method() in .onLoad (see R/zzz.R).

#' @noRd
collect_daf_axis_tbl <- function(x, ...) .realize(x)

#' @noRd
as_tibble_daf_axis_tbl <- function(x, ...) {
    out <- .realize(x)
    if (inherits(out, "grouped_df")) out <- dplyr::ungroup(out)
    out
}

#' @noRd
print_daf_axis_tbl <- function(x, n = 6L, ...) {
    daf <- attr(x, "daf")
    axis <- attr(x, "axis")
    rm <- attr(x, "row_mask")
    nrows <- if (is.null(rm)) axis_length(daf, axis) else length(rm)
    cat(sprintf("<daf_axis_tbl>  axis: %s  [%d rows]\n", axis, nrows))
    grps <- attr(x, "groups")
    if (length(grps)) cat("groups:", paste(grps, collapse = ", "), "\n")
    print(utils::head(.realize(x), n))
    invisible(x)
}

# ---- select / pull ---------------------------------------------------------

#' @noRd
select_daf_axis_tbl <- function(.data, ...) {
    # Realize to a typed tibble so tidyselect predicates like
    # `where(is.integer)` can inspect column types. If the selection
    # only reorders/subsets columns without renaming, we just update
    # col_select; if it renames, we materialize as overrides.
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    chosen <- tidyselect::eval_select(rlang::expr(c(...)), df)
    selected_orig <- colnames(df)[chosen]
    selected_new <- names(chosen)
    if (identical(selected_orig, selected_new)) {
        attr(.data, "col_select") <- selected_new
        .data
    } else {
        overrides <- as.list(df[, chosen, drop = FALSE])
        names(overrides) <- selected_new
        overrides[["name"]] <- NULL
        new_daf_axis_tbl(
            daf = attr(.data, "daf"),
            axis = attr(.data, "axis"),
            row_mask = attr(.data, "row_mask"),
            col_select = setdiff(selected_new, "name"),
            overrides = overrides,
            groups = attr(.data, "groups")
        )
    }
}

#' @noRd
pull_daf_axis_tbl <- function(.data, var = -1, name = NULL, ...) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    dplyr::pull(df, {{ var }}, name = {{ name }})
}

# ---- row-reduction helper (shared by filter, slice*) -----------------------

# Apply a `reducer(df) -> kept_df` on the realized tibble, then
# reindex row_mask and overrides to the kept rows.
.apply_row_reducer <- function(.data, reducer) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    kept <- reducer(df)
    if (inherits(kept, "grouped_df")) kept <- dplyr::ungroup(kept)
    daf <- attr(.data, "daf")
    axis <- attr(.data, "axis")
    attr(.data, "row_mask") <- match(kept$name, axis_entries(daf, axis))
    overrides <- attr(.data, "overrides")
    if (length(overrides)) {
        local_idx <- match(kept$name, df$name)
        attr(.data, "overrides") <- lapply(overrides, function(v) v[local_idx])
    }
    .data
}

# ---- filter ----------------------------------------------------------------

#' @noRd
filter_daf_axis_tbl <- function(.data, ..., .by = NULL) {
    .apply_row_reducer(.data, function(df) dplyr::filter(df, ..., .by = {{ .by }}))
}

# ---- slice family ----------------------------------------------------------

#' @noRd
slice_daf_axis_tbl <- function(.data, ..., .by = NULL, .preserve = FALSE) {
    .apply_row_reducer(.data, function(df) dplyr::slice(df, ..., .by = {{ .by }}))
}

#' @noRd
slice_head_daf_axis_tbl <- function(.data, ..., by = NULL) {
    .apply_row_reducer(.data, function(df) dplyr::slice_head(df, ..., by = {{ by }}))
}

#' @noRd
slice_tail_daf_axis_tbl <- function(.data, ..., by = NULL) {
    .apply_row_reducer(.data, function(df) dplyr::slice_tail(df, ..., by = {{ by }}))
}

#' @noRd
slice_min_daf_axis_tbl <- function(.data, order_by, ..., by = NULL) {
    .apply_row_reducer(.data, function(df) {
        dplyr::slice_min(df, order_by = {{ order_by }}, ..., by = {{ by }})
    })
}

#' @noRd
slice_max_daf_axis_tbl <- function(.data, order_by, ..., by = NULL) {
    .apply_row_reducer(.data, function(df) {
        dplyr::slice_max(df, order_by = {{ order_by }}, ..., by = {{ by }})
    })
}

#' @noRd
slice_sample_daf_axis_tbl <- function(.data, ..., by = NULL) {
    .apply_row_reducer(.data, function(df) dplyr::slice_sample(df, ..., by = {{ by }}))
}

# ---- rename / relocate -----------------------------------------------------

# rename is implemented by realizing the tbl, applying dplyr::rename,
# and rebuilding the daf_axis_tbl with all columns now as overrides
# (so the rename sticks — stored columns are keyed by their daf name
# and can't be "renamed at source").

#' @noRd
rename_daf_axis_tbl <- function(.data, ...) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    renamed <- dplyr::rename(df, ...)
    daf <- attr(.data, "daf")
    axis <- attr(.data, "axis")
    overrides <- as.list(
        renamed[, setdiff(colnames(renamed), "name"), drop = FALSE]
    )
    new_daf_axis_tbl(
        daf = daf,
        axis = axis,
        row_mask = attr(.data, "row_mask"),
        col_select = names(overrides),
        overrides = overrides,
        groups = attr(.data, "groups")
    )
}

#' @noRd
relocate_daf_axis_tbl <- function(.data, ..., .before = NULL, .after = NULL) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    reordered <- dplyr::relocate(df, ...,
        .before = {{ .before }}, .after = {{ .after }})
    attr(.data, "col_select") <- colnames(reordered)
    .data
}

# ---- count / tally / add_count / add_tally --------------------------------
#
# These are thin sugar over group_by + summarise/mutate and inherit
# our existing tie-back behavior. `wt = ...` (weighted counting) is
# not supported in v1.1.

.check_no_wt <- function(wt) {
    wt_expr <- rlang::enquo(wt)
    if (!rlang::quo_is_null(wt_expr)) {
        warning("count()/tally() `wt =` is not supported on daf_axis_tbl; ignoring.",
                call. = FALSE)
    }
}

#' @noRd
count_daf_axis_tbl <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
    .check_no_wt({{ wt }})
    nm <- if (is.null(name)) "n" else name
    grouped <- dplyr::group_by(x, ..., .add = FALSE)
    result <- dplyr::summarise(grouped, !!nm := dplyr::n())
    if (isTRUE(sort)) {
        result <- dplyr::arrange(result, dplyr::desc(.data[[nm]]))
    }
    result
}

#' @noRd
tally_daf_axis_tbl <- function(x, wt = NULL, sort = FALSE, name = NULL) {
    .check_no_wt({{ wt }})
    nm <- if (is.null(name)) "n" else name
    result <- dplyr::summarise(x, !!nm := dplyr::n())
    if (isTRUE(sort)) {
        result <- dplyr::arrange(result, dplyr::desc(.data[[nm]]))
    }
    result
}

#' @noRd
add_count_daf_axis_tbl <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
    .check_no_wt({{ wt }})
    nm <- if (is.null(name)) "n" else name
    grouped <- dplyr::group_by(x, ..., .add = TRUE)
    result <- dplyr::mutate(grouped, !!nm := dplyr::n())
    result <- dplyr::ungroup(result)
    if (isTRUE(sort)) {
        result <- dplyr::arrange(result, dplyr::desc(.data[[nm]]))
    }
    result
}

#' @noRd
add_tally_daf_axis_tbl <- function(x, wt = NULL, sort = FALSE, name = NULL) {
    .check_no_wt({{ wt }})
    nm <- if (is.null(name)) "n" else name
    result <- dplyr::mutate(x, !!nm := dplyr::n())
    if (isTRUE(sort)) {
        result <- dplyr::arrange(result, dplyr::desc(.data[[nm]]))
    }
    result
}

# ---- transmute / reframe --------------------------------------------------

# transmute is mutate-then-drop-everything-except-new-columns. We keep
# the axis "name" column automatically (it's the row identity), so
# transmute's dropping applies to stored cols and pre-existing
# overrides.

#' @noRd
transmute_daf_axis_tbl <- function(.data, ...) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    result <- dplyr::transmute(df, ...)
    daf <- attr(.data, "daf")
    axis <- attr(.data, "axis")
    overrides <- as.list(
        result[, setdiff(colnames(result), "name"), drop = FALSE]
    )
    new_daf_axis_tbl(
        daf = daf,
        axis = axis,
        row_mask = attr(.data, "row_mask"),
        col_select = names(overrides),
        overrides = overrides,
        groups = attr(.data, "groups")
    )
}

# reframe allows multi-row-per-group output, so it always exits the
# daf_axis_tbl world (row count doesn't match the axis).

#' @noRd
reframe_daf_axis_tbl <- function(.data, ..., .by = NULL) {
    df <- .realize(.data)
    dplyr::reframe(df, ..., .by = {{ .by }})
}

# ---- mutate ----------------------------------------------------------------

#' @noRd
mutate_daf_axis_tbl <- function(.data, ..., .by = NULL,
                                .keep = c("all", "used", "unused", "none"),
                                .before = NULL, .after = NULL) {
    .keep <- match.arg(.keep)
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) {
        df2 <- dplyr::ungroup(dplyr::mutate(df, ..., .by = {{ .by }},
            .keep = .keep, .before = {{ .before }}, .after = {{ .after }}))
        df <- dplyr::ungroup(df)
    } else {
        df2 <- dplyr::mutate(df, ..., .by = {{ .by }},
            .keep = .keep, .before = {{ .before }}, .after = {{ .after }})
    }
    old_names <- colnames(df)
    new_or_changed <- setdiff(colnames(df2), "name")
    overrides <- attr(.data, "overrides")
    for (nm in new_or_changed) {
        if (!(nm %in% old_names) || !identical(df[[nm]], df2[[nm]])) {
            overrides[[nm]] <- df2[[nm]]
        }
    }
    attr(.data, "overrides") <- overrides
    # When .keep != "all", dplyr::mutate returns only a subset of
    # columns. Restrict col_select so our tbl displays the same subset.
    if (.keep != "all") {
        attr(.data, "col_select") <- setdiff(colnames(df2), "name")
    }
    .data
}

# ---- arrange / distinct ----------------------------------------------------

#' @noRd
arrange_daf_axis_tbl <- function(.data, ..., .by_group = FALSE) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    sorted <- dplyr::arrange(df, ...)
    local_idx <- match(sorted$name, df$name)
    daf <- attr(.data, "daf")
    axis <- attr(.data, "axis")
    old_rm <- attr(.data, "row_mask")
    if (is.null(old_rm)) old_rm <- seq_len(axis_length(daf, axis))
    attr(.data, "row_mask") <- old_rm[local_idx]
    overrides <- attr(.data, "overrides")
    if (length(overrides)) {
        attr(.data, "overrides") <- lapply(overrides, function(v) v[local_idx])
    }
    .data
}

#' @noRd
distinct_daf_axis_tbl <- function(.data, ..., .keep_all = FALSE) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    dplyr::distinct(df, ..., .keep_all = .keep_all)
}

# ---- group_by / summarise --------------------------------------------------

#' @noRd
group_by_daf_axis_tbl <- function(.data, ..., .add = FALSE,
                                  .drop = dplyr::group_by_drop_default(.data)) {
    quos <- rlang::enquos(...)
    vars <- unname(vapply(quos, rlang::as_name, character(1)))
    prev <- attr(.data, "groups")
    attr(.data, "groups") <- if (isTRUE(.add)) unique(c(prev, vars)) else vars
    .data
}

#' @noRd
ungroup_daf_axis_tbl <- function(x, ...) {
    attr(x, "groups") <- character()
    x
}

# ---- compute() — explicit write-back --------------------------------------
#
# We hook into dplyr::compute rather than defining our own generic, so
# dafr doesn't shadow dplyr's compute() for db-tbl users.
# `dplyr::compute(daf_tbl, vectors = c("x"))` persists named overrides
# on the axis via set_vector(). Called as a method on an existing
# generic (not a new symbol), so no NAMESPACE export is needed — just
# the runtime binding in .onLoad via registerS3method().

#' @noRd
compute_daf_axis_tbl <- function(x, ..., vectors = NULL, overwrite = FALSE) {
    if (is.null(vectors) || !length(vectors)) {
        stop("compute(): `vectors` must name override columns to persist",
             call. = FALSE)
    }
    overrides <- attr(x, "overrides")
    absent <- setdiff(vectors, names(overrides))
    if (length(absent)) {
        stop(sprintf(
            "compute(): not present as an override: %s. Create with mutate().",
            paste(sQuote(absent), collapse = ", ")
        ), call. = FALSE)
    }
    daf <- attr(x, "daf")
    axis <- attr(x, "axis")
    rm <- attr(x, "row_mask")
    n <- axis_length(daf, axis)
    if (is.null(rm)) {
        reorder_idx <- NULL
    } else if (length(rm) == n && !anyDuplicated(rm)) {
        reorder_idx <- order(rm)
    } else {
        stop("compute(): tbl has a partial row mask (filtered/reduced); cannot write back a full vector. Undo the filter or explicitly extend.",
             call. = FALSE)
    }
    for (nm in vectors) {
        v <- overrides[[nm]]
        if (!is.null(reorder_idx)) v <- v[reorder_idx]
        set_vector(daf, axis, nm, v, overwrite = overwrite)
    }
    invisible(x)
}

#' @noRd
summarise_daf_axis_tbl <- function(.data, ..., .by = NULL, .groups = NULL) {
    df <- .realize(.data)
    # Resolve grouping source: either persistent groups (from group_by)
    # or a per-call .by. We can't use both.
    by_quo <- rlang::enquo(.by)
    has_by <- !rlang::quo_is_null(by_quo)
    persistent_grps <- attr(.data, "groups")
    if (has_by && length(persistent_grps) > 0) {
        stop("summarise() can't use both group_by() and `.by` on the same call",
             call. = FALSE)
    }
    effective_grps <- if (has_by) {
        # Extract names of .by expression(s) — can be a single name or c(a, b).
        by_expr <- rlang::quo_get_expr(by_quo)
        if (rlang::is_call(by_expr, "c")) {
            vapply(rlang::call_args(by_expr), rlang::as_string, character(1))
        } else {
            rlang::as_string(by_expr)
        }
    } else {
        persistent_grps
    }

    if (length(effective_grps) == 0) {
        if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
        return(dplyr::summarise(df, ...))
    }
    # Route through dplyr: if using .by, pass it; else rely on the
    # grouped_df from .realize.
    if (has_by) {
        if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
        result <- dplyr::summarise(df, ..., .by = !!by_quo)
    } else {
        result <- dplyr::summarise(df, ..., .groups = "drop")
    }
    daf <- attr(.data, "daf")
    # Auto-tie-back: single group var, that var names an axis in the daf.
    if (length(effective_grps) == 1L && has_axis(daf, effective_grps)) {
        axis_name <- effective_grps
        all_entries <- axis_entries(daf, axis_name)
        group_col <- result[[axis_name]]
        if (is.character(group_col) && all(group_col %in% all_entries)) {
            rm <- match(group_col, all_entries)
            derived <- as.list(result[, setdiff(colnames(result), axis_name),
                drop = FALSE])
            return(new_daf_axis_tbl(
                daf = daf,
                axis = axis_name,
                row_mask = rm,
                col_select = names(derived),
                overrides = derived
            ))
        }
    }
    result
}
