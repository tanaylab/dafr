#' Test whether an axis exists.
#' @param daf A `DafReader`.
#' @param axis Axis name (character scalar).
#' @return Logical scalar.
#' @examples
#' # Mirrors readers.jl jldoctest at lines 210 + 218.
#' has_axis(example_cells_daf(),     "metacell") # FALSE
#' has_axis(example_metacells_daf(), "metacell") # TRUE
#' @export
has_axis <- function(daf, axis) {
    .assert_name(axis, "axis")
    format_has_axis(daf, axis)
}

#' Names of all axes, sorted.
#' @inheritParams has_axis
#' @return Character vector of axis names.
#' @examples
#' # Mirrors readers.jl jldoctest at line 273.
#' axes_set(example_cells_daf()) # "cell" "donor" "experiment" "gene"
#' @export
axes_set <- function(daf) format_axes_set(daf)

#' Length (entry count) of an axis.
#' @inheritParams has_axis
#' @return Integer scalar.
#' @examples
#' # Mirrors readers.jl jldoctest at line 487.
#' axis_length(example_metacells_daf(), "type") # 4
#' @export
axis_length <- function(daf, axis) {
    .assert_name(axis, "axis")
    format_axis_length(daf, axis)
}

#' Entry-name vector for an axis.
#'
#' @inheritParams has_axis
#' @param null_if_missing If `TRUE`, return `NULL` when the axis is
#'   absent instead of raising.
#' @return Character vector of entry names.
#' @examples
#' # Mirrors readers.jl jldoctest at line 308.
#' axis_vector(example_metacells_daf(), "type")
#' # "memory-B" "MEBEMP-E" "MEBEMP-L" "MPP"
#' axis_vector(example_cells_daf(), "missing", null_if_missing = TRUE)
#' @export
axis_vector <- function(daf, axis, null_if_missing = FALSE) {
    .assert_name(axis, "axis")
    if (!format_has_axis(daf, axis)) {
        if (isTRUE(null_if_missing)) {
            return(NULL)
        }
        stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    }
    format_axis_array(daf, axis)$value
}

#' Entry names of an axis (full or by index).
#'
#' @inheritParams has_axis
#' @param indices Optional integer index vector (1-based). When
#'   `allow_empty = TRUE`, a zero or negative index is allowed and is
#'   translated to the empty string `""` in the result.
#' @param allow_empty If `TRUE`, treat zero/negative `indices` as the
#'   empty string `""` in the result (mirrors Julia
#'   `axis_entries(...; allow_empty=true)`).
#' @return Character vector.
#' @examples
#' # Mirrors readers.jl jldoctest at line 448.
#' axis_entries(example_metacells_daf(), "type",
#'              indices = c(3L, 0L), allow_empty = TRUE)
#' # "MEBEMP-L" ""
#' @export
axis_entries <- function(daf, axis, indices = NULL, allow_empty = FALSE) {
    .assert_flag(allow_empty, "allow_empty")
    entries <- axis_vector(daf, axis)
    if (is.null(indices)) {
        return(entries)
    }
    if (!(is.numeric(indices) || is.integer(indices))) {
        stop("`indices` must be an integer vector", call. = FALSE)
    }
    if (anyNA(indices)) {
        stop("`indices` must not contain NA", call. = FALSE)
    }
    if (any(indices != as.integer(indices))) {
        stop("`indices` must be integer-valued", call. = FALSE)
    }
    indices <- as.integer(indices)
    if (allow_empty) {
        out <- character(length(indices))
        valid <- indices > 0L
        if (any(indices[valid] > length(entries))) {
            stop(sprintf("indices out of range [1, %d]", length(entries)), call. = FALSE)
        }
        out[valid]  <- entries[indices[valid]]
        out[!valid] <- ""
        return(out)
    }
    if (any(indices < 1L | indices > length(entries))) {
        stop(sprintf("indices out of range [1, %d]", length(entries)), call. = FALSE)
    }
    entries[indices]
}

#' Look up 1-based positions of entries in an axis.
#'
#' @inheritParams has_axis
#' @param entries Character vector of entry names to resolve.
#' @param allow_empty If `TRUE`, the empty string `""` resolves to a zero
#'   index (mirrors Julia `axis_indices(...; allow_empty=true)`).
#' @param allow_missing If `TRUE`, any non-empty name that is not present
#'   in the axis resolves to a zero index (mirrors Julia
#'   `axis_indices(...; allow_missing=true)`).
#' @return Integer vector of positions (1-based when present;
#'   `0L` when `allow_empty`/`allow_missing` substitutes for an empty or
#'   absent name); same length as `entries`.
#' @examples
#' # Mirrors readers.jl jldoctest at line 389.
#' axis_indices(example_metacells_daf(), "type",
#'              c("MPP", ""), allow_empty = TRUE)
#' # 4 0
#' @export
axis_indices <- function(daf, axis, entries, allow_empty = FALSE,
                         allow_missing = FALSE) {
    .assert_name(axis, "axis")
    .assert_flag(allow_empty, "allow_empty")
    .assert_flag(allow_missing, "allow_missing")
    if (!is.character(entries)) stop("`entries` must be a character vector", call. = FALSE)
    if (anyNA(entries)) stop("`entries` must not contain NA", call. = FALSE)
    dict <- format_axis_dict(daf, axis)
    out <- integer(length(entries))
    missing_mask <- logical(length(entries))
    for (i in seq_along(entries)) {
        nm <- entries[[i]]
        if (!nzchar(nm)) {
            if (allow_empty) {
                out[i] <- 0L
            } else {
                missing_mask[i] <- TRUE
            }
            next
        }
        v <- dict[[nm]]
        if (is.null(v)) {
            if (allow_missing) {
                out[i] <- 0L
            } else {
                missing_mask[i] <- TRUE
            }
        } else {
            out[i] <- as.integer(v)
        }
    }
    if (any(missing_mask)) {
        stop(
            sprintf(
                "entries not found in axis %s: %s",
                sQuote(axis),
                paste(sQuote(entries[missing_mask]), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    out
}

#' Entry-name to 1-based-index hash for an axis.
#' @inheritParams has_axis
#' @return An environment mapping entry names to integer positions.
#' @examples
#' # Mirrors readers.jl jldoctest at line 353.
#' dict <- axis_dict(example_metacells_daf(), "type")
#' dict[["memory-B"]] # 1
#' dict[["MPP"]]      # 4
#' @export
axis_dict <- function(daf, axis) {
    .assert_name(axis, "axis")
    format_axis_dict(daf, axis)
}

#' Test whether a scalar exists.
#' @param daf A `DafReader`.
#' @param name Scalar name.
#' @return Logical scalar.
#' @examples
#' # Mirrors readers.jl jldoctests at lines 92 + 100.
#' has_scalar(example_cells_daf(),     "organism") # TRUE
#' has_scalar(example_metacells_daf(), "organism") # FALSE
#' @export
has_scalar <- function(daf, name) {
    .assert_name(name, "name")
    format_has_scalar(daf, name)
}

#' Names of all scalars, sorted.
#' @inheritParams has_scalar
#' @return Character vector.
#' @examples
#' # Mirrors readers.jl jldoctest at line 125.
#' scalars_set(example_cells_daf()) # "organism" "reference"
#' @export
scalars_set <- function(daf) format_scalars_set(daf)

#' Get a scalar, optionally with a default when missing.
#' @inheritParams has_scalar
#' @param default Value to return when the scalar is absent. If missing
#'   and the scalar is absent, an error is raised.
#' @return The scalar value.
#' @examples
#' # Mirrors readers.jl jldoctests at lines 157 + 165.
#' get_scalar(example_cells_daf(), "organism") # "human"
#' get_scalar(example_metacells_daf(), "organism", default = NULL) # NULL
#' @export
get_scalar <- function(daf, name, default) {
    .assert_name(name, "name")
    if (format_has_scalar(daf, name)) {
        return(format_get_scalar(daf, name)$value)
    }
    if (!missing(default)) {
        return(default)
    }
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
}

#' Test whether a vector exists on an axis.
#' @param daf A `DafReader`.
#' @param axis Axis name.
#' @param name Vector name.
#' @return Logical scalar.
#' @examples
#' # Mirrors readers.jl jldoctests at lines 525 + 533.
#' has_vector(example_cells_daf(),     "cell",     "type") # FALSE
#' has_vector(example_metacells_daf(), "metacell", "type") # TRUE
#' @export
has_vector <- function(daf, axis, name) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    format_has_vector(daf, axis, name)
}

#' Names of vectors on an axis, sorted.
#' @inheritParams has_vector
#' @return Character vector.
#' @examples
#' # Mirrors readers.jl jldoctest at line 595.
#' vectors_set(example_cells_daf(), "cell") # "donor" "experiment"
#' @export
vectors_set <- function(daf, axis) {
    .assert_name(axis, "axis")
    format_vectors_set(daf, axis)
}

#' Get a vector, returning it as an axis-named R vector.
#'
#' @param daf A `DafReader`.
#' @param axis Axis name.
#' @param name Vector name.
#' @param default If supplied and the vector is absent, return a named
#'   vector of length `axis_length(daf, axis)` with the axis entries as
#'   names. A length-1 `default` is recycled to every entry; a length-N
#'   `default` (matching the axis length) is used as-is. Any other
#'   length is an error. The vector's atomic type follows `default`
#'   (e.g. `default = NA` yields `logical`, `default = "x"` yields
#'   `character`, `default = 0.0` yields `double`).
#' @return Named atomic vector.
#' @examples
#' # Mirrors readers.jl jldoctest at line 633.
#' get_vector(example_metacells_daf(), "type", "color")
#' # memory-B="steelblue" MEBEMP-E="#eebb6e" MEBEMP-L="plum" MPP="gold"
#'
#' # Default for a missing vector (recycled to axis length):
#' head(get_vector(example_cells_daf(), "cell", "missing_vec",
#'                 default = NA_character_))
#' @export
get_vector <- function(daf, axis, name, default) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    if (!format_has_axis(daf, axis)) {
        stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    }
    entries <- format_axis_array(daf, axis)$value
    if (!format_has_vector(daf, axis, name)) {
        if (missing(default)) {
            stop(sprintf(
                "vector %s does not exist on axis %s",
                sQuote(name), sQuote(axis)
            ), call. = FALSE)
        }
        n <- length(entries)
        if (length(default) == 1L) {
            out <- rep(default, n)
        } else if (length(default) == n) {
            out <- default
        } else {
            stop(sprintf(
                "default has length %d (expected 1 or %d) for axis %s",
                length(default), n, sQuote(axis)
            ), call. = FALSE)
        }
        names(out) <- entries
        return(out)
    }
    cache_key <- cache_key_vector(axis, name)
    cache_env <- S7::prop(daf, "cache")
    stamp_now <- vector_stamp(daf, axis, name)
    res <- format_get_vector(daf, axis, name)
    raw <- res$value
    tier <- .canonical_tier(res$cache_group)
    hit <- cache_lookup(cache_env, tier, cache_key, stamp_now)
    if (!is.null(hit)) {
        return(hit)
    }
    out <- raw
    if (is.null(names(out))) names(out) <- entries
    cache_store(cache_env, tier, cache_key, out, stamp_now,
        size_bytes = object.size(out)
    )
    out
}

#' Test whether a matrix exists for an axis pair.
#' @param daf A `DafReader`.
#' @param rows_axis Row-axis name.
#' @param columns_axis Column-axis name.
#' @param name Matrix name.
#' @param relayout If `TRUE` (default), also report `TRUE` when the
#'   matrix is stored only at the flipped axis pair
#'   `(columns_axis, rows_axis)`. Set to `FALSE` to ask the strict
#'   "this exact layout?" question. Mirrors Julia
#'   `has_matrix(...; relayout)`.
#' @return Logical scalar.
#' @examples
#' # Mirrors readers.jl jldoctest at line 748.
#' has_matrix(example_cells_daf(), "gene", "cell", "UMIs") # TRUE
#'
#' # `relayout = FALSE` asks the strict "this exact layout?" question:
#' has_matrix(example_cells_daf(), "gene", "cell", "UMIs", relayout = FALSE)
#' @export
has_matrix <- function(daf, rows_axis, columns_axis, name, relayout = TRUE) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_flag(relayout, "relayout")
    if (format_has_matrix(daf, rows_axis, columns_axis, name)) {
        return(TRUE)
    }
    if (relayout && rows_axis != columns_axis &&
        format_has_matrix(daf, columns_axis, rows_axis, name)) {
        return(TRUE)
    }
    FALSE
}

#' Names of matrices for an axis pair, sorted.
#' @inheritParams has_matrix
#' @return Character vector.
#' @examples
#' # Mirrors readers.jl jldoctest at line 801.
#' matrices_set(example_cells_daf(), "gene", "cell") # "UMIs"
#' @export
matrices_set <- function(daf, rows_axis, columns_axis) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    format_matrices_set(daf, rows_axis, columns_axis)
}

#' Get a matrix, returning it with axis-entry dimnames.
#'
#' When the matrix is stored only at the flipped-layout axis pair
#' `(columns_axis, rows_axis)`, this function transposes on-the-fly and
#' returns with the requested dimnames.
#'
#' @inheritParams has_matrix
#' @param default If supplied and the matrix is absent under both
#'   layouts, return a constant-valued `nrow x ncol` matrix with axis
#'   entries as dimnames.
#' @return Dense `matrix` or sparse `dgCMatrix` / `lgCMatrix` with
#'   dimnames set.
#' @examples
#' # Mirrors readers.jl jldoctest at line 933.
#' m <- get_matrix(example_metacells_daf(), "gene", "metacell", "fraction")
#' dim(m)            # 683  7
#' colnames(m)       # 7 metacell IDs
#' m[1:3, 1:2]
#' @export
get_matrix <- function(daf, rows_axis, columns_axis, name, default) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")

    rows <- format_axis_array(daf, rows_axis)$value
    cols <- format_axis_array(daf, columns_axis)$value

    primary <- format_has_matrix(daf, rows_axis, columns_axis, name)
    flipped <- !primary && format_has_matrix(daf, columns_axis, rows_axis, name)

    if (!primary && !flipped) {
        if (missing(default)) {
            stop(
                sprintf(
                    "matrix %s does not exist on axes (%s, %s)",
                    sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
                ),
                call. = FALSE
            )
        }
        out <- matrix(default,
            nrow = length(rows), ncol = length(cols),
            dimnames = list(rows, cols)
        )
        return(out)
    }

    if (primary) {
        ra <- rows_axis
        ca <- columns_axis
    } else {
        ra <- columns_axis
        ca <- rows_axis
    }

    cache_key <- cache_key_matrix(ra, ca, name)
    cache_env <- S7::prop(daf, "cache")
    stamp_now <- matrix_stamp(daf, ra, ca, name)
    res <- format_get_matrix(daf, ra, ca, name)
    raw <- res$value
    tier <- .canonical_tier(res$cache_group)
    stored <- cache_lookup(cache_env, tier, cache_key, stamp_now)
    if (is.null(stored)) {
        stored <- raw
        cache_store(cache_env, tier, cache_key, stored, stamp_now,
            size_bytes = object.size(stored)
        )
    }

    out <- if (flipped) {
        if (methods::is(stored, "dgCMatrix") || methods::is(stored, "lgCMatrix")) {
            Matrix::t(stored)
        } else {
            t(stored)
        }
    } else {
        stored
    }

    if (methods::is(out, "dgCMatrix") || methods::is(out, "lgCMatrix")) {
        out@Dimnames <- list(rows, cols)
    } else {
        dimnames(out) <- list(rows, cols)
    }
    out
}

#' Human-readable summary of a Daf store.
#'
#' Returns a multi-line string describing axes, scalars, vectors, and
#' matrices. Matches the column-order rendering of Julia DAF's own
#' `description()`.
#'
#' @param daf A `DafReader`.
#' @return Character scalar.
#' @examples
#' # Mirrors readers.jl jldoctest at line 1170 (description of the chain).
#' cat(description(example_chain_daf()))
#' @export
description <- function(daf) {
    lines <- c(
        sprintf("name: %s", S7::prop(daf, "name")),
        sprintf("type: %s", .daf_type_name(daf))
    )
    sc <- format_scalars_set(daf)
    if (length(sc)) {
        lines <- c(lines, "scalars:")
        for (nm in sc) {
            v <- format_get_scalar(daf, nm)$value
            lines <- c(lines, sprintf("  %s: %s", nm, .format_scalar_literal(v)))
        }
    }
    axes <- format_axes_set(daf)
    if (length(axes)) {
        lines <- c(lines, "axes:")
        for (ax in axes) {
            lines <- c(lines, sprintf("  %s: %d entries", ax, format_axis_length(daf, ax)))
        }
    }
    vec_axes <- Filter(function(ax) length(format_vectors_set(daf, ax)) > 0L, axes)
    if (length(vec_axes)) {
        lines <- c(lines, "vectors:")
        for (ax in vec_axes) {
            lines <- c(lines, sprintf("  %s:", ax))
            for (nm in format_vectors_set(daf, ax)) {
                lines <- c(lines, sprintf("    %s", nm))
            }
        }
    }
    mat_keys <- character(0L)
    for (ra in axes) {
        for (ca in axes) {
            ms <- format_matrices_set(daf, ra, ca)
            if (length(ms)) mat_keys <- c(mat_keys, sprintf("%s,%s", ra, ca))
        }
    }
    if (length(mat_keys)) {
        lines <- c(lines, "matrices:")
        for (k in mat_keys) {
            parts <- strsplit(k, ",", fixed = TRUE)[[1L]]
            lines <- c(lines, sprintf("  %s:", k))
            for (nm in format_matrices_set(daf, parts[[1L]], parts[[2L]])) {
                lines <- c(lines, sprintf("    %s", nm))
            }
        }
    }
    paste0(paste(lines, collapse = "\n"), "\n")
}

.daf_type_name <- function(daf) {
    cls <- class(daf)[[1L]]
    sub("^dafr::", "", cls)
}

.format_scalar_literal <- function(v) {
    if (is.character(v)) {
        sprintf('"%s"', v)
    } else {
        format(v)
    }
}
