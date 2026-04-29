#' In-memory Daf store.
#'
#' A concrete `DafWriter` backed entirely by R environments — no disk,
#' no mmap. Scalars, axes, vectors, and matrices live in nested
#' environments (hash tables) under the `internal` property:
#'
#' - `internal$scalars`     : `env(name -> value)`
#' - `internal$axes`        : `env(axis -> list(entries = character, dict = env))`
#' - `internal$vectors`     : `env(axis -> env(name -> vector))`
#' - `internal$matrices`    : `env(rows_axis -> env(columns_axis -> env(name -> matrix)))`
#'
#' @param name Human-readable identifier. Defaults to `"memory"`.
#' @return A `MemoryDaf` instance.
#' @export
#' @examples
#' d <- memory_daf(name = "scratch")
#' add_axis(d, "cell", c("A", "B", "C"))
#' set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
memory_daf <- function(name = "memory") {
    stopifnot(is.character(name), length(name) == 1L, !is.na(name))
    internal <- new_internal_env()
    internal$scalars <- new.env(parent = emptyenv())
    internal$axes <- new.env(parent = emptyenv())
    internal$vectors <- new.env(parent = emptyenv())
    internal$matrices <- new.env(parent = emptyenv())
    MemoryDaf(
        name                   = name,
        internal               = internal,
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
}

#' Concrete `DafWriter` backed by R environments (no I/O).
#'
#' Use `memory_daf()` to construct instances — the S7 constructor is
#' exported only for `isVirtualClass`-style checks.
#'
#' @inheritParams DafReader
#' @examples
#' d <- memory_daf()
#' inherits(d, "dafr::MemoryDaf")
#' @export
MemoryDaf <- S7::new_class(
    name    = "MemoryDaf",
    package = "dafr",
    parent  = DafWriter
)

# ---- Scalars: query ---------------------------------------------------------

S7::method(
    format_has_scalar,
    list(MemoryDaf, S7::class_character)
) <- function(daf, name) {
    exists(name, envir = S7::prop(daf, "internal")$scalars, inherits = FALSE)
}

S7::method(
    format_get_scalar,
    list(MemoryDaf, S7::class_character)
) <- function(daf, name) {
    scalars <- S7::prop(daf, "internal")$scalars
    if (!exists(name, envir = scalars, inherits = FALSE)) {
        stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
    }
    .cache_group_value(
        get(name, envir = scalars, inherits = FALSE),
        MEMORY_DATA
    )
}

S7::method(format_scalars_set, MemoryDaf) <- function(daf) {
    sort(ls(S7::prop(daf, "internal")$scalars, all.names = TRUE),
        method = "radix"
    )
}

# ---- Scalars: mutation ------------------------------------------------------

S7::method(
    format_set_scalar,
    list(MemoryDaf, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    .assert_scalar_value(name, value)
    scalars <- S7::prop(daf, "internal")$scalars
    if (exists(name, envir = scalars, inherits = FALSE) && !overwrite) {
        stop(sprintf("scalar %s already exists; use overwrite = TRUE", sQuote(name)), call. = FALSE)
    }
    assign(name, value, envir = scalars)
    invisible()
}

S7::method(
    format_delete_scalar,
    list(MemoryDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    scalars <- S7::prop(daf, "internal")$scalars
    if (!exists(name, envir = scalars, inherits = FALSE)) {
        if (must_exist) {
            stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
        }
        return(invisible())
    }
    rm(list = name, envir = scalars)
    invisible()
}

# ---- Axes: query methods ----------------------------------------------------

S7::method(format_has_axis, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
    exists(axis, envir = S7::prop(daf, "internal")$axes, inherits = FALSE)
}

S7::method(format_axes_set, MemoryDaf) <- function(daf) {
    nms <- ls(S7::prop(daf, "internal")$axes, all.names = TRUE)
    sort(nms)
}

.memory_axis <- function(daf, axis) {
    axes <- S7::prop(daf, "internal")$axes
    if (!exists(axis, envir = axes, inherits = FALSE)) {
        stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    }
    get(axis, envir = axes, inherits = FALSE)
}

S7::method(format_axis_length, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
    length(.memory_axis(daf, axis)$entries)
}

S7::method(format_axis_array, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
    .cache_group_value(.memory_axis(daf, axis)$entries, MEMORY_DATA)
}

S7::method(format_axis_dict, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
    .memory_axis(daf, axis)$dict
}

# ---- Axes: mutation ---------------------------------------------------------

S7::method(
    format_add_axis,
    list(MemoryDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, entries) {
    if (!is.character(entries)) {
        stop(sprintf("axis %s entries must be a character vector", sQuote(axis)), call. = FALSE)
    }
    if (anyNA(entries)) {
        stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
    }
    if (any(!nzchar(entries))) {
        stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
    }
    if (anyDuplicated(entries)) {
        dup <- entries[duplicated(entries)][1L]
        stop(sprintf("axis %s has duplicate entry %s", sQuote(axis), sQuote(dup)), call. = FALSE)
    }
    axes <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = axes, inherits = FALSE)) {
        stop(sprintf("axis %s already exists", sQuote(axis)), call. = FALSE)
    }
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    assign(axis, list(entries = entries, dict = dict), envir = axes)
    bump_axis_counter(daf, axis)
    invisible()
}

S7::method(
    format_delete_axis,
    list(MemoryDaf, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    internal <- S7::prop(daf, "internal")
    if (!exists(axis, envir = internal$axes, inherits = FALSE)) {
        if (must_exist) {
            stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
        }
        return(invisible())
    }
    # Drop axis + its dependent vectors + matrix rows + matrix cols
    rm(list = axis, envir = internal$axes)
    if (exists(axis, envir = internal$vectors, inherits = FALSE)) {
        rm(list = axis, envir = internal$vectors)
    }
    if (exists(axis, envir = internal$matrices, inherits = FALSE)) {
        rm(list = axis, envir = internal$matrices)
    }
    for (rows in ls(internal$matrices, all.names = TRUE)) {
        cols_env <- get(rows, envir = internal$matrices, inherits = FALSE)
        if (exists(axis, envir = cols_env, inherits = FALSE)) {
            rm(list = axis, envir = cols_env)
        }
    }
    bump_axis_counter(daf, axis)
    invisible()
}

# ---- Vectors: query ---------------------------------------------------------

.memory_axis_vectors <- function(daf, axis, create = FALSE) {
    if (!format_has_axis(daf, axis)) {
        stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    }
    vectors <- S7::prop(daf, "internal")$vectors
    if (exists(axis, envir = vectors, inherits = FALSE)) {
        return(get(axis, envir = vectors, inherits = FALSE))
    }
    if (create) {
        new_env <- new.env(parent = emptyenv())
        assign(axis, new_env, envir = vectors)
        return(new_env)
    }
    NULL
}

S7::method(
    format_has_vector,
    list(MemoryDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    if (!format_has_axis(daf, axis)) {
        return(FALSE)
    }
    env <- .memory_axis_vectors(daf, axis, create = FALSE)
    if (is.null(env)) {
        return(FALSE)
    }
    exists(name, envir = env, inherits = FALSE)
}

S7::method(
    format_vectors_set,
    list(MemoryDaf, S7::class_character)
) <- function(daf, axis) {
    env <- .memory_axis_vectors(daf, axis, create = FALSE)
    if (is.null(env)) {
        return(character(0L))
    }
    sort(ls(env, all.names = TRUE), method = "radix")
}

S7::method(
    format_get_vector,
    list(MemoryDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    env <- .memory_axis_vectors(daf, axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        stop(sprintf(
            "vector %s does not exist on axis %s",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    .cache_group_value(
        get(name, envir = env, inherits = FALSE),
        MEMORY_DATA
    )
}

# ---- Vectors: mutation ------------------------------------------------------

S7::method(
    format_set_vector,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, axis, name, vec, overwrite) {
    vec <- .validate_vector_value(daf, axis, name, vec)
    env <- .memory_axis_vectors(daf, axis, create = TRUE)
    if (exists(name, envir = env, inherits = FALSE) && !overwrite) {
        stop(sprintf(
            "vector %s already exists on axis %s; use overwrite = TRUE",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    assign(name, vec, envir = env)
    bump_vector_counter(daf, axis, name)
    invisible()
}

S7::method(
    format_delete_vector,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, axis, name, must_exist) {
    env <- .memory_axis_vectors(daf, axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        if (must_exist) {
            stop(sprintf(
                "vector %s does not exist on axis %s",
                sQuote(name), sQuote(axis)
            ), call. = FALSE)
        }
        return(invisible())
    }
    rm(list = name, envir = env)
    bump_vector_counter(daf, axis, name)
    invisible()
}

# ---- Matrices: query --------------------------------------------------------

.memory_matrix_bucket <- function(daf, rows_axis, columns_axis, create = FALSE) {
    if (!format_has_axis(daf, rows_axis)) {
        stop(sprintf("axis %s does not exist", sQuote(rows_axis)), call. = FALSE)
    }
    if (!format_has_axis(daf, columns_axis)) {
        stop(sprintf("axis %s does not exist", sQuote(columns_axis)), call. = FALSE)
    }
    matrices <- S7::prop(daf, "internal")$matrices
    if (!exists(rows_axis, envir = matrices, inherits = FALSE)) {
        if (!create) {
            return(NULL)
        }
        assign(rows_axis, new.env(parent = emptyenv()), envir = matrices)
    }
    rows_env <- get(rows_axis, envir = matrices, inherits = FALSE)
    if (!exists(columns_axis, envir = rows_env, inherits = FALSE)) {
        if (!create) {
            return(NULL)
        }
        assign(columns_axis, new.env(parent = emptyenv()), envir = rows_env)
    }
    get(columns_axis, envir = rows_env, inherits = FALSE)
}

S7::method(
    format_has_matrix,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    if (!format_has_axis(daf, rows_axis) || !format_has_axis(daf, columns_axis)) {
        return(FALSE)
    }
    env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
    if (is.null(env)) {
        return(FALSE)
    }
    exists(name, envir = env, inherits = FALSE)
}

S7::method(
    format_matrices_set,
    list(MemoryDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
    if (is.null(env)) {
        return(character(0L))
    }
    sort(ls(env, all.names = TRUE), method = "radix")
}

S7::method(
    format_get_matrix,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        stop(
            sprintf(
                "matrix %s does not exist on axes (%s, %s)",
                sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
            ),
            call. = FALSE
        )
    }
    .cache_group_value(
        get(name, envir = env, inherits = FALSE),
        MEMORY_DATA
    )
}

# ---- Matrices: mutation -----------------------------------------------------

.validate_matrix_value <- function(daf, rows_axis, columns_axis, name, mat) {
    is_sparse <- methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")
    is_dense <- is.matrix(mat)
    if (!is_sparse && !is_dense) {
        stop(sprintf(
            "matrix %s is not a matrix or dgCMatrix/lgCMatrix",
            sQuote(name)
        ), call. = FALSE)
    }
    nr <- format_axis_length(daf, rows_axis)
    nc <- format_axis_length(daf, columns_axis)
    d <- dim(mat)
    if (d[[1L]] != nr || d[[2L]] != nc) {
        stop(sprintf(
            "matrix %s has dim %d x %d (expected %d x %d)",
            sQuote(name), d[[1L]], d[[2L]], nr, nc
        ), call. = FALSE)
    }
    if (is_dense) {
        dimnames(mat) <- NULL
    } else {
        mat@Dimnames <- list(NULL, NULL)
    }
    mat
}

S7::method(
    format_set_matrix,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    mat <- .validate_matrix_value(daf, rows_axis, columns_axis, name, mat)
    env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = TRUE)
    if (exists(name, envir = env, inherits = FALSE) && !overwrite) {
        stop(
            sprintf(
                "matrix %s already exists on axes (%s, %s); use overwrite = TRUE",
                sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
            ),
            call. = FALSE
        )
    }
    assign(name, mat, envir = env)
    bump_matrix_counter(daf, rows_axis, columns_axis, name)
    invisible()
}

S7::method(
    format_delete_matrix,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, must_exist) {
    env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        if (must_exist) {
            stop(
                sprintf(
                    "matrix %s does not exist on axes (%s, %s)",
                    sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
                ),
                call. = FALSE
            )
        }
        return(invisible())
    }
    rm(list = name, envir = env)
    bump_matrix_counter(daf, rows_axis, columns_axis, name)
    invisible()
}

S7::method(
    format_relayout_matrix,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    src <- format_get_matrix(daf, rows_axis, columns_axis, name)
    transposed <- if (methods::is(src, "dgCMatrix") || methods::is(src, "lgCMatrix")) {
        Matrix::t(src)
    } else {
        t(src)
    }
    format_set_matrix(daf, columns_axis, rows_axis, name, transposed, overwrite = TRUE)
    invisible()
}
