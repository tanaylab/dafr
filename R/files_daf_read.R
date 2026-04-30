#' @include files_daf.R files_io.R format_api.R
NULL

.files_root <- function(daf) S7::prop(daf, "internal")$path

# Per-item cache_group classifier for files_daf reads. Mirrors upstream
# DataAxesFormats.jl `src/files_format.jl` post-49fbba1 thresholds.
#
# Upstream classifies MappedData vs MemoryData based on whether the returned
# value is mmap-backed or fully materialized into heap memory. There are NO
# size-based thresholds. MemoryData is returned for:
#   - string/character vectors (dense or sparse-string reconstructed)
#   - sparse-Bool vectors/matrices where .nzval file was absent (synthesized)
#   - sparse-string matrices (reconstructed in-memory)
#
# In R all reads go through readBin (no real mmap), but we mirror the upstream
# classification: character/factor → MEMORY_DATA; everything else → MAPPED_DATA.
# The synthesized-Bool case is indistinguishable from the returned value alone,
# so it correctly stays MAPPED_DATA (same as upstream's non-absent nzval path).
#
# CALLED ONLY from format_get_vector / format_get_matrix S7 methods on
# FilesDaf / FilesDafReadOnly. The wrap site uses the returned constant to
# populate the cache_group field.
.files_daf_classify_vector <- function(value) {
    if (is.character(value) || is.factor(value)) return(MEMORY_DATA)
    MAPPED_DATA
}

.files_daf_classify_matrix <- function(value) {
    if (is.character(value) || is.factor(value)) return(MEMORY_DATA)
    MAPPED_DATA
}

# ---- scalars: query ----
S7::method(
    format_has_scalar,
    list(FilesDaf, S7::class_character)
) <- function(daf, name) {
    file.exists(.path_scalar(.files_root(daf), name))
}
S7::method(
    format_has_scalar,
    list(FilesDafReadOnly, S7::class_character)
) <- function(daf, name) {
    file.exists(.path_scalar(.files_root(daf), name))
}

.files_get_scalar <- function(daf, name) {
    p <- .path_scalar(.files_root(daf), name)
    if (!file.exists(p)) {
        .require_scalar(daf, name)
    }
    .read_scalar_json(p)
}
S7::method(
    format_get_scalar,
    list(FilesDaf, S7::class_character)
) <- function(daf, name) {
    .cache_group_value(.files_get_scalar(daf, name), MEMORY_DATA)
}
S7::method(
    format_get_scalar,
    list(FilesDafReadOnly, S7::class_character)
) <- function(daf, name) {
    .cache_group_value(.files_get_scalar(daf, name), MEMORY_DATA)
}

.files_scalars_set <- function(daf) {
    dir <- file.path(.files_root(daf), "scalars")
    files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
    sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(format_scalars_set, FilesDaf) <- function(daf) {
    .files_scalars_set(daf)
}
S7::method(format_scalars_set, FilesDafReadOnly) <- function(daf) {
    .files_scalars_set(daf)
}

# ---- axes: query ----

.files_axis_parsed <- function(daf, axis) {
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) {
        return(get(axis, envir = cache, inherits = FALSE))
    }
    p <- .path_axis(.files_root(daf), axis)
    if (!file.exists(p)) {
        return(NULL)
    }
    entries <- readLines(p, encoding = "UTF-8", warn = FALSE)
    if (anyNA(entries) || any(!nzchar(entries))) {
        stop(sprintf("files_daf: axis %s contains empty entries", sQuote(axis)),
            call. = FALSE
        )
    }
    if (anyDuplicated(entries)) {
        stop(sprintf(
            "non-unique entries for new axis: %s\nin the daf data: %s",
            axis, S7::prop(daf, "name")
        ), call. = FALSE)
    }
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    parsed <- list(entries = entries, dict = dict)
    assign(axis, parsed, envir = cache)
    parsed
}

.files_has_axis <- function(daf, axis) {
    file.exists(.path_axis(.files_root(daf), axis))
}

S7::method(
    format_has_axis,
    list(FilesDaf, S7::class_character)
) <- function(daf, axis) {
    .files_has_axis(daf, axis)
}
S7::method(
    format_has_axis,
    list(FilesDafReadOnly, S7::class_character)
) <- function(daf, axis) {
    .files_has_axis(daf, axis)
}

.files_axes_set <- function(daf) {
    dir <- file.path(.files_root(daf), "axes")
    files <- list.files(dir, pattern = "\\.txt$", full.names = FALSE)
    sort(sub("\\.txt$", "", files), method = "radix")
}
S7::method(format_axes_set, FilesDaf) <- function(daf) .files_axes_set(daf)
S7::method(format_axes_set, FilesDafReadOnly) <- function(daf) .files_axes_set(daf)

.files_axis_require <- function(daf, axis) {
    parsed <- .files_axis_parsed(daf, axis)
    if (is.null(parsed)) {
        .require_axis(daf, "for: files backend", axis)
    }
    parsed
}

S7::method(
    format_axis_length,
    list(FilesDaf, S7::class_character)
) <- function(daf, axis) {
    length(.files_axis_require(daf, axis)$entries)
}
S7::method(
    format_axis_length,
    list(FilesDafReadOnly, S7::class_character)
) <- function(daf, axis) {
    length(.files_axis_require(daf, axis)$entries)
}

S7::method(
    format_axis_array,
    list(FilesDaf, S7::class_character)
) <- function(daf, axis) {
    .cache_group_value(.files_axis_require(daf, axis)$entries, MEMORY_DATA)
}
S7::method(
    format_axis_array,
    list(FilesDafReadOnly, S7::class_character)
) <- function(daf, axis) {
    .cache_group_value(.files_axis_require(daf, axis)$entries, MEMORY_DATA)
}

S7::method(
    format_axis_dict,
    list(FilesDaf, S7::class_character)
) <- function(daf, axis) {
    .files_axis_require(daf, axis)$dict
}
S7::method(
    format_axis_dict,
    list(FilesDafReadOnly, S7::class_character)
) <- function(daf, axis) {
    .files_axis_require(daf, axis)$dict
}

# ---- vectors: query ----

.files_vector_desc_path <- function(root, axis, name) {
    file.path(.path_vector_dir(root, axis), paste0(name, ".json"))
}

.files_has_vector <- function(daf, axis, name) {
    if (!format_has_axis(daf, axis)) {
        return(FALSE)
    }
    file.exists(.files_vector_desc_path(.files_root(daf), axis, name))
}
S7::method(
    format_has_vector,
    list(FilesDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    .files_has_vector(daf, axis, name)
}
S7::method(
    format_has_vector,
    list(FilesDafReadOnly, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    .files_has_vector(daf, axis, name)
}

.files_vectors_set <- function(daf, axis) {
    if (!format_has_axis(daf, axis)) {
        return(character(0L))
    }
    dir <- .path_vector_dir(.files_root(daf), axis)
    if (!dir.exists(dir)) {
        return(character(0L))
    }
    files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
    sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(
    format_vectors_set,
    list(FilesDaf, S7::class_character)
) <- function(daf, axis) {
    .files_vectors_set(daf, axis)
}
S7::method(
    format_vectors_set,
    list(FilesDafReadOnly, S7::class_character)
) <- function(daf, axis) {
    .files_vectors_set(daf, axis)
}

# ---- vectors: read ----

.files_get_vector_dense_string <- function(daf, axis, name, n) {
    dir <- .path_vector_dir(.files_root(daf), axis)
    txt <- file.path(dir, paste0(name, ".txt"))
    if (!file.exists(txt)) {
        stop(sprintf("files_daf: missing payload %s", sQuote(txt)), call. = FALSE)
    }
    out <- readLines(txt, encoding = "UTF-8", warn = FALSE)
    if (length(out) != n) {
        stop(sprintf(
            "files_daf: string vector %s has %d entries (expected %d)",
            sQuote(name), length(out), n
        ), call. = FALSE)
    }
    out
}

.files_get_vector_dense <- function(daf, axis, name, desc, n) {
    root <- .files_root(daf)
    dir <- .path_vector_dir(root, axis)
    elt <- desc$eltype
    if (elt == "String") {
        return(.files_get_vector_dense_string(daf, axis, name, n))
    }
    data_path <- file.path(dir, paste0(name, ".data"))
    if (!file.exists(data_path)) {
        stop(sprintf("files_daf: missing payload %s", sQuote(data_path)),
            call. = FALSE
        )
    }
    expected <- n * .dtype_size(elt)
    actual <- file.size(data_path)
    if (actual < expected) {
        stop(sprintf(
            "files_daf: vector %s payload truncated (%d < %d bytes)",
            sQuote(name), actual, expected
        ), call. = FALSE)
    }
    use_mmap <- isTRUE(dafr_opt("dafr.mmap"))
    if (use_mmap && elt == "Float64") {
        return(mmap_real(data_path, n))
    }
    if (use_mmap && elt == "Int32") {
        return(mmap_int(data_path, n))
    }
    # Fallback: eager read for all other types and when mmap disabled.
    .read_bin_dense(data_path, n, elt)
}

.files_get_vector_sparse <- function(daf, axis, name, desc, n) {
    vdir <- .path_vector_dir(.files_root(daf), axis)
    ind_path <- file.path(vdir, paste0(name, ".nzind"))
    if (!file.exists(ind_path)) {
        stop(sprintf("files_daf: sparse vector %s missing .nzind", sQuote(name)),
            call. = FALSE
        )
    }
    indtype <- desc$indtype %||% "UInt32"
    nnz <- file.size(ind_path) %/% .dtype_size(indtype)
    idx <- .read_bin_dense(ind_path, nnz, indtype)
    if (desc$eltype == "Bool") {
        val_path <- file.path(vdir, paste0(name, ".nzval"))
        vals <- if (file.exists(val_path)) {
            as.logical(.read_bin_dense(val_path, nnz, "Bool"))
        } else {
            rep(TRUE, nnz)
        }
        out <- logical(n)
        out[as.integer(idx)] <- vals
        return(out)
    }
    if (desc$eltype == "String") {
        nztxt <- file.path(vdir, paste0(name, ".nztxt"))
        vals <- readLines(nztxt, encoding = "UTF-8", warn = FALSE)
        if (length(vals) != nnz) {
            stop(sprintf(
                "files_daf: sparse string vector %s .nztxt has %d lines (expected %d)",
                sQuote(name), length(vals), nnz
            ), call. = FALSE)
        }
        out <- rep("", n)
        out[as.integer(idx)] <- vals
        return(out)
    }
    val_path <- file.path(vdir, paste0(name, ".nzval"))
    if (!file.exists(val_path)) {
        stop(sprintf(
            "files_daf: sparse vector %s missing .nzval for non-Bool eltype",
            sQuote(name)
        ), call. = FALSE)
    }
    vals <- .read_bin_dense(val_path, nnz, desc$eltype)
    out <- if (desc$eltype %in% c("Int8", "Int16", "Int32", "UInt8", "UInt16", "UInt32")) {
        integer(n)
    } else if (desc$eltype %in% c("Int64", "UInt64")) {
        bit64::as.integer64(integer(n))
    } else {
        numeric(n)
    }
    out[as.integer(idx)] <- vals
    out
}

.files_get_vector_impl <- function(daf, axis, name) {
    root <- .files_root(daf)
    desc_path <- .files_vector_desc_path(root, axis, name)
    if (!file.exists(desc_path)) {
        .require_vector(daf, axis, name)
    }
    desc <- .read_descriptor(desc_path)
    n <- format_axis_length(daf, axis)
    if (desc$format == "dense") {
        return(.files_get_vector_dense(daf, axis, name, desc, n))
    }
    if (desc$format == "sparse") {
        return(.files_get_vector_sparse(daf, axis, name, desc, n))
    }
    stop(sprintf("files_daf: unsupported vector format %s", desc$format),
        call. = FALSE
    )
}

.files_get_vector_cached <- function(daf, axis, name) {
    ce <- S7::prop(daf, "cache")
    key <- cache_key_vector(axis, name)
    stamp <- vector_stamp(daf, axis, name)
    hit <- cache_lookup(ce, "mapped", key, stamp)
    if (!is.null(hit)) {
        return(hit)
    }
    v <- .files_get_vector_impl(daf, axis, name)
    cache_store(ce, "mapped", key, v, stamp, size_bytes = 0)
    v
}

S7::method(
    format_get_vector,
    list(FilesDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    v <- .files_get_vector_cached(daf, axis, name)
    .cache_group_value(v, .files_daf_classify_vector(v))
}
S7::method(
    format_get_vector,
    list(FilesDafReadOnly, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    v <- .files_get_vector_cached(daf, axis, name)
    .cache_group_value(v, .files_daf_classify_vector(v))
}

# ---- matrices: query ----

.files_matrix_desc_path <- function(root, rows_axis, columns_axis, name) {
    file.path(
        .path_matrix_dir(root, rows_axis, columns_axis),
        paste0(name, ".json")
    )
}

.files_has_matrix <- function(daf, rows_axis, columns_axis, name) {
    if (!format_has_axis(daf, rows_axis) ||
        !format_has_axis(daf, columns_axis)) {
        return(FALSE)
    }
    file.exists(.files_matrix_desc_path(
        .files_root(daf),
        rows_axis, columns_axis, name
    ))
}
S7::method(
    format_has_matrix,
    list(FilesDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    .files_has_matrix(daf, rows_axis, columns_axis, name)
}
S7::method(
    format_has_matrix,
    list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    .files_has_matrix(daf, rows_axis, columns_axis, name)
}

.files_matrices_set <- function(daf, rows_axis, columns_axis) {
    if (!format_has_axis(daf, rows_axis) ||
        !format_has_axis(daf, columns_axis)) {
        return(character(0L))
    }
    dir <- .path_matrix_dir(.files_root(daf), rows_axis, columns_axis)
    if (!dir.exists(dir)) {
        return(character(0L))
    }
    files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
    sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(
    format_matrices_set,
    list(FilesDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    .files_matrices_set(daf, rows_axis, columns_axis)
}
S7::method(
    format_matrices_set,
    list(FilesDafReadOnly, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    .files_matrices_set(daf, rows_axis, columns_axis)
}

# ---- matrices: read ----

.files_get_matrix_impl <- function(daf, rows_axis, columns_axis, name) {
    root <- .files_root(daf)
    desc_path <- .files_matrix_desc_path(root, rows_axis, columns_axis, name)
    if (!file.exists(desc_path)) {
        .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
    }
    desc <- .read_descriptor(desc_path)
    nr <- format_axis_length(daf, rows_axis)
    nc <- format_axis_length(daf, columns_axis)
    if (desc$format == "dense") {
        return(.files_get_matrix_dense(daf, rows_axis, columns_axis, name, desc, nr, nc))
    }
    if (desc$format == "sparse") {
        return(.files_get_matrix_sparse(daf, rows_axis, columns_axis, name, desc, nr, nc))
    }
    stop(sprintf("files_daf: unsupported matrix format %s", desc$format),
        call. = FALSE
    )
}

.files_get_matrix_dense <- function(daf, rows_axis, columns_axis, name,
                                    desc, nr, nc) {
    root <- .files_root(daf)
    mdir <- .path_matrix_dir(root, rows_axis, columns_axis)
    elt <- desc$eltype
    if (elt == "String") {
        return(.files_get_matrix_dense_string(mdir, name, nr, nc))
    }
    data_path <- file.path(mdir, paste0(name, ".data"))
    if (!file.exists(data_path)) {
        stop(sprintf("files_daf: missing payload %s", sQuote(data_path)),
            call. = FALSE
        )
    }
    total <- as.integer(nr) * as.integer(nc)
    expected_bytes <- total * .dtype_size(elt)
    if (file.size(data_path) < expected_bytes) {
        stop(
            sprintf(
                "files_daf: matrix %s payload truncated (%d < %d bytes)",
                sQuote(name), file.size(data_path), expected_bytes
            ),
            call. = FALSE
        )
    }
    use_mmap <- isTRUE(dafr_opt("dafr.mmap"))
    v <- if (use_mmap && elt == "Float64") {
        mmap_real(data_path, total)
    } else if (use_mmap && elt == "Int32") {
        mmap_int(data_path, total)
    } else {
        .read_bin_dense(data_path, total, elt)
    }
    dim(v) <- c(as.integer(nr), as.integer(nc))
    v
}

.files_get_matrix_dense_string <- function(mdir, name, nr, nc) {
    txt <- file.path(mdir, paste0(name, ".txt"))
    if (!file.exists(txt)) {
        stop(sprintf("files_daf: missing payload %s", sQuote(txt)), call. = FALSE)
    }
    vals <- readLines(txt, encoding = "UTF-8", warn = FALSE)
    expected <- nr * nc
    if (length(vals) != expected) {
        stop(sprintf(
            "files_daf: string matrix has %d lines (expected %d)",
            length(vals), expected
        ), call. = FALSE)
    }
    matrix(vals, nrow = nr, ncol = nc)
}

.files_get_matrix_sparse <- function(daf, rows_axis, columns_axis, name,
                                     desc, nr, nc) {
    mdir <- .path_matrix_dir(.files_root(daf), rows_axis, columns_axis)
    indtype <- desc$indtype %||% "UInt32"
    colptr_path <- file.path(mdir, paste0(name, ".colptr"))
    rowval_path <- file.path(mdir, paste0(name, ".rowval"))
    nzval_path <- file.path(mdir, paste0(name, ".nzval"))
    if (!file.exists(colptr_path) || !file.exists(rowval_path)) {
        stop(sprintf(
            "files_daf: sparse matrix %s missing colptr/rowval",
            sQuote(name)
        ), call. = FALSE)
    }
    colptr <- .read_bin_dense(colptr_path, as.integer(nc) + 1L, indtype)
    nnz <- as.integer(colptr[length(colptr)]) - 1L
    rowval <- if (nnz > 0L) {
        .read_bin_dense(rowval_path, nnz, indtype)
    } else {
        integer(0L)
    }
    if (desc$eltype == "Bool") {
        vals <- if (file.exists(nzval_path)) {
            as.logical(.read_bin_dense(nzval_path, nnz, "Bool"))
        } else {
            rep(TRUE, nnz)
        }
        return(methods::new("lgCMatrix",
            x = vals,
            i = as.integer(rowval) - 1L,
            p = as.integer(colptr) - 1L,
            Dim = c(as.integer(nr), as.integer(nc)),
            Dimnames = list(NULL, NULL)
        ))
    }
    if (!file.exists(nzval_path)) {
        stop(sprintf(
            "files_daf: sparse matrix %s missing .nzval for non-Bool",
            sQuote(name)
        ), call. = FALSE)
    }
    vals <- if (nnz > 0L) {
        .read_bin_dense(nzval_path, nnz, desc$eltype)
    } else {
        double(0L)
    }
    methods::new("dgCMatrix",
        x = as.double(vals),
        i = as.integer(rowval) - 1L,
        p = as.integer(colptr) - 1L,
        Dim = c(as.integer(nr), as.integer(nc)),
        Dimnames = list(NULL, NULL)
    )
}

.files_get_matrix_cached <- function(daf, rows_axis, columns_axis, name) {
    ce <- S7::prop(daf, "cache")
    key <- cache_key_matrix(rows_axis, columns_axis, name)
    stamp <- matrix_stamp(daf, rows_axis, columns_axis, name)
    hit <- cache_lookup(ce, "mapped", key, stamp)
    if (!is.null(hit)) {
        return(hit)
    }
    m <- .files_get_matrix_impl(daf, rows_axis, columns_axis, name)
    cache_store(ce, "mapped", key, m, stamp, size_bytes = 0)
    m
}

S7::method(
    format_get_matrix,
    list(FilesDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    m <- .files_get_matrix_cached(daf, rows_axis, columns_axis, name)
    .cache_group_value(m, .files_daf_classify_matrix(m))
}
S7::method(
    format_get_matrix,
    list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    m <- .files_get_matrix_cached(daf, rows_axis, columns_axis, name)
    .cache_group_value(m, .files_daf_classify_matrix(m))
}
