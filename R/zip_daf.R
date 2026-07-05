#' @include classes.R files_daf.R files_daf_read.R files_daf_write.R files_io.R files_packed.R format_api.R zarr_store.R
NULL

# ZipDaf: a whole Daf store in one append-only `.daf.zip` archive, byte-
# compatible with DataAxesFormats.jl `ZipDaf`. Same on-disk layout as FilesDaf
# (daf.json marker + scalars/axes/vectors/matrices keys, identical component
# serialization) but the keys live inside the archive's MmapZipStore instead of
# a directory, and there is NO metadata.json (the ZIP central directory is the
# enumeration index). The archive is append-only: overwrite / delete / reorder
# raise a clean error. Serialization is shared with FilesDaf via the pure cores
# in files_io.R; only the container I/O (store_get_bytes / store_set_bytes /
# store_exists / store_list) differs.

#' File-backed (zip) Daf writer class.
#'
#' Concrete `DafWriter` subclass instantiated by [zip_daf()] for writable modes
#' (`"r+"`, `"w"`, `"w+"`). The archive is append-only. Use [zip_daf()] to
#' construct instances.
#' @inheritParams DafReader
#' @export
ZipDaf <- S7::new_class(name = "ZipDaf", package = "dafr", parent = DafWriter)

#' File-backed (zip) read-only Daf class.
#'
#' Concrete `DafReadOnly` subclass instantiated by [zip_daf()] with mode `"r"`.
#' @inheritParams DafReader
#' @export
ZipDafReadOnly <- S7::new_class(
    name = "ZipDafReadOnly", package = "dafr", parent = DafReadOnly
)

.zip_store <- function(daf) S7::prop(daf, "internal")$store

.zip_read_only_guard <- function(verb) {
    stop(sprintf("zip_daf: store opened read-only; %s not permitted", verb),
        call. = FALSE
    )
}
.zip_append_only_guard <- function(verb) {
    stop(sprintf("zip_daf: archive is append-only; %s not permitted", verb),
        call. = FALSE
    )
}

# ---- key builders (relative archive keys) ----
.zkey_scalar <- function(name) paste0("scalars/", name, ".json")
.zkey_axis <- function(axis) paste0("axes/", axis, ".txt")
.zkey_vector <- function(axis, name, ext = "") paste0("vectors/", axis, "/", name, ext)
.zkey_matrix <- function(ra, ca, name, ext = "") {
    paste0("matrices/", ra, "/", ca, "/", name, ext)
}

# Direct-child names under `prefix` whose key ends in `ext` (e.g. ".json"),
# with the prefix and ext stripped. Robust to store_list's prefix semantics:
# re-filters with startsWith(prefix/) and drops any nested key.
.zip_names_in <- function(store, prefix, ext) {
    keys <- store_list(store, prefix)
    pfx <- paste0(prefix, "/")
    keep <- startsWith(keys, pfx) & endsWith(keys, ext)
    if (!any(keep)) return(character(0L))
    nm <- substring(keys[keep], nchar(pfx) + 1L)
    nm <- substring(nm, 1L, nchar(nm) - nchar(ext))
    nm <- nm[!grepl("/", nm, fixed = TRUE)]
    sort(nm, method = "radix")
}

# ==== scalars: read ==========================================================

.zip_has_scalar <- function(daf, name) {
    store_exists(.zip_store(daf), .zkey_scalar(name))
}
.zip_get_scalar <- function(daf, name) {
    store <- .zip_store(daf)
    key <- .zkey_scalar(name)
    if (!store_exists(store, key)) .require_scalar(daf, name)
    .decode_scalar_json(store_get_bytes(store, key))
}
.zip_scalars_set <- function(daf) .zip_names_in(.zip_store(daf), "scalars", ".json")

# ==== axes: read =============================================================

.zip_axis_parsed <- function(daf, axis) {
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) {
        return(get(axis, envir = cache, inherits = FALSE))
    }
    store <- .zip_store(daf)
    key <- .zkey_axis(axis)
    if (!store_exists(store, key)) return(NULL)
    entries <- .decode_lines(store_get_bytes(store, key))
    if (anyNA(entries) || any(!nzchar(entries))) {
        stop(sprintf("zip_daf: axis %s contains empty entries", sQuote(axis)),
            call. = FALSE)
    }
    if (anyDuplicated(entries)) {
        stop(sprintf("non-unique entries for new axis: %s\nin the daf data: %s",
            axis, S7::prop(daf, "name")), call. = FALSE)
    }
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    parsed <- list(entries = entries, dict = dict)
    assign(axis, parsed, envir = cache)
    parsed
}
.zip_has_axis <- function(daf, axis) store_exists(.zip_store(daf), .zkey_axis(axis))
.zip_axes_set <- function(daf) .zip_names_in(.zip_store(daf), "axes", ".txt")
.zip_axis_require <- function(daf, axis) {
    parsed <- .zip_axis_parsed(daf, axis)
    if (is.null(parsed)) .require_axis(daf, "for: zip backend", axis)
    parsed
}

# ==== vectors: read ==========================================================

.zip_has_vector <- function(daf, axis, name) {
    if (!format_has_axis(daf, axis)) return(FALSE)
    store_exists(.zip_store(daf), .zkey_vector(axis, name, ".json"))
}
.zip_vectors_set <- function(daf, axis) {
    if (!format_has_axis(daf, axis)) return(character(0L))
    .zip_names_in(.zip_store(daf), paste0("vectors/", axis), ".json")
}

.zip_get_vector_dense <- function(daf, axis, name, desc, n) {
    store <- .zip_store(daf)
    base <- .zkey_vector(axis, name)
    elt <- desc$eltype
    if (.files_is_packed(desc)) {
        return(.files_packed_decode_vector(
            store_get_bytes(store, paste0(base, ".zip")), desc, n, name))
    }
    if (elt == "String") {
        vals <- .decode_lines(store_get_bytes(store, paste0(base, ".txt")))
        if (length(vals) != n) {
            stop(sprintf("zip_daf: string vector %s has %d entries (expected %d)",
                sQuote(name), length(vals), n), call. = FALSE)
        }
        return(vals)
    }
    .decode_dense(store_get_bytes(store, paste0(base, ".data")), n, elt)
}

.zip_get_vector_sparse <- function(daf, axis, name, desc, n) {
    store <- .zip_store(daf)
    base <- .zkey_vector(axis, name)
    sd <- .files_parse_sparse_descriptor(desc, "nzind")
    indtype <- sd$indtype
    eltype <- sd$eltype
    nzind_desc <- desc$nzind
    if (!is.null(nzind_desc) && .files_is_packed(nzind_desc)) {
        nnz <- as.integer(nzind_desc$n_elements)
        idx <- .files_packed_decode_vector(
            store_get_bytes(store, paste0(base, ".nzind.zip")), nzind_desc, nnz, name)
    } else {
        b <- store_get_bytes(store, paste0(base, ".nzind"))
        nnz <- as.integer(length(b) %/% .dtype_size(indtype))
        idx <- .decode_dense(b, nnz, indtype)
    }
    if (eltype == "Bool") {
        nzd <- desc$nzval
        vals <- if (!is.null(nzd) && .files_is_packed(nzd)) {
            as.logical(.files_packed_decode_vector(
                store_get_bytes(store, paste0(base, ".nzval.zip")), nzd, nnz, name))
        } else if (store_exists(store, paste0(base, ".nzval"))) {
            as.logical(.decode_dense(store_get_bytes(store, paste0(base, ".nzval")),
                nnz, "Bool"))
        } else {
            rep(TRUE, nnz)
        }
        out <- logical(n)
        out[as.integer(idx)] <- vals
        return(out)
    }
    if (eltype == "String") {
        vals <- .decode_lines(store_get_bytes(store, paste0(base, ".nztxt")))
        if (length(vals) != nnz) {
            stop(sprintf(
                "zip_daf: sparse string vector %s .nztxt has %d lines (expected %d)",
                sQuote(name), length(vals), nnz), call. = FALSE)
        }
        out <- rep("", n)
        out[as.integer(idx)] <- vals
        return(out)
    }
    vals <- if (!is.null(desc$nzval) && .files_is_packed(desc$nzval)) {
        .files_packed_decode_vector(
            store_get_bytes(store, paste0(base, ".nzval.zip")), desc$nzval, nnz, name)
    } else {
        .decode_dense(store_get_bytes(store, paste0(base, ".nzval")), nnz, eltype)
    }
    out <- if (eltype %in% c("Int8", "Int16", "Int32", "UInt8", "UInt16", "UInt32")) {
        integer(n)
    } else if (eltype %in% c("Int64", "UInt64")) {
        bit64::as.integer64(integer(n))
    } else {
        numeric(n)
    }
    out[as.integer(idx)] <- vals
    out
}

.zip_get_vector_impl <- function(daf, axis, name) {
    store <- .zip_store(daf)
    dkey <- .zkey_vector(axis, name, ".json")
    if (!store_exists(store, dkey)) .require_vector(daf, axis, name)
    desc <- .decode_descriptor_bytes(store_get_bytes(store, dkey))
    n <- format_axis_length(daf, axis)
    if (desc$format == "dense") return(.zip_get_vector_dense(daf, axis, name, desc, n))
    if (desc$format == "sparse") return(.zip_get_vector_sparse(daf, axis, name, desc, n))
    stop(sprintf("zip_daf: unsupported vector format %s", desc$format), call. = FALSE)
}

# ==== matrices: read =========================================================

.zip_has_matrix <- function(daf, ra, ca, name) {
    if (!format_has_axis(daf, ra) || !format_has_axis(daf, ca)) return(FALSE)
    store_exists(.zip_store(daf), .zkey_matrix(ra, ca, name, ".json"))
}
.zip_matrices_set <- function(daf, ra, ca) {
    if (!format_has_axis(daf, ra) || !format_has_axis(daf, ca)) return(character(0L))
    .zip_names_in(.zip_store(daf), paste0("matrices/", ra, "/", ca), ".json")
}

.zip_get_matrix_dense <- function(daf, ra, ca, name, desc, nr, nc) {
    store <- .zip_store(daf)
    base <- .zkey_matrix(ra, ca, name)
    elt <- desc$eltype
    if (.files_is_packed(desc)) {
        return(.files_packed_decode_matrix(
            store_get_bytes(store, paste0(base, ".zip")), desc,
            as.integer(nr), as.integer(nc), name))
    }
    if (elt == "String") {
        vals <- .decode_lines(store_get_bytes(store, paste0(base, ".txt")))
        expected <- nr * nc
        if (length(vals) != expected) {
            stop(sprintf("zip_daf: string matrix has %d lines (expected %d)",
                length(vals), expected), call. = FALSE)
        }
        return(matrix(vals, nrow = nr, ncol = nc))
    }
    total <- as.numeric(nr) * as.numeric(nc)
    v <- .decode_dense(store_get_bytes(store, paste0(base, ".data")), total, elt)
    dim(v) <- c(as.integer(nr), as.integer(nc))
    v
}

.zip_get_matrix_sparse <- function(daf, ra, ca, name, desc, nr, nc) {
    store <- .zip_store(daf)
    base <- .zkey_matrix(ra, ca, name)
    sd <- .files_parse_sparse_descriptor(desc, "colptr")
    indtype <- sd$indtype
    eltype <- sd$eltype
    read_comp <- function(comp, comp_desc, count, type) {
        if (!is.null(comp_desc) && .files_is_packed(comp_desc)) {
            .files_packed_decode_vector(
                store_get_bytes(store, paste0(base, ".", comp, ".zip")),
                comp_desc, count, name)
        } else {
            .decode_dense(store_get_bytes(store, paste0(base, ".", comp)), count, type)
        }
    }
    colptr <- read_comp("colptr", desc$colptr, as.integer(nc) + 1L, indtype)
    nnz <- as.integer(colptr[length(colptr)]) - 1L
    rowval <- if (nnz > 0L) read_comp("rowval", desc$rowval, nnz, indtype) else integer(0L)
    if (eltype == "Bool") {
        nzd <- desc$nzval
        vals <- if (!is.null(nzd) && .files_is_packed(nzd)) {
            as.logical(read_comp("nzval", nzd, nnz, "Bool"))
        } else if (store_exists(store, paste0(base, ".nzval"))) {
            as.logical(.decode_dense(store_get_bytes(store, paste0(base, ".nzval")),
                nnz, "Bool"))
        } else {
            rep(TRUE, nnz)
        }
        return(methods::new("lgCMatrix", x = vals,
            i = as.integer(rowval) - 1L, p = as.integer(colptr) - 1L,
            Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL)))
    }
    vals <- if (nnz > 0L) read_comp("nzval", desc$nzval, nnz, eltype) else double(0L)
    methods::new("dgCMatrix", x = as.double(vals),
        i = as.integer(rowval) - 1L, p = as.integer(colptr) - 1L,
        Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL))
}

.zip_get_matrix_impl <- function(daf, ra, ca, name) {
    store <- .zip_store(daf)
    dkey <- .zkey_matrix(ra, ca, name, ".json")
    if (!store_exists(store, dkey)) {
        .require_matrix(daf, ra, ca, name, relayout = FALSE)
    }
    desc <- .decode_descriptor_bytes(store_get_bytes(store, dkey))
    nr <- format_axis_length(daf, ra)
    nc <- format_axis_length(daf, ca)
    if (desc$format == "dense") return(.zip_get_matrix_dense(daf, ra, ca, name, desc, nr, nc))
    if (desc$format == "sparse") return(.zip_get_matrix_sparse(daf, ra, ca, name, desc, nr, nc))
    stop(sprintf("zip_daf: unsupported matrix format %s", desc$format), call. = FALSE)
}

# ==== writers (append-only) ==================================================

# Guard: an append-only store cannot replace an existing key. `!overwrite` gets
# the standard "already exists" error; overwrite=TRUE gets the append-only error.
.zip_guard_new <- function(exists, overwrite, require_fn, verb) {
    if (exists) {
        if (!overwrite) require_fn()
        .zip_append_only_guard(verb)
    }
}

.zip_set_scalar <- function(daf, name, value, overwrite) {
    .assert_scalar_value(name, value)
    store <- .zip_store(daf)
    .zip_guard_new(store_exists(store, .zkey_scalar(name)), overwrite,
        function() .require_no_scalar(daf, name), sprintf("overwrite scalar %s", name))
    store_set_bytes(store, .zkey_scalar(name), .encode_scalar_json(value))
    MEMORY_DATA
}

.zip_add_axis <- function(daf, axis, entries) {
    if (!is.character(entries)) {
        stop(sprintf("axis %s entries must be a character vector", sQuote(axis)), call. = FALSE)
    }
    if (anyNA(entries)) {
        stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
    }
    if (any(!nzchar(entries))) {
        stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
    }
    if (any(grepl("[\n\r]", entries))) {
        stop(sprintf("axis %s entries contain newline characters", sQuote(axis)), call. = FALSE)
    }
    if (anyDuplicated(entries)) {
        stop(sprintf("non-unique entries for new axis: %s\nin the daf data: %s",
            axis, S7::prop(daf, "name")), call. = FALSE)
    }
    if (length(entries) > .Machine$integer.max) {
        stop(sprintf("axis %s length exceeds R integer capacity", sQuote(axis)), call. = FALSE)
    }
    .require_no_axis(daf, axis)
    store_set_bytes(.zip_store(daf), .zkey_axis(axis), .encode_lines(entries))
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    assign(axis, list(entries = entries, dict = dict),
        envir = S7::prop(daf, "internal")$axes)
    invisible()
}

# Write one sparse component to the store (packed or flat) and return its
# descriptor-entry for .descriptor_sparse_string. Mirrors .files_write_component
# + .files_sparse_comp_entry but stores bytes.
.zip_write_sparse_comp <- function(store, base, comp, values, dtype_lc, eltype, packed) {
    r <- .files_component_bytes(paste0(".", comp), values, dtype_lc, eltype,
        packed = packed, include_n = TRUE)
    store_set_bytes(store, paste0(base, r$ext), r$bytes)
    if (r$ext == ".zip") list(key = comp, desc = r$desc)
    else list(key = comp, eltype = eltype, n_elements = as.integer(length(values)))
}

.zip_write_vector <- function(daf, axis, name, vec) {
    store <- .zip_store(daf)
    base <- .zkey_vector(axis, name)
    packed <- .files_is_packed_writer(daf)
    eltype <- .dtype_for_r_vector(vec)
    n <- length(vec)
    indtype <- .indtype_for_size(n)
    go_sparse <- if (eltype == "String") {
        .should_sparsify_string(vec, indtype)
    } else {
        .should_sparsify_numeric(vec, eltype, indtype)
    }
    if (!go_sparse) {
        if (eltype == "String") {
            store_set_bytes(store, paste0(base, ".txt"), .encode_lines(vec))
            store_set_bytes(store, paste0(base, ".json"),
                charToRaw(.descriptor_dense_string(eltype)))
        } else {
            r <- .files_component_bytes(".data", vec, tolower(eltype), eltype,
                packed = packed, include_n = FALSE)
            store_set_bytes(store, paste0(base, r$ext), r$bytes)
            if (r$ext == ".zip") {
                store_set_bytes(store, paste0(base, ".json"),
                    charToRaw(.descriptor_packed_string(r$desc)))
            } else {
                store_set_bytes(store, paste0(base, ".json"),
                    charToRaw(.descriptor_dense_string(eltype)))
            }
        }
        return(invisible())
    }
    if (eltype == "String") {
        nz <- which(nzchar(vec))
        r <- .files_component_bytes(".nzind", as.integer(nz), tolower(indtype),
            indtype, packed = FALSE, include_n = TRUE)
        store_set_bytes(store, paste0(base, ".nzind"), r$bytes)
        store_set_bytes(store, paste0(base, ".nztxt"), .encode_lines(vec[nz]))
        comps <- list(list(key = "nzind", eltype = indtype, n_elements = length(nz)),
                      list(key = "nzval", eltype = "String", n_elements = length(nz)))
        store_set_bytes(store, paste0(base, ".json"),
            charToRaw(.descriptor_sparse_string(comps)))
        return(invisible())
    }
    nz <- if (is.logical(vec)) which(vec) else which(is.nan(vec) | vec != 0)
    comps <- list(.zip_write_sparse_comp(store, base, "nzind", as.integer(nz),
        tolower(indtype), indtype, packed))
    if (eltype == "Bool") {
        if (!all(vec[nz])) {
            comps <- c(comps, list(.zip_write_sparse_comp(store, base, "nzval",
                as.logical(vec[nz]), "bool", "Bool", packed)))
        }
    } else {
        comps <- c(comps, list(.zip_write_sparse_comp(store, base, "nzval",
            vec[nz], tolower(eltype), eltype, packed)))
    }
    store_set_bytes(store, paste0(base, ".json"),
        charToRaw(.descriptor_sparse_string(comps)))
    invisible()
}

.zip_set_vector <- function(daf, axis, name, vec, overwrite) {
    if (methods::is(vec, "sparseVector")) {
        return(.zip_set_vector_sparse_input(daf, axis, name, vec, overwrite))
    }
    vec <- .validate_vector_value(daf, axis, name, vec)
    .zip_guard_new(.zip_has_vector(daf, axis, name), overwrite,
        function() .require_no_vector(daf, axis, name),
        sprintf("overwrite vector %s", name))
    .zip_write_vector(daf, axis, name, vec)
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

.zip_set_vector_sparse_input <- function(daf, axis, name, sv, overwrite) {
    n <- format_axis_length(daf, axis)
    if (sv@length != n) {
        stop(sprintf("sparseVector %s length %d (expected %d) on axis %s",
            sQuote(name), sv@length, n, sQuote(axis)), call. = FALSE)
    }
    .zip_guard_new(.zip_has_vector(daf, axis, name), overwrite,
        function() .require_no_vector(daf, axis, name),
        sprintf("overwrite vector %s", name))
    store <- .zip_store(daf)
    base <- .zkey_vector(axis, name)
    packed <- .files_is_packed_writer(daf)
    eltype <- .dtype_for_r_vector(sv@x)
    indtype <- .indtype_for_size(n)
    comps <- list(.zip_write_sparse_comp(store, base, "nzind", as.integer(sv@i),
        tolower(indtype), indtype, packed))
    if (eltype == "Bool") {
        if (!all(sv@x)) {
            comps <- c(comps, list(.zip_write_sparse_comp(store, base, "nzval",
                as.logical(sv@x), "bool", "Bool", packed)))
        }
    } else {
        comps <- c(comps, list(.zip_write_sparse_comp(store, base, "nzval",
            sv@x, tolower(eltype), eltype, packed)))
    }
    store_set_bytes(store, paste0(base, ".json"),
        charToRaw(.descriptor_sparse_string(comps)))
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

.zip_write_matrix_sparse <- function(store, base, mat, packed) {
    is_bool <- methods::is(mat, "lgCMatrix")
    nr <- nrow(mat)
    nc <- ncol(mat)
    nnz <- length(mat@x)
    indtype <- .indtype_for_size(max(nr, nc, nnz))
    comps <- list(
        .zip_write_sparse_comp(store, base, "colptr", as.integer(mat@p) + 1L,
            tolower(indtype), indtype, packed),
        .zip_write_sparse_comp(store, base, "rowval", as.integer(mat@i) + 1L,
            tolower(indtype), indtype, packed))
    if (is_bool) {
        if (!all(mat@x)) {
            comps <- c(comps, list(.zip_write_sparse_comp(store, base, "nzval",
                as.logical(mat@x), "bool", "Bool", packed)))
        }
    } else {
        comps <- c(comps, list(.zip_write_sparse_comp(store, base, "nzval",
            as.double(mat@x), "float64", "Float64", packed)))
    }
    store_set_bytes(store, paste0(base, ".json"),
        charToRaw(.descriptor_sparse_string(comps)))
    invisible()
}

.zip_set_matrix <- function(daf, ra, ca, name, mat, overwrite) {
    mat <- .validate_matrix_value(daf, ra, ca, name, mat)
    .zip_guard_new(.zip_has_matrix(daf, ra, ca, name), overwrite,
        function() .require_no_matrix(daf, ra, ca, name, relayout = FALSE),
        sprintf("overwrite matrix %s", name))
    store <- .zip_store(daf)
    base <- .zkey_matrix(ra, ca, name)
    packed <- .files_is_packed_writer(daf)
    if (methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")) {
        .zip_write_matrix_sparse(store, base, mat, packed)
        bump_matrix_counter(daf, ra, ca, name)
        return(MEMORY_DATA)
    }
    dtype <- .dtype_for_r_vector(as.vector(mat))
    if (dtype == "String") {
        store_set_bytes(store, paste0(base, ".txt"), .encode_lines(as.vector(mat)))
        store_set_bytes(store, paste0(base, ".json"),
            charToRaw(.descriptor_dense_string(dtype)))
    } else if (packed && .files_matrix_should_pack(nrow(mat), tolower(dtype))) {
        r <- .files_dense_matrix_packed_bytes(mat, tolower(dtype), dtype)
        store_set_bytes(store, paste0(base, ".zip"), r$bytes)
        store_set_bytes(store, paste0(base, ".json"),
            charToRaw(.descriptor_packed_string(r$desc)))
    } else {
        store_set_bytes(store, paste0(base, ".data"),
            .encode_dense(as.vector(mat), tolower(dtype)))
        store_set_bytes(store, paste0(base, ".json"),
            charToRaw(.descriptor_dense_string(dtype)))
    }
    bump_matrix_counter(daf, ra, ca, name)
    MEMORY_DATA
}

# relayout writes the transposed matrix under (columns_axis, rows_axis); on an
# append-only store this is a fresh append when that slot is empty, else the
# setter raises append-only. Mirrors FilesDaf.
.zip_relayout_matrix <- function(daf, ra, ca, name) {
    src <- format_get_matrix(daf, ra, ca, name)$value
    transposed <- if (methods::is(src, "dgCMatrix") || methods::is(src, "lgCMatrix")) {
        Matrix::t(src)
    } else {
        t(src)
    }
    format_set_matrix(daf, ca, ra, name, transposed, overwrite = TRUE)
    invisible()
}

# ==== S7 method registrations ================================================

# --- read (both writer + read-only) ---
local({
    for (cls in list(ZipDaf, ZipDafReadOnly)) {
        S7::method(format_has_scalar, list(cls, S7::class_character)) <-
            function(daf, name) .zip_has_scalar(daf, name)
        S7::method(format_get_scalar, list(cls, S7::class_character)) <-
            function(daf, name) .cache_group_value(.zip_get_scalar(daf, name), MEMORY_DATA)
        S7::method(format_scalars_set, cls) <- function(daf) .zip_scalars_set(daf)

        S7::method(format_has_axis, list(cls, S7::class_character)) <-
            function(daf, axis) .zip_has_axis(daf, axis)
        S7::method(format_axes_set, cls) <- function(daf) .zip_axes_set(daf)
        S7::method(format_axis_length, list(cls, S7::class_character)) <-
            function(daf, axis) length(.zip_axis_require(daf, axis)$entries)
        S7::method(format_axis_array, list(cls, S7::class_character)) <-
            function(daf, axis) .cache_group_value(.zip_axis_require(daf, axis)$entries, MEMORY_DATA)
        S7::method(format_axis_dict, list(cls, S7::class_character)) <-
            function(daf, axis) .zip_axis_require(daf, axis)$dict

        S7::method(format_has_vector, list(cls, S7::class_character, S7::class_character)) <-
            function(daf, axis, name) .zip_has_vector(daf, axis, name)
        S7::method(format_vectors_set, list(cls, S7::class_character)) <-
            function(daf, axis) .zip_vectors_set(daf, axis)
        S7::method(format_get_vector, list(cls, S7::class_character, S7::class_character)) <-
            function(daf, axis, name) {
                v <- .zip_get_vector_impl(daf, axis, name)
                .cache_group_value(.attach_vector_axis_names(daf, axis, v),
                    .files_daf_classify_vector(v))
            }

        S7::method(format_has_matrix,
            list(cls, S7::class_character, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis, name) .zip_has_matrix(daf, rows_axis, columns_axis, name)
        S7::method(format_matrices_set,
            list(cls, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis) .zip_matrices_set(daf, rows_axis, columns_axis)
        S7::method(format_get_matrix,
            list(cls, S7::class_character, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis, name) {
                m <- .zip_get_matrix_impl(daf, rows_axis, columns_axis, name)
                .cache_group_value(
                    .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, m),
                    .files_daf_classify_matrix(m))
            }

        S7::method(.is_leaf_dispatch, cls) <- function(daf) TRUE
        S7::method(format_description_header, cls) <-
            function(daf, indent = "", deep = FALSE) {
                internal <- S7::prop(daf, "internal")
                c(paste0(indent, "type: ZipDaf"),
                  paste0(indent, "path: ", internal$path),
                  paste0(indent, "mode: ", internal$mode))
            }
    }
})

# --- write (ZipDaf only; append-only) ---
S7::method(format_set_scalar,
    list(ZipDaf, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, name, value, overwrite) .zip_set_scalar(daf, name, value, overwrite)
S7::method(format_add_axis,
    list(ZipDaf, S7::class_character, S7::class_character)) <-
    function(daf, axis, entries) .zip_add_axis(daf, axis, entries)
S7::method(format_set_vector,
    list(ZipDaf, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, axis, name, vec, overwrite) .zip_set_vector(daf, axis, name, vec, overwrite)
S7::method(format_set_matrix,
    list(ZipDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, mat, overwrite) {
        .zip_set_matrix(daf, rows_axis, columns_axis, name, mat, overwrite)
    }
S7::method(format_relayout_matrix,
    list(ZipDaf, S7::class_character, S7::class_character, S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) .zip_relayout_matrix(daf, rows_axis, columns_axis, name)

# delete / reorder: append-only
S7::method(format_delete_scalar, list(ZipDaf, S7::class_character, S7::class_logical)) <-
    function(daf, name, must_exist) .zip_append_only_guard(sprintf("delete scalar %s", name))
S7::method(format_delete_axis, list(ZipDaf, S7::class_character, S7::class_logical)) <-
    function(daf, axis, must_exist) .zip_append_only_guard(sprintf("delete axis %s", axis))
S7::method(format_delete_vector,
    list(ZipDaf, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, axis, name, must_exist) .zip_append_only_guard(sprintf("delete vector %s", name))
S7::method(format_delete_matrix,
    list(ZipDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) {
        .zip_append_only_guard(sprintf("delete matrix %s", name))
    }
S7::method(format_replace_reorder, list(ZipDaf, S7::class_list)) <-
    function(daf, plan, crash_counter = NULL) .zip_append_only_guard("reorder axes")

# --- read-only guards (mutating on ZipDafReadOnly) ---
S7::method(format_set_scalar,
    list(ZipDafReadOnly, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, name, value, overwrite) .zip_read_only_guard("set_scalar")
S7::method(format_delete_scalar,
    list(ZipDafReadOnly, S7::class_character, S7::class_logical)) <-
    function(daf, name, must_exist) .zip_read_only_guard("delete_scalar")
S7::method(format_add_axis,
    list(ZipDafReadOnly, S7::class_character, S7::class_character)) <-
    function(daf, axis, entries) .zip_read_only_guard("add_axis")
S7::method(format_delete_axis,
    list(ZipDafReadOnly, S7::class_character, S7::class_logical)) <-
    function(daf, axis, must_exist) .zip_read_only_guard("delete_axis")
S7::method(format_set_vector,
    list(ZipDafReadOnly, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, axis, name, vec, overwrite) .zip_read_only_guard("set_vector")
S7::method(format_delete_vector,
    list(ZipDafReadOnly, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, axis, name, must_exist) .zip_read_only_guard("delete_vector")
S7::method(format_set_matrix,
    list(ZipDafReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, mat, overwrite) .zip_read_only_guard("set_matrix")
S7::method(format_delete_matrix,
    list(ZipDafReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) .zip_read_only_guard("delete_matrix")
S7::method(format_relayout_matrix,
    list(ZipDafReadOnly, S7::class_character, S7::class_character, S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) .zip_read_only_guard("relayout_matrix")

# ==== constructor ============================================================

.DAF_MARKER_KEY <- "daf.json"

.zip_check_version <- function(store, path) {
    raw <- rawToChar(store_get_bytes(store, .DAF_MARKER_KEY))
    m <- regmatches(raw, regexec(.DAF_JSON_RE, raw, perl = TRUE))[[1L]]
    if (length(m) != 3L) {
        j <- jsonlite::fromJSON(raw)
        v <- j$version
        if (is.null(v) || length(v) != 2L) {
            stop(sprintf("zip_daf: %s daf.json is malformed", sQuote(path)), call. = FALSE)
        }
        v1 <- v[[1L]]
        v2 <- v[[2L]]
    } else {
        v1 <- as.integer(m[[2L]])
        v2 <- as.integer(m[[3L]])
    }
    if (v1 != 1L || v2 > 1L) {
        stop(sprintf(
            "incompatible format version: %d.%d\nfor the zip daf: %s\nthe code supports version: 1.1",
            v1, v2, path), call. = FALSE)
    }
    invisible()
}

#' Single-file (zip) Daf store.
#'
#' A `Daf` store held in one append-only `.daf.zip` archive, byte-compatible
#' with Julia's `DataAxesFormats.ZipDaf`. Same on-disk layout as [files_daf()]
#' but inside a ZIP archive (whose central directory replaces `metadata.json`).
#' The archive is append-only: overwriting or deleting a property, or reordering
#' an axis, raises an error.
#'
#' @param path Path to a `.daf.zip` archive.
#' @param mode One of `"r"` (read; must exist), `"r+"` (append; must exist),
#'   `"w"` (create; fails if it is already a daf archive), `"w+"` (create or
#'   append).
#' @param name Human-readable identifier. Default derived from the archive's
#'   `name` scalar if present, else `basename(path)`.
#' @param packed When `TRUE` (writeable modes), large numeric components are
#'   written as packed `.zip` shards, as in [files_daf()].
#' @return A `ZipDaf` (writable modes) or `ZipDafReadOnly` (`"r"`).
#' @examples
#' # ZipDaf is built on the POSIX-only MmapZipStore, so it is unavailable on
#' # Windows; guard the example accordingly.
#' if (.Platform$OS.type != "windows") {
#'   path <- tempfile("dafr-zip-", fileext = ".daf.zip")
#'   d <- zip_daf(path, mode = "w")
#'   add_axis(d, "cell", c("c1", "c2"))
#'   set_scalar(d, "organism", "human")
#'   rm(d)
#'   unlink(path)
#' }
#' @export
zip_daf <- function(path, mode = c("r", "r+", "w", "w+"), name = NULL,
                    packed = FALSE) {
    stopifnot(is.character(path), length(path) == 1L, !is.na(path))
    mode <- match.arg(mode)
    # `w` truncates the archive on open, so we cannot detect a pre-existing daf
    # store from the opened (emptied) store. Peek read-only first (matching
    # FilesDaf's pre-open daf.json check on the directory).
    if (mode == "w" && file.exists(path)) {
        already <- tryCatch({
            s0 <- new_mmap_zip_store(path, mode = "r")
            res <- store_exists(s0, .DAF_MARKER_KEY)
            dafr_mmap_zip_close(S7::prop(s0, "xptr"))
            res
        }, error = function(e) FALSE)
        if (isTRUE(already)) {
            stop(sprintf("zip_daf(%s, 'w'): archive is already a daf store; use 'w+'",
                sQuote(path)), call. = FALSE)
        }
    }
    store <- new_mmap_zip_store(path, mode = mode)
    has_marker <- store_exists(store, .DAF_MARKER_KEY)
    if (mode %in% c("r", "r+") && !has_marker) {
        stop(sprintf("zip_daf(%s, '%s'): not a daf archive (no daf.json)",
            sQuote(path), mode), call. = FALSE)
    }
    if (mode %in% c("w", "w+") && !has_marker) {
        store_set_bytes(store, .DAF_MARKER_KEY, charToRaw('{"version":[1,1]}\n'))
    }
    .zip_check_version(store, path)
    if (is.null(name)) {
        nm_key <- .zkey_scalar("name")
        name <- if (store_exists(store, nm_key)) {
            .decode_scalar_json(store_get_bytes(store, nm_key))
        } else {
            basename(path)
        }
    }
    .assert_name(name, "name")
    internal <- new_internal_env()
    internal$store <- store
    internal$path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    internal$mode <- mode
    internal$packed <- isTRUE(packed)
    internal$axes <- new.env(parent = emptyenv())
    ctor <- if (mode == "r") ZipDafReadOnly else ZipDaf
    ctor(
        name = name, internal = internal, cache = new_cache_env(),
        axis_version_counter = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
}
