#' @include files_daf.R files_io.R utils.R format_api.R
NULL

S7::method(
    format_set_scalar,
    list(FilesDaf, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    .assert_scalar_value(name, value)
    p <- .path_scalar(.files_root(daf), name)
    if (file.exists(p) && !overwrite) {
        stop(sprintf(
            "scalar %s already exists; use overwrite = TRUE",
            sQuote(name)
        ), call. = FALSE)
    }
    .write_scalar_json(p, value)
    MEMORY_DATA
}

S7::method(
    format_delete_scalar,
    list(FilesDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    p <- .path_scalar(.files_root(daf), name)
    if (!file.exists(p)) {
        if (must_exist) {
            stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
        }
        return(invisible())
    }
    unlink(p, force = TRUE)
    invisible()
}

.write_axis_file <- function(path, entries) {
    con <- file(path, open = "wb", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(entries, con, useBytes = FALSE)
}

S7::method(
    format_add_axis,
    list(FilesDaf, S7::class_character, S7::class_character)
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
    if (any(grepl("[\n\r]", entries))) {
        stop(sprintf("axis %s entries contain newline characters", sQuote(axis)), call. = FALSE)
    }
    if (anyDuplicated(entries)) {
        dup <- entries[duplicated(entries)][1L]
        stop(sprintf(
            "axis %s has duplicate entry %s",
            sQuote(axis), sQuote(dup)
        ), call. = FALSE)
    }
    if (length(entries) > .Machine$integer.max) {
        stop(sprintf("axis %s length exceeds R integer capacity", sQuote(axis)),
            call. = FALSE
        )
    }
    root <- .files_root(daf)
    p <- .path_axis(root, axis)
    if (file.exists(p)) {
        stop(sprintf("axis %s already exists", sQuote(axis)), call. = FALSE)
    }
    .write_axis_file(p, entries)
    # Match Julia FilesDaf layout: eagerly create vectors/<axis> and the
    # matrices/<axis>/<other> + matrices/<other>/<axis> bucket dirs so that
    # a Julia reader scanning the store does not trip on missing directories.
    dir.create(.path_vector_dir(root, axis),
        recursive = TRUE,
        showWarnings = FALSE
    )
    mroot <- file.path(root, "matrices")
    dir.create(file.path(mroot, axis), recursive = TRUE, showWarnings = FALSE)
    existing_axes <- list.files(mroot, full.names = FALSE)
    for (other in existing_axes) {
        if (other == axis) next
        dir.create(file.path(mroot, axis, other), showWarnings = FALSE)
        dir.create(file.path(mroot, other, axis), showWarnings = FALSE)
    }
    dir.create(file.path(mroot, axis, axis), showWarnings = FALSE)
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    assign(axis, list(entries = entries, dict = dict),
        envir = S7::prop(daf, "internal")$axes
    )
    invisible()
}

S7::method(
    format_delete_axis,
    list(FilesDaf, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    root <- .files_root(daf)
    p <- .path_axis(root, axis)
    if (!file.exists(p)) {
        if (must_exist) {
            stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
        }
        return(invisible())
    }
    unlink(p, force = TRUE)
    # Cascade: drop vectors/<axis> and any matrices involving <axis>.
    vdir <- .path_vector_dir(root, axis)
    if (dir.exists(vdir)) unlink(vdir, recursive = TRUE, force = TRUE)
    mroot <- file.path(root, "matrices")
    mrow <- file.path(mroot, axis)
    if (dir.exists(mrow)) unlink(mrow, recursive = TRUE, force = TRUE)
    # Remove <axis> used as columns_axis under other rows_axis buckets.
    for (rows in list.files(mroot, full.names = FALSE)) {
        mcol <- file.path(mroot, rows, axis)
        if (dir.exists(mcol)) unlink(mcol, recursive = TRUE, force = TRUE)
    }
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) {
        rm(list = axis, envir = cache)
    }
    bump_axis_counter(daf, axis)
    invisible()
}

.files_write_vector_dense <- function(vdir, name, vec) {
    dtype <- .dtype_for_r_vector(vec)
    if (dtype == "String") {
        con <- file(file.path(vdir, paste0(name, ".txt")),
            open = "wb",
            encoding = "UTF-8"
        )
        writeLines(vec, con, useBytes = FALSE)
        close(con)
    } else {
        .write_bin_dense(file.path(vdir, paste0(name, ".data")), vec, dtype)
    }
    .write_descriptor_dense(file.path(vdir, paste0(name, ".json")), dtype)
    invisible()
}

.files_vector_unlink_payload <- function(vdir, name) {
    for (ext in c(".data", ".txt", ".nzind", ".nzval", ".nztxt")) {
        p <- file.path(vdir, paste0(name, ext))
        if (file.exists(p)) unlink(p, force = TRUE)
    }
}

.files_write_vector_sparse_numeric <- function(vdir, name, nzind, nzval,
                                               eltype, indtype) {
    .write_bin_dense(
        file.path(vdir, paste0(name, ".nzind")),
        as.integer(nzind), indtype
    )
    if (eltype == "Bool") {
        if (!all(nzval)) {
            .write_bin_dense(
                file.path(vdir, paste0(name, ".nzval")),
                as.logical(nzval), "Bool"
            )
        }
    } else {
        .write_bin_dense(file.path(vdir, paste0(name, ".nzval")), nzval, eltype)
    }
    .write_descriptor_sparse(file.path(vdir, paste0(name, ".json")),
        dtype = eltype, indtype = indtype
    )
    invisible()
}

.files_write_vector_sparse_string <- function(vdir, name, nzind, nzval, indtype) {
    .write_bin_dense(
        file.path(vdir, paste0(name, ".nzind")),
        as.integer(nzind), indtype
    )
    con <- file(file.path(vdir, paste0(name, ".nztxt")),
        open = "wb",
        encoding = "UTF-8"
    )
    writeLines(nzval, con, useBytes = FALSE)
    close(con)
    .write_descriptor_sparse(file.path(vdir, paste0(name, ".json")),
        dtype = "String", indtype = indtype
    )
    invisible()
}

# Adaptive dispatcher: picks dense vs sparse on-disk layout based on the
# heuristics in files_io.R. sparseVector input is always written sparse.
S7::method(
    format_set_vector,
    list(
        FilesDaf, S7::class_character, S7::class_character,
        S7::class_any, S7::class_logical
    )
) <- function(daf, axis, name, vec, overwrite) {
    if (methods::is(vec, "sparseVector")) {
        return(.files_set_vector_sparse_input(daf, axis, name, vec, overwrite))
    }
    vec <- .validate_vector_value(daf, axis, name, vec)
    root <- .files_root(daf)
    vdir <- .path_vector_dir(root, axis)
    dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
    desc_path <- file.path(vdir, paste0(name, ".json"))
    if (file.exists(desc_path) && !overwrite) {
        stop(sprintf(
            "vector %s already exists on axis %s; use overwrite = TRUE",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    .files_vector_unlink_payload(vdir, name)
    eltype <- .dtype_for_r_vector(vec)
    n <- length(vec)
    indtype <- .indtype_for_size(n)
    go_sparse <- if (eltype == "String") {
        .should_sparsify_string(vec, indtype)
    } else {
        .should_sparsify_numeric(vec, eltype, indtype)
    }
    if (!go_sparse) {
        .files_write_vector_dense(vdir, name, vec)
    } else if (eltype == "String") {
        nz <- which(nzchar(vec))
        .files_write_vector_sparse_string(vdir, name, nz, vec[nz], indtype)
    } else {
        nz <- if (is.logical(vec)) which(vec) else which(vec != 0)
        .files_write_vector_sparse_numeric(vdir, name, nz, vec[nz], eltype, indtype)
    }
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

.files_set_vector_sparse_input <- function(daf, axis, name, sv, overwrite) {
    n <- format_axis_length(daf, axis)
    if (sv@length != n) {
        stop(sprintf(
            "sparseVector %s length %d (expected %d) on axis %s",
            sQuote(name), sv@length, n, sQuote(axis)
        ), call. = FALSE)
    }
    root <- .files_root(daf)
    vdir <- .path_vector_dir(root, axis)
    dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
    desc_path <- file.path(vdir, paste0(name, ".json"))
    if (file.exists(desc_path) && !overwrite) {
        stop(sprintf(
            "vector %s already exists on axis %s; use overwrite = TRUE",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    .files_vector_unlink_payload(vdir, name)
    eltype <- .dtype_for_r_vector(sv@x)
    indtype <- .indtype_for_size(n)
    .files_write_vector_sparse_numeric(
        vdir, name,
        as.integer(sv@i), sv@x, eltype, indtype
    )
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

S7::method(
    format_delete_vector,
    list(FilesDaf, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, axis, name, must_exist) {
    vdir <- .path_vector_dir(.files_root(daf), axis)
    desc_path <- file.path(vdir, paste0(name, ".json"))
    if (!file.exists(desc_path)) {
        if (must_exist) {
            stop(sprintf(
                "vector %s does not exist on axis %s",
                sQuote(name), sQuote(axis)
            ), call. = FALSE)
        }
        return(invisible())
    }
    unlink(desc_path, force = TRUE)
    .files_vector_unlink_payload(vdir, name)
    invisible()
}

.files_matrix_unlink_payload <- function(mdir, name) {
    for (ext in c(".data", ".txt", ".colptr", ".rowval", ".nzval", ".nztxt")) {
        p <- file.path(mdir, paste0(name, ext))
        if (file.exists(p)) unlink(p, force = TRUE)
    }
}

.files_write_matrix_sparse <- function(mdir, name, mat) {
    is_bool <- methods::is(mat, "lgCMatrix")
    dtype <- if (is_bool) "Bool" else "Float64"
    nr <- nrow(mat)
    nc <- ncol(mat)
    nnz <- length(mat@x)
    indtype <- .indtype_for_size(max(nr, nc, nnz))
    .write_bin_dense(
        file.path(mdir, paste0(name, ".colptr")),
        as.integer(mat@p) + 1L, indtype
    )
    .write_bin_dense(
        file.path(mdir, paste0(name, ".rowval")),
        as.integer(mat@i) + 1L, indtype
    )
    if (is_bool) {
        if (!all(mat@x)) {
            .write_bin_dense(
                file.path(mdir, paste0(name, ".nzval")),
                as.logical(mat@x), "Bool"
            )
        }
    } else {
        .write_bin_dense(
            file.path(mdir, paste0(name, ".nzval")),
            as.double(mat@x), "Float64"
        )
    }
    .write_descriptor_sparse(file.path(mdir, paste0(name, ".json")),
        dtype = dtype, indtype = indtype
    )
    invisible()
}

S7::method(
    format_set_matrix,
    list(
        FilesDaf, S7::class_character, S7::class_character,
        S7::class_character, S7::class_any, S7::class_logical
    )
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    mat <- .validate_matrix_value(daf, rows_axis, columns_axis, name, mat)
    root <- .files_root(daf)
    mdir <- .path_matrix_dir(root, rows_axis, columns_axis)
    dir.create(mdir, recursive = TRUE, showWarnings = FALSE)
    desc_path <- file.path(mdir, paste0(name, ".json"))
    if (file.exists(desc_path) && !overwrite) {
        stop(
            sprintf(
                "matrix %s already exists on axes (%s, %s); use overwrite = TRUE",
                sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
            ),
            call. = FALSE
        )
    }
    .files_matrix_unlink_payload(mdir, name)
    if (methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")) {
        .files_write_matrix_sparse(mdir, name, mat)
        bump_matrix_counter(daf, rows_axis, columns_axis, name)
        return(MEMORY_DATA)
    }
    dtype <- .dtype_for_r_vector(as.vector(mat))
    if (dtype == "String") {
        con <- file(file.path(mdir, paste0(name, ".txt")),
            open = "wb",
            encoding = "UTF-8"
        )
        writeLines(as.vector(mat), con, useBytes = FALSE)
        close(con)
    } else {
        .write_bin_dense(
            file.path(mdir, paste0(name, ".data")),
            as.vector(mat), dtype
        )
    }
    .write_descriptor_dense(desc_path, dtype)
    bump_matrix_counter(daf, rows_axis, columns_axis, name)
    MEMORY_DATA
}

S7::method(
    format_delete_matrix,
    list(
        FilesDaf, S7::class_character, S7::class_character,
        S7::class_character, S7::class_logical
    )
) <- function(daf, rows_axis, columns_axis, name, must_exist) {
    mdir <- .path_matrix_dir(.files_root(daf), rows_axis, columns_axis)
    desc_path <- file.path(mdir, paste0(name, ".json"))
    if (!file.exists(desc_path)) {
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
    unlink(desc_path, force = TRUE)
    .files_matrix_unlink_payload(mdir, name)
    invisible()
}

S7::method(
    format_relayout_matrix,
    list(FilesDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    src <- format_get_matrix(daf, rows_axis, columns_axis, name)$value
    transposed <- if (methods::is(src, "dgCMatrix") || methods::is(src, "lgCMatrix")) {
        Matrix::t(src)
    } else {
        t(src)
    }
    format_set_matrix(daf, columns_axis, rows_axis, name, transposed,
        overwrite = TRUE
    )
    invisible()
}

# ---- Reorder ---------------------------------------------------------------
#
# Mirrors upstream `DataAxesFormats.jl::FilesFormat`
# (`src/files_format.jl::format_replace_reorder!` and friends).
#
# Protocol — backup-via-hardlinks:
#   1. lock     create `.reorder.backup/` directory; its existence is the lock.
#   2. backup   for every property that will be rewritten, hardlink the
#               existing payload files into `.reorder.backup/<relpath>`.
#               Hardlinks share inodes, so the backup keeps a stable copy of
#               the pre-reorder bytes even after the original is overwritten.
#   3. replace  delete + rewrite the live payload files with the permuted
#               contents. Tick the crash counter once per vector and once per
#               matrix (matching upstream's two tick-call sites).
#   4. cleanup  rm -rf `.reorder.backup/` and bump version counters.
#   5. reset    on a subsequent open, if `.reorder.backup/` is present, copy
#               every backed-up file back over its live counterpart, then
#               remove the backup. A backed-up file always represents the
#               pre-reorder bytes, so a blind copy-over restores the state
#               regardless of how far the replace phase had progressed.
#
# Why backup + overwrite instead of stage + rename?
#   - Upstream uses this exact protocol; mirroring it keeps tick-point parity
#     with the Julia FLAKY tests (Phase 7).
#   - Hardlinks are O(1) and don't duplicate bytes on disk.
#   - Recovery is uniform: copy backup → live, ignoring the partial-replace
#     state. No high-water mark needed in any STATE marker — the directory's
#     existence IS the marker.

.REORDER_BACKUP_DIR <- ".reorder.backup"

# Suffixes upstream backs up + restores per vector/matrix (`.txt` is for
# string-payload dense vectors; `.nztxt` for sparse strings).
.REORDER_VECTOR_SUFFIXES <- c(".json", ".data", ".txt", ".nzind", ".nzval", ".nztxt")
.REORDER_MATRIX_SUFFIXES <- c(".json", ".data", ".txt", ".colptr", ".rowval", ".nzval", ".nztxt")

.reorder_backup_root <- function(daf) {
    file.path(.files_root(daf), .REORDER_BACKUP_DIR)
}

# Hardlink src to dst if src exists. Falls back to file.copy on systems
# where file.link returns FALSE (e.g. cross-filesystem). Within a single
# files_daf store this is always intra-filesystem so file.link succeeds.
.reorder_backup_hardlink <- function(src, dst) {
    if (!file.exists(src)) return(invisible())
    ok <- file.link(src, dst)
    if (!isTRUE(ok)) {
        if (!isTRUE(file.copy(src, dst, overwrite = FALSE))) {
            stop(sprintf("files_daf: failed to back up %s -> %s",
                         sQuote(src), sQuote(dst)), call. = FALSE)
        }
    }
    invisible()
}

# ---- lock + backup ----------------------------------------------------------
#
# Combined into format_replace_reorder's prologue (matching the orchestrator
# pattern in upstream's reorder_axes_locked!: lock → backup → replace).
# The replace method is the public S7 entry point; lock + backup happen
# inline as its first action, so a crash mid-backup leaves a recoverable
# partial backup directory (reset will simply restore whatever it finds).

.reorder_lock <- function(daf) {
    backup_root <- .reorder_backup_root(daf)
    if (dir.exists(backup_root)) {
        stop(sprintf(
            "files_daf: reorder backup directory already exists at %s; ",
            sQuote(backup_root),
            "call reset_reorder_axes() first"
        ), call. = FALSE)
    }
    dir.create(backup_root, recursive = FALSE, showWarnings = FALSE)
    invisible()
}

.reorder_backup <- function(daf, plan) {
    root <- .files_root(daf)
    backup_root <- .reorder_backup_root(daf)
    dir.create(file.path(backup_root, "axes"),
               recursive = TRUE, showWarnings = FALSE)
    for (axis in names(plan$planned_axes)) {
        src <- .path_axis(root, axis)
        if (file.exists(src)) {
            .reorder_backup_hardlink(
                src,
                file.path(backup_root, "axes", paste0(axis, ".txt"))
            )
        }
    }
    for (pv in plan$planned_vectors) {
        bdir <- file.path(backup_root, "vectors", pv$axis)
        dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
        for (suffix in .REORDER_VECTOR_SUFFIXES) {
            src <- file.path(.path_vector_dir(root, pv$axis),
                             paste0(pv$name, suffix))
            if (file.exists(src)) {
                .reorder_backup_hardlink(
                    src, file.path(bdir, paste0(pv$name, suffix))
                )
            }
        }
    }
    for (pm in plan$planned_matrices) {
        bdir <- file.path(backup_root, "matrices",
                          pm$rows_axis, pm$columns_axis)
        dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
        for (suffix in .REORDER_MATRIX_SUFFIXES) {
            src <- file.path(.path_matrix_dir(root, pm$rows_axis,
                                              pm$columns_axis),
                             paste0(pm$name, suffix))
            if (file.exists(src)) {
                .reorder_backup_hardlink(
                    src, file.path(bdir, paste0(pm$name, suffix))
                )
            }
        }
    }
    invisible()
}

# ---- replace helpers --------------------------------------------------------

.reorder_unlink_vector_payload <- function(vdir, name) {
    for (suffix in .REORDER_VECTOR_SUFFIXES) {
        p <- file.path(vdir, paste0(name, suffix))
        if (file.exists(p)) unlink(p, force = TRUE)
    }
}

.reorder_unlink_matrix_payload <- function(mdir, name) {
    for (suffix in .REORDER_MATRIX_SUFFIXES) {
        p <- file.path(mdir, paste0(name, suffix))
        if (file.exists(p)) unlink(p, force = TRUE)
    }
}

.reorder_replace_axis <- function(daf, axis, planned_axis) {
    p <- .path_axis(.files_root(daf), axis)
    if (file.exists(p)) {
        unlink(p, force = TRUE)
        .write_axis_file(p, planned_axis$new_entries)
    }
    # Refresh the in-memory axis cache so subsequent format_get_vector /
    # format_get_matrix calls see the new length / entries during replacement.
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) {
        rm(list = axis, envir = cache)
    }
    invisible()
}

.reorder_replace_vector <- function(daf, pv, plan) {
    root <- .files_root(daf)
    # Read the source from backup (the live payload may already be partially
    # gone if upstream retry semantics reapply). Backup hardlinks are stable.
    # We read from the *live* file because backup is by hardlink so they share
    # the same inode and content.
    src_vec <- format_get_vector(daf, pv$axis, pv$name)$value
    pa <- plan$planned_axes[[pv$axis]]
    vdir <- .path_vector_dir(root, pv$axis)
    .reorder_unlink_vector_payload(vdir, pv$name)
    if (methods::is(src_vec, "sparseVector")) {
        # Permute sparse: nzi'_k = inverse_permutation[nzi_k].
        new_nzi <- pa$inverse[as.integer(src_vec@i)]
        ord <- order(new_nzi)
        new_nzi <- new_nzi[ord]
        new_nzv <- src_vec@x[ord]
        eltype <- .dtype_for_r_vector(new_nzv)
        n <- length(src_vec)
        indtype <- .indtype_for_size(n)
        .files_write_vector_sparse_numeric(vdir, pv$name,
                                           new_nzi, new_nzv,
                                           eltype, indtype)
    } else {
        permuted <- src_vec[pa$permutation]
        .files_write_vector_dense(vdir, pv$name, permuted)
    }
    invisible()
}

.reorder_replace_matrix <- function(daf, pm, plan) {
    root <- .files_root(daf)
    src_mat <- format_get_matrix(daf, pm$rows_axis, pm$columns_axis,
                                 pm$name)$value
    pr <- plan$planned_axes[[pm$rows_axis]]
    pc <- plan$planned_axes[[pm$columns_axis]]
    r_perm <- if (!is.null(pr)) pr$permutation else seq_len(nrow(src_mat))
    c_perm <- if (!is.null(pc)) pc$permutation else seq_len(ncol(src_mat))
    permuted <- src_mat[r_perm, c_perm, drop = FALSE]
    mdir <- .path_matrix_dir(root, pm$rows_axis, pm$columns_axis)
    .reorder_unlink_matrix_payload(mdir, pm$name)
    if (methods::is(permuted, "dgCMatrix") ||
        methods::is(permuted, "lgCMatrix")) {
        .files_write_matrix_sparse(mdir, pm$name, permuted)
    } else {
        # Dense matrix: write column-major .data + descriptor.
        dtype <- .dtype_for_r_vector(as.vector(permuted))
        if (dtype == "String") {
            con <- file(file.path(mdir, paste0(pm$name, ".txt")),
                        open = "wb", encoding = "UTF-8")
            writeLines(as.vector(permuted), con, useBytes = FALSE)
            close(con)
        } else {
            .write_bin_dense(file.path(mdir, paste0(pm$name, ".data")),
                             as.vector(permuted), dtype)
        }
        .write_descriptor_dense(
            file.path(mdir, paste0(pm$name, ".json")), dtype
        )
    }
    invisible()
}

# ---- S7 methods -------------------------------------------------------------

S7::method(format_replace_reorder, list(FilesDaf, S7::class_list)) <-
    function(daf, plan, crash_counter = NULL) {
        .reorder_lock(daf)
        .reorder_backup(daf, plan)

        # Axis files first. Matches upstream ordering (axes pass before
        # vectors/matrices). No tick on the axis pass — upstream doesn't
        # tick there either.
        for (axis in names(plan$planned_axes)) {
            .reorder_replace_axis(daf, axis, plan$planned_axes[[axis]])
        }

        # Vectors. One tick per vector — matches upstream
        # files_format.jl line 1385.
        for (pv in plan$planned_vectors) {
            .reorder_replace_vector(daf, pv, plan)
            tick_crash_counter(crash_counter)
        }

        # Matrices. One tick per matrix — matches upstream
        # files_format.jl line 1390.
        for (pm in plan$planned_matrices) {
            .reorder_replace_matrix(daf, pm, plan)
            tick_crash_counter(crash_counter)
        }

        invisible()
    }

S7::method(format_cleanup_reorder, list(FilesDaf, S7::class_list)) <-
    function(daf, plan, crash_counter = NULL) {
        backup_root <- .reorder_backup_root(daf)
        if (dir.exists(backup_root)) {
            unlink(backup_root, recursive = TRUE, force = TRUE)
        }
        # Bump version counters once cleanup succeeds — the new payloads
        # are now committed. Upstream invalidates caches via
        # invalidate_reorder_caches!; in dafr the counter bump invalidates
        # any per-key cache entry. Bumping in cleanup (not in replace)
        # mirrors the "all-or-nothing" commit semantics: a crash mid-replace
        # never leaves stamps half-bumped because a successful reset
        # restores the original bytes.
        for (axis in names(plan$planned_axes)) {
            bump_axis_counter(daf, axis)
        }
        for (pv in plan$planned_vectors) {
            bump_vector_counter(daf, pv$axis, pv$name)
        }
        for (pm in plan$planned_matrices) {
            bump_matrix_counter(daf, pm$rows_axis, pm$columns_axis, pm$name)
        }
        invisible()
    }

S7::method(format_reset_reorder, FilesDaf) <-
    function(daf, crash_counter = NULL) {
        backup_root <- .reorder_backup_root(daf)
        if (!dir.exists(backup_root)) {
            return(invisible(FALSE))
        }
        root <- .files_root(daf)
        # Walk the backup tree; for every file in there, copy back over
        # the live equivalent. Use list.files(recursive = TRUE) for an
        # exhaustive enumeration that mirrors Julia's `walkdir`.
        rels <- list.files(backup_root, recursive = TRUE,
                           all.files = FALSE, no.. = TRUE,
                           include.dirs = FALSE)
        for (rel in rels) {
            backup_path <- file.path(backup_root, rel)
            live_path   <- file.path(root, rel)
            live_dir    <- dirname(live_path)
            if (!dir.exists(live_dir)) {
                dir.create(live_dir, recursive = TRUE, showWarnings = FALSE)
            }
            if (file.exists(live_path)) unlink(live_path, force = TRUE)
            ok <- file.link(backup_path, live_path)
            if (!isTRUE(ok)) {
                if (!isTRUE(file.copy(backup_path, live_path, overwrite = FALSE))) {
                    stop(sprintf("files_daf: reset failed to restore %s",
                                 sQuote(rel)), call. = FALSE)
                }
            }
        }
        unlink(backup_root, recursive = TRUE, force = TRUE)
        # Drop in-memory axis cache so reads after reset re-read from disk.
        axes_cache <- S7::prop(daf, "internal")$axes
        for (k in ls(envir = axes_cache, all.names = TRUE)) {
            rm(list = k, envir = axes_cache)
        }
        invisible(TRUE)
    }

# Recovery hook used by files_daf() when a writeable mode opens a store
# that has a leftover .reorder.backup/ from a previously-crashed reorder.
# Defensive: if anything inside the backup directory looks corrupt, the
# safe action is to log a warning and continue with whatever we managed
# to restore — never crash the open.
.files_daf_recover_reorder <- function(daf) {
    backup_root <- .reorder_backup_root(daf)
    if (!dir.exists(backup_root)) return(invisible(FALSE))
    res <- tryCatch(
        format_reset_reorder(daf),
        error = function(e) {
            warning(sprintf(
                "files_daf: reorder backup at %s could not be restored (%s); ",
                sQuote(backup_root), conditionMessage(e),
                "removing leftover backup"
            ), call. = FALSE)
            unlink(backup_root, recursive = TRUE, force = TRUE)
            FALSE
        }
    )
    invisible(res)
}
