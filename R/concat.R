#' @include classes.R readers.R writers.R memory_daf.R copies.R
NULL

#' Merge action constants for [concatenate()].
#'
#' Used as values in `concatenate()`'s `merge` argument:
#' `MERGE_SKIP` skips the property (default), `MERGE_LAST_VALUE` uses the
#' last source containing it, `MERGE_COLLECT_AXIS` collects values along
#' the `dataset_axis`.
#'
#' @name merge_actions
#' @export
MERGE_SKIP <- "SkipProperty"
#' @rdname merge_actions
#' @export
MERGE_LAST_VALUE <- "LastValue"
#' @rdname merge_actions
#' @export
MERGE_COLLECT_AXIS <- "CollectAxis"

#' Concatenate multiple dafs along one or more axes.
#'
#' Mirrors Julia `concatenate!()`. For each concatenation axis, entries from
#' each source are appended in source order. Non-concat axes must be
#' identical across all sources and are copied once.
#'
#' @param destination A `DafWriter`. Must be empty of the concatenation axes.
#' @param axis A single axis name or a character vector of axis names.
#' @param sources List of `DafReader`s to concatenate.
#' @param names Optional character vector of unique data set names (defaults
#'   to each source's `name` prop).
#' @param dataset_axis Name of the per-source axis to create. `NULL` disables.
#' @param dataset_property If `TRUE` (default) and `dataset_axis` is non-NULL,
#'   create a same-named vector on every concatenation axis holding the
#'   source name for each entry.
#' @param prefix Logical (single or per-axis). Prefix concat-axis entries
#'   with `"<dataset_name>."` to de-duplicate across sources.
#' @param prefixed Optional character vector (or list of vectors per axis)
#'   of additional property names to prefix, beyond the heuristic (same-name
#'   or `"<axis>.*"` properties).
#' @param empty Named list of fill values for missing per-source properties.
#' @param merge Named list mapping property keys to a merge action
#'   (`"SkipProperty"`, `"LastValue"`, `"CollectAxis"`).
#' @param sparse_if_saves_storage_fraction Numeric (default 0.25). A
#'   `MERGE_COLLECT_AXIS` vector merge produces a sparse (`dgCMatrix`)
#'   result when sparse storage would save at least this fraction of the
#'   dense storage size (mirrors Julia's heuristic; sparse-stored sources
#'   count their nnz, dense-stored sources count their full length).
#' @param overwrite If `TRUE`, allow replacing pre-existing destination
#'   entries.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1", "a2"))
#' b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
#' dest <- memory_daf(name = "dest")
#' concatenate(dest, "cell", list(a, b))
concatenate <- function(destination, axis, sources,
                        names = NULL,
                        dataset_axis = "dataset",
                        dataset_property = TRUE,
                        prefix = FALSE,
                        prefixed = NULL,
                        empty = NULL,
                        merge = NULL,
                        sparse_if_saves_storage_fraction = 0.25,
                        overwrite = FALSE) {
    axes <- if (is.character(axis) && length(axis) > 1L) axis else as.character(axis)
    if (length(axes) < 1L) stop("`axis` must be a non-empty character vector",
                                call. = FALSE)
    if (anyDuplicated(axes)) stop("`axis` names must be unique", call. = FALSE)
    for (ax in axes) .assert_name(ax, "axis")
    .assert_flag(overwrite, "overwrite")

    if (length(sources) < 1L) stop("`sources` must be non-empty",
                                   call. = FALSE)
    dataset_names <- .concat_dataset_names(sources, names)

    # Validate: no matrix has both axes in the concat set.
    for (src in sources) {
        for (ra in axes) for (ca in axes) {
            if (format_has_axis(src, ra) && format_has_axis(src, ca) &&
                length(format_matrices_set(src, ra, ca)) > 0L) {
                stop(sprintf(
                    "can't concatenate a matrix with both axes in the concat set: %s,%s in %s",
                    sQuote(ra), sQuote(ca), S7::prop(src, "name")
                ), call. = FALSE)
            }
        }
    }

    # Per-axis prefix flag.
    prefixes <- if (is.logical(prefix) && length(prefix) == 1L) {
        rep(prefix, length(axes))
    } else if (is.logical(prefix) && length(prefix) == length(axes)) {
        prefix
    } else {
        stop("`prefix` must be a logical scalar or a logical vector of length(axis)",
             call. = FALSE)
    }
    base::names(prefixes) <- axes

    # Create non-concat axes from the first source; verify all sources agree.
    for (src in sources) {
        for (ax in format_axes_set(src)) {
            if (ax %in% axes) next
            if (!format_has_axis(destination, ax)) {
                format_add_axis(destination, ax, format_axis_array(src, ax)$value)
            } else if (!identical(format_axis_array(destination, ax)$value,
                                  format_axis_array(src, ax)$value)) {
                stop(sprintf(
                    "different entries for the axis: %s between dafs", sQuote(ax)
                ), call. = FALSE)
            }
        }
    }

    # Create the concatenation axes.
    for (ax in axes) {
        .concat_one_axis(destination, ax, sources, dataset_names,
                         prefixes[[ax]], prefixes, prefixed, empty, overwrite,
                         sparse_if_saves_storage_fraction)
    }

    # Optional dataset_axis + property.
    if (!is.null(dataset_axis)) {
        format_add_axis(destination, dataset_axis, dataset_names)
        if (isTRUE(dataset_property)) {
            for (ax in axes) {
                offsets <- cumsum(c(0L, vapply(sources,
                    function(s) format_axis_length(s, ax), integer(1L))))
                labels <- character(offsets[[length(offsets)]])
                for (i in seq_along(sources)) {
                    labels[(offsets[[i]] + 1L):offsets[[i + 1L]]] <- dataset_names[[i]]
                }
                format_set_vector(destination, ax, dataset_axis, labels,
                                  overwrite = overwrite)
            }
        }
    }

    # Merge pass for properties not on any concat axis.
    if (!is.null(merge)) {
        .concat_merge(destination, sources, dataset_names, dataset_axis,
                      axes, merge, empty, overwrite,
                      sparse_if_saves_storage_fraction)
    }

    invisible(destination)
}

.concat_dataset_names <- function(sources, names) {
    if (is.null(names)) {
        names <- vapply(sources, function(s) S7::prop(s, "name"),
                        character(1L))
    } else {
        if (length(names) != length(sources)) {
            stop("`names` must have one entry per source", call. = FALSE)
        }
    }
    if (anyDuplicated(names)) {
        stop("dataset `names` must be unique", call. = FALSE)
    }
    names
}

.concat_one_axis <- function(destination, axis, sources, dataset_names,
                             do_prefix, all_prefixes, prefixed, empty,
                             overwrite, sparse_threshold) {
    per_src <- lapply(seq_along(sources), function(i) {
        e <- format_axis_array(sources[[i]], axis)$value
        if (isTRUE(do_prefix)) paste(dataset_names[[i]], e, sep = ".") else e
    })
    combined <- unlist(per_src, use.names = FALSE)
    if (anyDuplicated(combined)) {
        stop(sprintf("duplicate entries on axis %s across sources; use prefix = TRUE",
                     sQuote(axis)), call. = FALSE)
    }
    format_add_axis(destination, axis, combined)

    # Union of vector names across sources for this axis.
    all_vec_names <- unique(unlist(lapply(sources,
        function(s) format_vectors_set(s, axis)), use.names = FALSE))

    for (vn in all_vec_names) {
        .concat_axis_vector(destination, axis, vn, sources, dataset_names,
                            do_prefix, all_prefixes, prefixed, empty,
                            overwrite, sparse_threshold)
    }

    # Matrices with `axis` as one side and some other-axis on the other.
    other_axes <- unique(unlist(lapply(sources, format_axes_set)))
    other_axes <- setdiff(other_axes, axis)
    for (oa in other_axes) {
        all_mat_names <- unique(unlist(lapply(sources, function(s) {
            if (format_has_axis(s, axis) && format_has_axis(s, oa)) {
                c(format_matrices_set(s, axis, oa),
                  format_matrices_set(s, oa, axis))
            } else character(0L)
        })))
        for (mn in all_mat_names) {
            .concat_axis_matrix(destination, axis, oa, mn, sources,
                                empty, overwrite, sparse_threshold)
        }
    }
}

.concat_axis_vector <- function(destination, axis, name, sources,
                                dataset_names, do_prefix, all_prefixes,
                                prefixed, empty, overwrite, sparse_threshold) {
    parts <- vector("list", length(sources))
    for (i in seq_along(sources)) {
        src <- sources[[i]]
        if (format_has_vector(src, axis, name)) {
            v <- format_get_vector(src, axis, name)$value
        } else {
            key <- paste(axis, name, sep = "|")
            fill <- if (is.null(empty)) NULL else empty[[key]]
            if (is.null(fill)) {
                stop(sprintf(
                    "no empty value for the vector: %s of the axis: %s which is missing from the daf data: %s",
                    sQuote(name), sQuote(axis), S7::prop(src, "name")
                ), call. = FALSE)
            }
            v <- rep(fill, format_axis_length(src, axis))
        }
        is_prefix_target <- {
            if (!is.null(prefixed)) {
                # Julia parity (M4): `prefixed` is an override that fires
                # regardless of `prefix[axis]`. The list names properties
                # whose values reference a prefixed axis and thus need
                # the dataset name spliced in.
                vec <- if (is.list(prefixed)) prefixed[[axis]] else prefixed
                !is.null(vec) && name %in% vec
            } else {
                # Heuristic: prefix if the property name matches (or looks
                # like a subproperty of) any concat axis that is itself
                # prefixed. This remaps cross-axis references so that
                # `cell|cluster` values still line up with the (prefixed)
                # cluster axis entries.
                prefixed_axes <- base::names(all_prefixes)[
                    vapply(all_prefixes, isTRUE, logical(1L))]
                any(vapply(prefixed_axes, function(ax) {
                    name == ax || startsWith(name, paste0(ax, "."))
                }, logical(1L)))
            }
        }
        if (is_prefix_target && is.character(v)) {
            v <- paste(dataset_names[[i]], v, sep = ".")
        }
        parts[[i]] <- v
    }
    out <- do.call(c, parts)
    # Strip names: `format_get_vector(...)$value` is now axis-named, so
    # `do.call(c, parts)` carries source-axis names. When the destination
    # axis is prefix-rewritten or otherwise distinct, those names do not
    # match the destination axis entries and `format_set_vector`'s
    # `.validate_vector_value` correctly rejects the value.
    out <- unname(out)
    format_set_vector(destination, axis, name, out, overwrite = overwrite)
}

.concat_axis_matrix <- function(destination, axis, other_axis, name, sources,
                                empty, overwrite, sparse_threshold) {
    parts <- vector("list", length(sources))
    for (i in seq_along(sources)) {
        src <- sources[[i]]
        have_ao <- format_has_axis(src, axis) && format_has_axis(src, other_axis)
        if (!have_ao) {
            stop(sprintf("source %s missing axes for matrix %s",
                         S7::prop(src, "name"), sQuote(name)), call. = FALSE)
        }
        mat <- if (format_has_matrix(src, other_axis, axis, name)) {
            format_get_matrix(src, other_axis, axis, name)$value
        } else if (format_has_matrix(src, axis, other_axis, name)) {
            m <- format_get_matrix(src, axis, other_axis, name)$value
            if (inherits(m, "dgCMatrix")) Matrix::t(m) else t(m)
        } else {
            key <- paste(other_axis, axis, name, sep = "|")
            alt_key <- paste(axis, other_axis, name, sep = "|")
            fill <- if (is.null(empty)) NULL else (empty[[key]] %||% empty[[alt_key]])
            if (is.null(fill)) {
                stop(sprintf(
                    "no empty value for the matrix: %s of the rows axis: %s and the columns axis: %s which is missing from the daf data: %s",
                    sQuote(name), sQuote(other_axis), sQuote(axis),
                    S7::prop(src, "name")
                ), call. = FALSE)
            }
            matrix(fill,
                   nrow = format_axis_length(src, other_axis),
                   ncol = format_axis_length(src, axis))
        }
        parts[[i]] <- mat
    }
    if (all(vapply(parts, inherits, logical(1L), "dgCMatrix"))) {
        combined <- do.call(cbind, parts)
    } else {
        combined <- do.call(cbind, lapply(parts, function(m)
            if (inherits(m, "dgCMatrix")) as.matrix(m) else m))
    }
    dest_rows <- format_axis_array(destination, other_axis)$value
    dest_cols <- format_axis_array(destination, axis)$value
    dimnames(combined) <- list(dest_rows, dest_cols)
    format_set_matrix(destination, other_axis, axis, name, combined,
                      overwrite = overwrite)
}

.concat_merge <- function(destination, sources, dataset_names, dataset_axis,
                          concat_axes, merge, empty, overwrite,
                          sparse_if_saves_storage_fraction) {
    expanded <- .concat_expand_merge_wildcards(sources, concat_axes, merge)
    for (prop_key in base::names(expanded)) {
        action <- expanded[[prop_key]]
        parts <- strsplit(prop_key, "|", fixed = TRUE)[[1L]]
        if (length(parts) == 1L) {
            .concat_merge_scalar(destination, sources, dataset_names,
                                 dataset_axis, parts[[1L]], action, overwrite)
        } else if (length(parts) == 2L) {
            if (parts[[1L]] %in% concat_axes) next
            .concat_merge_vector(destination, sources, dataset_names,
                                 dataset_axis, parts[[1L]], parts[[2L]],
                                 action, empty, overwrite,
                                 sparse_if_saves_storage_fraction)
        } else if (length(parts) == 3L) {
            if (action == MERGE_COLLECT_AXIS) {
                stop(sprintf(
                    "can't CollectAxis for a matrix: %s (would create a 3D tensor)",
                    sQuote(prop_key)
                ), call. = FALSE)
            }
            if (action == MERGE_LAST_VALUE) {
                .concat_merge_matrix(destination, sources, parts[[1L]],
                                     parts[[2L]], parts[[3L]], overwrite)
            }
        }
    }
}

# Expand `*` wildcards in merge keys (M1 parity with Julia's
# `ALL_SCALARS / ALL_VECTORS / ALL_MATRICES => action` map). Specific
# (non-wildcard) keys override wildcard expansions so users can opt
# out of selected entries.
.concat_expand_merge_wildcards <- function(sources, concat_axes, merge) {
    out <- list()
    explicit <- character(0)
    # Pass 1: explicit keys win over wildcard expansions.
    for (key in base::names(merge)) {
        parts <- strsplit(key, "|", fixed = TRUE)[[1L]]
        if (!any(parts == "*")) {
            out[[key]] <- merge[[key]]
            explicit <- c(explicit, key)
        }
    }
    # Pass 2: expand wildcards against the actual properties present in
    # the sources, skipping keys already pinned in pass 1.
    for (key in base::names(merge)) {
        parts <- strsplit(key, "|", fixed = TRUE)[[1L]]
        if (!any(parts == "*")) next
        action <- merge[[key]]
        if (length(parts) == 1L && parts[[1L]] == "*") {
            for (src in sources) {
                for (n in format_scalars_set(src)) {
                    if (!(n %in% explicit)) out[[n]] <- action
                }
            }
        } else if (length(parts) == 2L) {
            ax_pat <- parts[[1L]]; nm_pat <- parts[[2L]]
            for (src in sources) {
                for (axis in format_axes_set(src)) {
                    if (axis %in% concat_axes) next
                    if (ax_pat != "*" && ax_pat != axis) next
                    for (n in format_vectors_set(src, axis)) {
                        if (nm_pat != "*" && nm_pat != n) next
                        k <- paste(axis, n, sep = "|")
                        if (!(k %in% explicit)) out[[k]] <- action
                    }
                }
            }
        } else if (length(parts) == 3L) {
            r_pat <- parts[[1L]]; c_pat <- parts[[2L]]; nm_pat <- parts[[3L]]
            for (src in sources) {
                axes <- format_axes_set(src)
                for (rax in axes) {
                    if (r_pat != "*" && r_pat != rax) next
                    # Matrices with a concat axis on either side are
                    # stitched by .concat_axis_matrix; merge handlers
                    # only own matrices with BOTH axes off-concat.
                    # Skipping here mirrors the vector branch above
                    # (`if (axis %in% concat_axes) next`).
                    if (rax %in% concat_axes) next
                    for (cax in axes) {
                        if (c_pat != "*" && c_pat != cax) next
                        if (cax %in% concat_axes) next
                        for (n in format_matrices_set(src, rax, cax)) {
                            if (nm_pat != "*" && nm_pat != n) next
                            k <- paste(rax, cax, n, sep = "|")
                            if (!(k %in% explicit)) out[[k]] <- action
                        }
                    }
                }
            }
        }
    }
    out
}

.concat_merge_scalar <- function(destination, sources, dataset_names,
                                 dataset_axis, name, action, overwrite) {
    if (action == MERGE_SKIP) return(invisible())
    if (action == MERGE_LAST_VALUE) {
        for (i in rev(seq_along(sources))) {
            if (format_has_scalar(sources[[i]], name)) {
                format_set_scalar(destination, name,
                                  format_get_scalar(sources[[i]], name)$value,
                                  overwrite = overwrite)
                return(invisible())
            }
        }
        return(invisible())
    }
    if (action == MERGE_COLLECT_AXIS) {
        if (is.null(dataset_axis)) {
            stop(sprintf(
                "can't collect axis for the scalar: %s\nof the daf data sets concatenated into the daf data: %s\nbecause no data set axis was created",
                name, S7::prop(destination, "name")
            ), call. = FALSE)
        }
        # Julia parity: concatenate_merge_scalar errors (eltype(nothing)) when
        # a source lacks the scalar, rather than collecting a silent NA.
        present <- vapply(sources, function(s) format_has_scalar(s, name),
                          logical(1L))
        if (!all(present)) {
            stop(sprintf(
                "can't collect axis for the scalar: %s\nof the daf data sets concatenated into the daf data: %s\nbecause it is missing from %d of the %d data sets",
                name, S7::prop(destination, "name"), sum(!present), length(sources)
            ), call. = FALSE)
        }
        vals <- lapply(sources, function(s) format_get_scalar(s, name)$value)
        format_set_vector(destination, dataset_axis, name,
                          do.call(c, vals), overwrite = overwrite)
    }
}

.concat_merge_vector <- function(destination, sources, dataset_names,
                                 dataset_axis, axis, name, action,
                                 empty, overwrite,
                                 sparse_if_saves_storage_fraction) {
    if (action == MERGE_SKIP) return(invisible())
    if (action == MERGE_LAST_VALUE) {
        for (i in rev(seq_along(sources))) {
            if (format_has_vector(sources[[i]], axis, name)) {
                # Strip source-axis names so .validate_vector_value doesn't
                # reject them when the destination axis differs (mirrors the
                # .concat_axis_vector unname() defense).
                format_set_vector(destination, axis, name,
                                  unname(format_get_vector(sources[[i]], axis,
                                                           name)$value),
                                  overwrite = overwrite)
                return(invisible())
            }
        }
        return(invisible())
    }
    if (action == MERGE_COLLECT_AXIS) {
        if (is.null(dataset_axis)) {
            stop(sprintf(
                "can't collect axis for the vector: %s on axis %s because no dataset axis was created",
                sQuote(name), sQuote(axis)
            ), call. = FALSE)
        }
        # Look up the per-property fill once; a source missing the vector
        # without a fill is an error (checked up front so the sparse path
        # can treat NULL slots as zero columns).
        fill <- if (is.null(empty)) NULL else empty[[paste(axis, name, sep = "|")]]
        vals <- vector("list", length(sources))
        for (i in seq_along(sources)) {
            s <- sources[[i]]
            if (format_has_vector(s, axis, name)) {
                vals[[i]] <- format_get_vector(s, axis, name)$value
            } else if (is.null(fill)) {
                stop(sprintf(
                    "no empty value for the vector: %s of the axis: %s which is missing from the daf data: %s",
                    sQuote(name), sQuote(axis), S7::prop(s, "name")
                ), call. = FALSE)
            }
        }
        src_len <- format_axis_length(destination, axis)
        dimn <- list(format_axis_array(destination, axis)$value,
                     dataset_names)
        out <- .concat_collect_vector_matrix(
            sources, vals, fill, axis, name, src_len, dimn,
            sparse_if_saves_storage_fraction
        )
        format_set_matrix(destination, axis, dataset_axis, name, out,
                          overwrite = overwrite)
    }
}

# nnz of the STORED form of a vector. Julia's sparse-savings heuristic
# (concat.jl sparse_vectors_storage_fraction) counts a dense-stored
# source at its full length even when its values are mostly zero; only
# sparse STORAGE contributes its nnz. dafr densifies vectors at the
# format_get_vector boundary (names contract), so sparsity is probed at
# the raw storage layer here. Returns NA_integer_ for dense or unknown
# storage (chains, views, zarr fall back to "dense", i.e. full length).
.concat_stored_vector_nnz <- function(src, axis, name) {
    if (S7::S7_inherits(src, MemoryDaf)) {
        env <- .memory_axis_vectors(src, axis, create = FALSE)
        if (!is.null(env) && exists(name, envir = env, inherits = FALSE)) {
            v <- get(name, envir = env, inherits = FALSE)
            if (methods::is(v, "sparseVector")) return(length(v@i))
        }
        return(NA_integer_)
    }
    if (S7::S7_inherits(src, FilesDaf) || S7::S7_inherits(src, FilesDafReadOnly)) {
        v <- .files_get_vector_cached(src, axis, name)
        if (methods::is(v, "sparseVector")) return(length(v@i))
        return(NA_integer_)
    }
    NA_integer_
}

# Julia sizeof(eltype) for the merged dtype of the collected values.
.concat_eltype_bytes <- function(vals, fill) {
    has_double <- !is.null(fill) && is.double(fill)
    has_int <- !is.null(fill) && is.integer(fill)
    has_lgl <- !is.null(fill) && is.logical(fill)
    for (v in vals) {
        if (is.null(v)) next
        if (is.double(v)) has_double <- TRUE
        else if (is.integer(v)) has_int <- TRUE
        else if (is.logical(v)) has_lgl <- TRUE
    }
    if (has_double) 8L else if (has_int) 4L else if (has_lgl) 1L else 8L
}

# Julia TanayLabUtilities.indtype_for_size: smallest uint type that
# holds `size` (UInt16 / UInt32 / UInt64).
.concat_indtype_bytes <- function(size) {
    if (size <= 65535) 2L else if (size <= 4294967295) 4L else 8L
}

# COLLECT_AXIS materialization: decide sparse vs dense via Julia's
# storage-savings heuristic (concat.jl concatenate_merge_vector), then
# build the (axis x dataset) matrix. `vals[[i]]` is the named dense
# value of source i, or NULL for a source missing the vector (only
# reachable when `fill` is non-NULL).
.concat_collect_vector_matrix <- function(sources, vals, fill, axis, name,
                                          src_len, dimn,
                                          sparse_if_saves_storage_fraction) {
    n_src <- length(sources)
    # Strings are never sparse (Julia: eltype != String guard). bit64
    # integer64 is double-backed; Matrix can't represent it - keep dense.
    never_sparse <-
        (!is.null(fill) && (is.character(fill) || inherits(fill, "integer64"))) ||
        any(vapply(vals, function(v) {
            !is.null(v) && (is.character(v) || inherits(v, "integer64"))
        }, logical(1)))
    if (!never_sparse) {
        dense_n <- src_len * n_src
        nnz_n <- 0
        for (i in seq_len(n_src)) {
            if (!is.null(vals[[i]])) {
                stored <- .concat_stored_vector_nnz(sources[[i]], axis, name)
                nnz_n <- nnz_n + (if (is.na(stored)) src_len else stored)
            } else if (!isTRUE(all(fill == 0))) {
                # Missing source with a nonzero fill is a dense column.
                nnz_n <- nnz_n + src_len
            }
        }
        el_b <- .concat_eltype_bytes(vals, fill)
        ind_b <- .concat_indtype_bytes(dense_n)
        saves <- (dense_n * el_b - nnz_n * (el_b + ind_b)) / (dense_n * el_b)
        if (saves >= sparse_if_saves_storage_fraction) {
            parts_i <- vector("list", n_src)
            parts_x <- vector("list", n_src)
            for (i in seq_len(n_src)) {
                if (is.null(vals[[i]])) next # fill == 0: zero column
                col <- unname(vals[[i]])
                nz <- which(col != 0)
                parts_i[[i]] <- nz
                parts_x[[i]] <- col[nz]
            }
            ii <- unlist(parts_i, use.names = FALSE)
            if (is.null(ii)) ii <- integer(0)
            jj <- rep.int(seq_len(n_src),
                          vapply(parts_i, length, integer(1)))
            xx <- unlist(parts_x, use.names = FALSE)
            if (is.null(xx)) xx <- double(0)
            return(Matrix::sparseMatrix(
                i = ii, j = jj, x = xx,
                dims = c(src_len, n_src), dimnames = dimn
            ))
        }
    }
    # Dense path: prototype value so column types cohere across
    # present / absent sources.
    proto <- NA
    for (v in vals) {
        if (!is.null(v)) {
            proto <- v[NA_integer_]
            break
        }
    }
    if (is.na(proto) && !is.null(fill)) proto <- fill[NA_integer_]
    out <- matrix(proto, nrow = src_len, ncol = n_src, dimnames = dimn)
    for (i in seq_len(n_src)) {
        out[, i] <- if (!is.null(vals[[i]])) vals[[i]] else fill
    }
    out
}

.concat_merge_matrix <- function(destination, sources, rows_axis, cols_axis,
                                 name, overwrite) {
    # MERGE_LAST_VALUE for a matrix property (3-part key) where neither
    # rows_axis nor cols_axis is in the concat set: pick the last source
    # holding the matrix and stamp it on the destination.
    for (i in rev(seq_along(sources))) {
        s <- sources[[i]]
        if (format_has_axis(s, rows_axis) && format_has_axis(s, cols_axis) &&
            format_has_matrix(s, rows_axis, cols_axis, name)) {
            mat <- format_get_matrix(s, rows_axis, cols_axis, name)$value
            format_set_matrix(destination, rows_axis, cols_axis, name, mat,
                              overwrite = overwrite)
            return(invisible())
        }
    }
    invisible()
}
