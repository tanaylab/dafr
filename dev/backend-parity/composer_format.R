# dev/backend-parity/composer_format.R
#
# Round-7 follow-up: exercise composers (viewer / chain_reader /
# chain_writer / concatenate) against the same 84-item manifest.
# Reference is the memory baseline; reads from every composer should
# match it (modulo documented semantic differences).
#
# Output: composer.jsonl, joinable by `key` against the harness diff
# tool. Composer label goes in the `backend` field as
# "composer:<name>".

suppressMessages(devtools::load_all("/home/aviezerl/src/dafr-native"))
source("/home/aviezerl/src/dafr-native/dev/backend-parity/build_fixture_R.R")
source("/home/aviezerl/src/dafr-native/dev/backend-parity/serialize.R")

OUT_PATH <- "/home/aviezerl/src/dafr-native/dev/backend-parity/composer.jsonl"

# ---------------------------------------------------------------------
# Per-item read (same as round_trip.R's .do_read).
# ---------------------------------------------------------------------
.do_read_c <- function(daf, item) {
    if (item$kind == "scalar")  return(get_scalar(daf, item$name))
    if (item$kind == "axis")    return(axis_vector(daf, item$axis))
    if (item$kind == "vector")  return(get_vector(daf, item$axis, item$name))
    if (item$kind == "matrix")  return(get_matrix(daf, item$axis, item$cols_axis,
                                                  item$name))
    stop("unknown kind: ", item$kind)
}

.read_one <- function(daf, label, item) {
    tryCatch({
        v <- .do_read_c(daf, item)
        serialize_read(label, item, v, status = "ok")
    }, error = function(e) {
        serialize_read(label, item, NULL, status = "error", cond = e)
    })
}

.run_label <- function(daf, label, manifest, con) {
    cat(sprintf("== %s ==\n", label))
    nerr <- 0L
    for (item in manifest) {
        rec <- .read_one(daf, label, item)
        if (identical(rec$status, "error")) nerr <- nerr + 1L
        write_record_jsonl(con, rec)
    }
    cat(sprintf("   wrote %d records  (errors=%d)\n",
                length(manifest), nerr))
}

# ---------------------------------------------------------------------
# Concatenate fixture: split cell axis across two sources, leave the
# other axes shared and identical across them. Per-cell vectors get
# stitched; everything else just copies through.
# ---------------------------------------------------------------------

.build_concat_sources <- function() {
    # Start from a full fixture in a throwaway daf, then carve.
    ref <- memory_daf("ref_for_concat")
    build_fixture(ref)

    cells <- axis_vector(ref, "cell")
    stopifnot(length(cells) == 5L)
    a_cells <- cells[1:2]
    b_cells <- cells[3:5]

    .make_part <- function(part_name, picked_cells) {
        d <- memory_daf(part_name)
        # Scalars - same on both sources so concatenate copies once.
        for (nm in scalars_set(ref)) {
            set_scalar(d, nm, get_scalar(ref, nm))
        }
        # Axes - cell is the only one we split.
        for (ax in axes_set(ref)) {
            if (ax == "cell") {
                add_axis(d, "cell", picked_cells)
            } else {
                add_axis(d, ax, axis_vector(ref, ax))
            }
        }
        # Vectors - per-cell ones get sliced; the rest get copied.
        for (ax in axes_set(ref)) {
            for (vn in vectors_set(ref, ax)) {
                v <- get_vector(ref, ax, vn)
                if (ax == "cell") {
                    v <- v[picked_cells]
                }
                # axis_vector for cell returns names; strip them so
                # set_vector receives an axis-ordered unnamed vec.
                set_vector(d, ax, vn, unname(v))
            }
        }
        # Matrices - skip cell x cell (concatenate rejects matrices
        # with both axes in the concat set). Slice cell-x-other and
        # other-x-cell; pass everything else through.
        for (ra in axes_set(ref)) for (ca in axes_set(ref)) {
            if (ra == "cell" && ca == "cell") next
            for (mn in matrices_set(ref, ra, ca)) {
                m <- get_matrix(ref, ra, ca, mn)
                rsel <- if (ra == "cell") picked_cells else NULL
                csel <- if (ca == "cell") picked_cells else NULL
                if (!is.null(rsel) || !is.null(csel)) {
                    # Matrix package supports sparse slicing natively;
                    # don't densify so concatenate sees the original
                    # storage class.
                    m2 <- m[
                        if (!is.null(rsel)) rsel else seq_len(nrow(m)),
                        if (!is.null(csel)) csel else seq_len(ncol(m)),
                        drop = FALSE
                    ]
                    set_matrix(d, ra, ca, mn, m2)
                } else {
                    set_matrix(d, ra, ca, mn, m)
                }
            }
        }
        d
    }

    list(
        a = .make_part("part_a", a_cells),
        b = .make_part("part_b", b_cells)
    )
}

# Cell x cell matrices are not concat-compatible; filter them out of
# the manifest for the concatenate pass so we only emit records the
# composer is expected to support.
.concat_compatible <- function(item) {
    !(item$kind == "matrix" &&
      identical(item$axis, "cell") &&
      identical(item$cols_axis, "cell"))
}

# ---------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------
main_composer <- function() {
    manifest <- fixture_manifest()
    cat(sprintf("manifest: %d items\n", length(manifest)))

    con <- file(OUT_PATH, "w")
    on.exit(close(con), add = TRUE)

    # ---- Memory baseline ----
    base <- memory_daf("baseline")
    build_fixture(base)
    .run_label(base, "memory", manifest, con)

    # ---- viewer (no overrides) ----
    v <- viewer(base)
    .run_label(v, "composer:viewer", manifest, con)

    # ---- chain_reader, single-element wrap ----
    cr1 <- chain_reader(list(base))
    .run_label(cr1, "composer:chain_reader_single", manifest, con)

    # ---- chain_writer: writes routed to a fresh tail, read back via the chain ----
    stub <- memory_daf("chain_stub")
    tail <- memory_daf("chain_tail")
    cw <- chain_writer(list(stub, tail))
    build_fixture(cw)
    .run_label(cw, "composer:chain_writer", manifest, con)

    # ---- concatenate: split cell axis 2/3 across two memory sources,
    #                   then read every manifest item from the result. ----
    # Default merge=NULL means SKIP for everything off the concat axis -
    # i.e. only concat-axis vectors/matrices flow through. To actually
    # test pass-through we ask for MERGE_LAST_VALUE on every scalar
    # ("*"), every vector ("*|*"), and every matrix ("*|*|*").
    parts <- .build_concat_sources()
    dst <- memory_daf("concat_dst")
    concatenate(
        dst, "cell", list(parts$a, parts$b),
        dataset_axis = NULL, prefix = FALSE,
        merge = list("*" = MERGE_LAST_VALUE,
                     "*|*" = MERGE_LAST_VALUE,
                     "*|*|*" = MERGE_LAST_VALUE)
    )
    concat_manifest <- Filter(.concat_compatible, manifest)
    cat(sprintf("   concat-compatible items: %d / %d\n",
                length(concat_manifest), length(manifest)))
    .run_label(dst, "composer:concatenate", concat_manifest, con)

    cat(sprintf("\nWROTE: %s\n", OUT_PATH))
}

if (!interactive() && identical(sys.nframe(), 0L) &&
    identical(basename(sub("^--file=", "",
        grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
        "composer_format.R")) {
    main_composer()
}
