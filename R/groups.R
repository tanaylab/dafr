#' @include classes.R format_api.R utils.R
NULL

# ---- FNV-32 hash (stable across sessions; pure R; no deps) ----------------

.FNV32_OFFSET <- 2166136261  # stored as double to dodge int32 overflow
.FNV32_PRIME  <- 16777619

.fnv32_hex <- function(bytes) {
    # h is maintained as an unsigned 32-bit value in double storage [0, 2^32).
    # bitwXor requires signed int32, so convert before XOR and back after.
    h <- .FNV32_OFFSET
    for (b in as.integer(bytes)) {
        # Convert unsigned double -> signed int32 for bitwXor
        h_signed <- as.integer(h - (h >= 2147483648) * 4294967296)
        xored <- bitwXor(h_signed, b)
        # Convert signed int32 back to unsigned double
        h <- as.double(xored) + (xored < 0L) * 4294967296
        # Multiply mod 2^32 via double arithmetic; FNV prime fits comfortably.
        h <- (h * .FNV32_PRIME) %% 4294967296
    }
    sprintf("%08x", as.integer(h - (h >= 2147483648) * 4294967296))
}

.stable_hash <- function(strings) {
    # Sort + SOH-join so that group_names is member-order invariant
    # but sensitive to membership and to entry names.
    # Use \x01 (SOH) as separator; NUL is not allowed in R strings.
    payload <- paste(sort(unique(strings)), collapse = "\x01")
    .fnv32_hex(charToRaw(payload))
}

# ---- compact_groups --------------------------------------------------------

#' Renumber group indices to be dense in 1..N.
#'
#' Given a vector where element `i` is the group index for entry `i`
#' (0 denotes "no group"), return a list with `n_groups` (the number
#' of unique non-zero groups) and `group_indices` (the renumbered
#' indices, using first-seen order). Zeros are preserved.
#'
#' @param group_indices Integer vector (or coercible).
#' @return `list(n_groups = integer(1), group_indices = integer(N))`.
#' @examples
#' compact_groups(c(5L, 10L, 5L, 0L, 10L))
#' # $n_groups = 2; $group_indices = c(1, 2, 1, 0, 2)
#' @seealso [collect_group_members()], [group_names()]
#' @export
compact_groups <- function(group_indices) {
    group_indices <- as.integer(group_indices)
    if (any(!is.na(group_indices) & group_indices < 0L)) {
        cli::cli_abort(
            "`group_indices` must be >= 0 (0 denotes no group)",
            call = NULL
        )
    }
    n_groups <- 0L
    out <- group_indices
    seen <- new.env(parent = emptyenv(), hash = TRUE)
    for (i in seq_along(group_indices)) {
        gi <- group_indices[[i]]
        if (is.na(gi) || gi == 0L) {
            next
        }
        key <- as.character(gi)
        compact <- seen[[key]]
        if (is.null(compact)) {
            n_groups <- n_groups + 1L
            compact <- n_groups
            seen[[key]] <- compact
        }
        out[[i]] <- compact
    }
    list(n_groups = n_groups, group_indices = out)
}

# ---- collect_group_members -------------------------------------------------

#' Invert group-index assignment into per-group entry lists.
#'
#' For each non-zero group in `group_indices`, returns the integer
#' positions that belong to it. Entries with index 0 are omitted.
#'
#' @inheritParams compact_groups
#' @return A list of integer vectors. Length equals `max(group_indices)`.
#' @examples
#' collect_group_members(c(1L, 2L, 1L, 0L, 2L))
#' # list(c(1, 3), c(2, 5))
#' @seealso [compact_groups()], [group_names()]
#' @export
collect_group_members <- function(group_indices) {
    group_indices <- as.integer(group_indices)
    if (length(group_indices) == 0L) {
        return(list())
    }
    if (any(!is.na(group_indices) & group_indices < 0L)) {
        cli::cli_abort(
            "`group_indices` must be >= 0 (0 denotes no group)",
            call = NULL
        )
    }
    n_groups <- max(0L, max(group_indices, na.rm = TRUE))
    if (n_groups == 0L) {
        return(list())
    }
    out <- vector("list", n_groups)
    for (i in seq_along(group_indices)) {
        gi <- group_indices[[i]]
        if (!is.na(gi) && gi > 0L) {
            out[[gi]] <- c(out[[gi]], i)
        }
    }
    out <- lapply(out, function(v) if (is.null(v)) integer(0L) else v)
    out
}

# ---- group_names -----------------------------------------------------------

#' Deterministic names for groups of axis entries.
#'
#' For each group in `entries_of_groups`, build a name from a stable
#' hash of the member entry names. Same-members => same name across
#' sessions and dafs.
#'
#' @param daf A [DafReader].
#' @param axis Axis name whose entries the group indices reference.
#' @param entries_of_groups A list of integer vectors; each vector is
#'   the 1-based axis positions belonging to the group.
#' @param prefix Character scalar prepended to each name.
#' @return Character vector of length `length(entries_of_groups)`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
#' group_names(d, "cell", list(c(1L, 2L), c(3L, 4L)), prefix = "grp_")
#' @seealso [compact_groups()], [collect_group_members()]
#' @export
group_names <- function(daf, axis, entries_of_groups, prefix) {
    stopifnot("`daf` must be a DafReader" = S7::S7_inherits(daf, DafReader))
    stopifnot(
        "`entries_of_groups` must be a list" = is.list(entries_of_groups),
        "`prefix` must be a character scalar" =
            is.character(prefix) && length(prefix) == 1L
    )
    entry_names <- format_axis_array(daf, axis)$value
    n <- length(entry_names)
    vapply(entries_of_groups, function(members) {
        members <- as.integer(members)
        if (length(members) && (any(members < 1L) || any(members > n))) {
            cli::cli_abort(
                "group member index out of range for axis {.val {axis}} (1..{n})",
                call = NULL
            )
        }
        names <- entry_names[members]
        paste0(prefix, .stable_hash(names))
    }, character(1L))
}
