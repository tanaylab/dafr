#' Generate names for groups of axis entries
#'
#' Given groups of axis entries (as indices), generates unique names for each group
#' based on the entry names using a hash.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @param entries_of_groups A list of integer vectors, where each vector contains
#'   the indices of entries belonging to a group
#' @param prefix A prefix to add to each generated name
#' @return A character vector of unique group names
#' @details This function generates deterministic names for groups based on their
#'   member entries. Groups with the same members will always get the same name.
#'   This is useful for creating reproducible group identifiers.
#' @export
group_names <- function(daf, axis, entries_of_groups, prefix) {
    validate_daf_object(daf)
    # Convert R list to Julia Vector{Vector{Int}}
    entries_of_groups <- lapply(entries_of_groups, as.integer)
    # Use julia_eval to properly construct the typed vector
    jl_entries <- JuliaCall::julia_eval("Vector{Vector{Int}}()")
    for (group in entries_of_groups) {
        JuliaCall::julia_call("push!", jl_entries, group)
    }
    julia_call("DataAxesFormats.Groups.group_names", daf$jl_obj, axis, jl_entries, prefix = prefix)
}

#' Compact group indices
#'
#' Given a vector of group indices (where 0 means no group), compacts the indices
#' so they are in the range 1 to N, where N is the number of unique groups.
#'
#' @param group_indices An integer vector where element i contains the group index
#'   for entry i. A value of 0 indicates the entry belongs to no group.
#' @return A list with two elements:
#'   - n_groups: The number of unique groups (N)
#'   - group_indices: The compacted group indices (in range 1..N, with 0 unchanged)
#' @details This function is useful when group indices have gaps (e.g., groups 5
#'   and 10) and need to be renumbered to consecutive integers (1 and 2).
#' @examples
#' \dontrun{
#' # Groups 5 and 10 with gaps
#' group_indices <- c(5L, 10L, 5L, 0L, 10L)
#' result <- compact_groups(group_indices)
#' # result$n_groups is 2
#' # result$group_indices is c(1L, 2L, 1L, 0L, 2L)
#' }
#' @export
compact_groups <- function(group_indices) {
    group_indices <- as.integer(group_indices)
    n_groups <- 0L
    compacts_of_groups <- new.env(hash = TRUE)

    for (i in seq_along(group_indices)) {
        group_index <- group_indices[i]
        if (group_index != 0L) {
            key <- as.character(group_index)
            compact_of_group <- compacts_of_groups[[key]]
            if (is.null(compact_of_group)) {
                n_groups <- n_groups + 1L
                compact_of_group <- n_groups
                compacts_of_groups[[key]] <- compact_of_group
            }
            group_indices[i] <- compact_of_group
        }
    }

    list(
        n_groups = n_groups,
        group_indices = group_indices
    )
}

#' Collect group members from group indices
#'
#' Given a vector where each element is a group index for the corresponding entry,
#' returns a list of vectors containing the indices of entries in each group.
#'
#' @param group_indices An integer vector where element i contains the group index
#'   for entry i. A value of 0 indicates the entry belongs to no group.
#' @return A list of integer vectors, where each vector contains the indices of
#'   entries belonging to that group
#' @details This is the inverse operation of assigning group indices. It's useful
#'   for converting a per-entry group assignment into a per-group member list.
#' @examples
#' \dontrun{
#' # If entries 1,3 are in group 1, entry 2 is in group 2, entry 4 is in no group
#' group_indices <- c(1L, 2L, 1L, 0L)
#' members <- collect_group_members(group_indices)
#' # Returns: list(c(1L, 3L), c(2L))
#' }
#' @export
collect_group_members <- function(group_indices) {
    result <- julia_call("DataAxesFormats.Groups.collect_group_members", as.integer(group_indices))
    # Convert Julia vectors to R list
    lapply(result, as.integer)
}
