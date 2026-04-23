#' @include query_builders.R
NULL

# Exports populated in phases B-F.

# ---- Phase B: Element-wise builders ---------------------------------------

#' Element-wise query operation: absolute value.
#'
#' Builds a `% Abs` query fragment. Chain after a vector or matrix
#' lookup via `|>` to apply element-wise absolute value.
#'
#' @param ... Optional [DafrQuery] pipe target.
#' @return A [DafrQuery].
#' @examples
#' Abs()
#' DafrQuery(ast = parse_query("@ cell : age"), canonical = "@ cell : age") |>
#'     Abs()
#' @seealso [Round()], [Log()], [Clamp()]
#' @export
Abs <- .make_nullary("Abs", .qop_abs)

#' Element-wise query operation: round to nearest integer.
#'
#' Builds a `% Round` query fragment.
#'
#' @param ... Optional [DafrQuery] pipe target.
#' @return A [DafrQuery].
#' @examples
#' Round()
#' @seealso [Abs()], [Significant()]
#' @export
Round <- .make_nullary("Round", .qop_round)

#' Element-wise query operation: keep only "significant" entries.
#'
#' Builds a `% Significant <params>` query fragment. Zeroes entries
#' whose absolute value is below `high`, optionally preserving entries
#' between `low` and `high` when any entry in the group is above
#' `high`.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Named parameters such as `high`, `low`; or a piped
#'   [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Significant(high = 3)
#' Significant(high = 3, low = 2)
#' @seealso [Abs()], [Clamp()]
#' @export
Significant <- .make_typed_reduction("Significant", .qop_significant)

#' Element-wise query operation: clamp values to a range.
#'
#' Builds a `% Clamp <params>` query fragment. Values less than `min`
#' become `min`; values greater than `max` become `max`.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Named parameters `min`, `max`; or a piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Clamp(min = 0, max = 1)
#' @seealso [Significant()], [Convert()]
#' @export
Clamp <- .make_typed_reduction("Clamp", .qop_clamp)

#' Element-wise query operation: convert values to a given type.
#'
#' Builds a `% Convert type <value>` query fragment.
#'
#' @param type Target type as a character scalar (e.g. `"Int64"`,
#'   `"Float32"`). Positional.
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Convert(type = "Int64")
#' @seealso [Clamp()], [Round()]
#' @export
Convert <- .make_typed_reduction("Convert", .qop_convert)

#' Element-wise query operation: convert each entry to its fraction of
#' the total.
#'
#' Builds a `% Fraction` query fragment. Each value is divided by the
#' sum of its vector (or matrix column).
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Fraction()
#' @seealso [Log()], [Clamp()]
#' @export
Fraction <- .make_typed_reduction("Fraction", .qop_fraction)

#' Element-wise query operation: logarithm.
#'
#' Builds a `% Log <params>` query fragment.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Named parameters `base` (default natural log), `eps`
#'   (added to operands to avoid `log(0)`); or a piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Log(base = 2, eps = 1e-6)
#' @seealso [Fraction()], [Clamp()]
#' @export
Log <- .make_typed_reduction("Log", .qop_log)
