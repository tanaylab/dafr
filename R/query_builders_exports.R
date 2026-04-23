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

# ---- Phase C: Reduction builders -----------------------------------------

#' Reduction query operation: maximum.
#'
#' Builds a `% Max` query fragment. Consumes a matrix axis (or the
#' single axis of a vector) and produces a reduced result.
#'
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Max()
#' @seealso [Min()], [Mean()], [Sum()]
#' @export
Max <- .make_nullary("Max", .qop_max)

#' Reduction query operation: minimum.
#'
#' Builds a `% Min` query fragment.
#'
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Min()
#' @seealso [Max()], [Mean()], [Sum()]
#' @export
Min <- .make_nullary("Min", .qop_min)

#' Reduction query operation: most-frequent value.
#'
#' Builds a `% Mode` query fragment. Returns the most common value of
#' the reduced axis. Numeric-only.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Mode()
#' @seealso [Median()]
#' @export
Mode <- .make_typed_reduction("Mode", .qop_mode)

#' Reduction query operation: sum.
#'
#' Builds a `% Sum` query fragment (optionally `% Sum type: <T>`).
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Sum()
#' Sum(type = "Float64")
#' @seealso [Mean()], [Max()]
#' @export
Sum <- .make_typed_reduction("Sum", .qop_sum)

#' Reduction query operation: mean.
#'
#' Builds a `% Mean` query fragment (optionally `% Mean type: <T>`).
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Mean()
#' Mean(type = "Float64")
#' @seealso [Sum()], [Median()]
#' @export
Mean <- .make_typed_reduction("Mean", .qop_mean)

#' Reduction query operation: median.
#'
#' Builds a `% Median` query fragment.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Median()
#' @seealso [Mean()], [Quantile()]
#' @export
Median <- .make_typed_reduction("Median", .qop_median)

#' Reduction query operation: count non-zero entries.
#'
#' Builds a `% Count` query fragment.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Count()
#' @seealso [Sum()]
#' @export
Count <- .make_typed_reduction("Count", .qop_count)

#' Reduction query operation: geometric mean.
#'
#' Builds a `% GeoMean` query fragment (optionally with `eps` to avoid
#' `log(0)`).
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Named parameter `eps`; or a piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' GeoMean()
#' GeoMean(eps = 1)
#' @seealso [Mean()]
#' @export
GeoMean <- .make_typed_reduction("GeoMean", .qop_geomean)

#' Reduction query operation: quantile.
#'
#' Builds a `% Quantile p: <p>` query fragment. The `p` parameter must
#' be provided by name (positional first argument is `type`).
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Named parameter `p` (quantile in 0..1); or a piped
#'   [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Quantile(p = 0.5)
#' Quantile(p = 0.9, type = "Float64")
#' @seealso [Median()]
#' @export
Quantile <- .make_typed_reduction("Quantile", .qop_quantile)

#' Reduction query operation: standard deviation.
#'
#' Builds a `% Std` query fragment.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Std()
#' @seealso [Var()], [StdN()]
#' @export
Std <- .make_typed_reduction("Std", .qop_std)

#' Reduction query operation: standard deviation normalised by the
#' mean.
#'
#' Builds a `% StdN` query fragment (optionally with `eps` to avoid
#' division by zero).
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Named parameter `eps`; or a piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' StdN()
#' StdN(eps = 1)
#' @seealso [Std()]
#' @export
StdN <- .make_typed_reduction("StdN", .qop_std_n)

#' Reduction query operation: variance.
#'
#' Builds a `% Var` query fragment.
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Var()
#' @seealso [Std()], [VarN()]
#' @export
Var <- .make_typed_reduction("Var", .qop_var)

#' Reduction query operation: variance normalised by the mean.
#'
#' Builds a `% VarN` query fragment (optionally with `eps` to avoid
#' division by zero).
#'
#' @param type Optional output type (character scalar) or a piped
#'   [DafrQuery].
#' @param ... Named parameter `eps`; or a piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' VarN()
#' VarN(eps = 1)
#' @seealso [Var()]
#' @export
VarN <- .make_typed_reduction("VarN", .qop_var_n)

#' Group-by query operation.
#'
#' Builds a `/ <property>` query fragment. Partitions the prior axis's
#' entries by `property` and then expects a subsequent reduction
#' (`Sum()`, `Mean()`, ...).
#'
#' @param property Property name to group by (character scalar), or a
#'   piped [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' GroupBy("donor")
#' @seealso [GroupRowsBy()], [GroupColumnsBy()], [CountBy()]
#' @export
GroupBy <- .make_string_op("GroupBy", .qop_group_by)

#' Group-rows-by query operation.
#'
#' Builds a `-/ <property>` query fragment. Groups rows of a matrix by
#' `property`; typically followed by [ReduceToRow()].
#'
#' @param property Property name to group rows by, or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' GroupRowsBy("donor")
#' @seealso [GroupBy()], [GroupColumnsBy()]
#' @export
GroupRowsBy <- .make_string_op("GroupRowsBy", .qop_group_rows_by)

#' Group-columns-by query operation.
#'
#' Builds a `|/ <property>` query fragment. Groups columns of a matrix
#' by `property`; typically followed by [ReduceToColumn()].
#'
#' @param property Property name to group columns by, or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' GroupColumnsBy("type")
#' @seealso [GroupBy()], [GroupRowsBy()]
#' @export
GroupColumnsBy <- .make_string_op("GroupColumnsBy", .qop_group_columns_by)

#' Count-by query operation.
#'
#' Builds a `* <property>` query fragment. Counts entries of the prior
#' axis, grouped by `property`.
#'
#' @param property Property name to count by, or a piped [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' CountBy("donor")
#' @seealso [GroupBy()]
#' @export
CountBy <- .make_string_op("CountBy", .qop_count_by)

#' Reduce-to-column query operation.
#'
#' Builds a `>| <Reduction>` query fragment from a reduction query.
#' Converts each row of a matrix to a single value via the given
#' reduction (`Sum()`, `Mean()`, `Quantile(p = 0.5)`, ...).
#'
#' @param reduction A reduction [DafrQuery] (e.g. `Sum()`,
#'   `Quantile(p = 0.5)`), or a piped [DafrQuery] with the reduction
#'   supplied in `...`.
#' @param ... The reduction [DafrQuery] when `reduction` holds the
#'   piped prior.
#' @return A [DafrQuery].
#' @examples
#' ReduceToColumn(Sum())
#' ReduceToColumn(Quantile(p = 0.5))
#' @seealso [ReduceToRow()]
#' @export
ReduceToColumn <- .make_reduce_to("ReduceToColumn", .qop_reduce_to_column)

#' Reduce-to-row query operation.
#'
#' Builds a `>- <Reduction>` query fragment from a reduction query.
#' Converts each column of a matrix to a single value via the given
#' reduction.
#'
#' @param reduction A reduction [DafrQuery] (e.g. `Sum()`,
#'   `Mean()`), or a piped [DafrQuery] with the reduction supplied in
#'   `...`.
#' @param ... The reduction [DafrQuery] when `reduction` holds the
#'   piped prior.
#' @return A [DafrQuery].
#' @examples
#' ReduceToRow(Sum())
#' @seealso [ReduceToColumn()]
#' @export
ReduceToRow <- .make_reduce_to("ReduceToRow", .qop_reduce_to_row)

# ---- Phase D: Selection/axis builders -------------------------------------

#' Axis selection query operation.
#'
#' Builds an `@ <axis_name>` query fragment, selecting the named axis
#' as the next input to the query.
#'
#' @param axis_name Axis name (character scalar), or a piped [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Axis("cell")
#' Axis("cell") |> Axis("gene")
#' @seealso [AsAxis()], [LookupVector()], [LookupMatrix()]
#' @export
Axis <- .make_string_op("Axis", .qop_axis, param_name = "axis_name")

#' Begin-mask query operation.
#'
#' Builds a `[ <property>` query fragment that opens a masked subquery
#' against `property`. Close the mask with [EndMask()].
#'
#' @param property Property name (character scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' BeginMask("type")
#' Axis("cell") |> BeginMask("type") |> EndMask()
#' @seealso [BeginNegatedMask()], [EndMask()], [AndMask()]
#' @export
BeginMask <- .make_string_op(
    "BeginMask",
    function(prop) .qop_begin_mask(prop, negated = FALSE)
)

#' Negated begin-mask query operation.
#'
#' Builds a `[ ! <property>` query fragment that opens a negated masked
#' subquery against `property`. Close the mask with [EndMask()].
#'
#' @param property Property name (character scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' BeginNegatedMask("type")
#' @seealso [BeginMask()], [EndMask()]
#' @export
BeginNegatedMask <- .make_string_op(
    "BeginNegatedMask",
    function(prop) .qop_begin_mask(prop, negated = TRUE)
)

#' End-mask query operation.
#'
#' Builds a `]` query fragment that closes a masked subquery opened
#' with [BeginMask()] or [BeginNegatedMask()].
#'
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' EndMask()
#' Axis("cell") |> BeginMask("type") |> EndMask()
#' @seealso [BeginMask()], [BeginNegatedMask()]
#' @export
EndMask <- .make_nullary("EndMask", .qop_end_mask)

#' Names query operation.
#'
#' Builds a `?` query fragment, listing the entries of the prior axis.
#'
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' Names()
#' Axis("cell") |> Names()
#' @seealso [Axis()]
#' @export
Names <- .make_nullary("Names", .qop_names)

#' If-missing query operation.
#'
#' Builds a `|| <default>` query fragment, providing a default value
#' for entries missing from the prior lookup.
#'
#' @param default Default value (character or numeric scalar), or a
#'   piped [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' IfMissing("N/A")
#' IfMissing(0)
#' Axis("cell") |> LookupVector("age") |> IfMissing(0)
#' @seealso [IfNot()]
#' @export
IfMissing <- .make_value_op(
    "IfMissing", .qop_if_missing,
    param_name = "default"
)

#' Square-column-is query operation.
#'
#' Builds a `@| <value>` query fragment, selecting the row of a square
#' matrix whose column key equals `value`.
#'
#' @param value Column key (character or numeric scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' SquareColumnIs("M1")
#' @seealso [SquareRowIs()]
#' @export
SquareColumnIs <- .make_value_op("SquareColumnIs", .qop_square_column_is)

#' Square-row-is query operation.
#'
#' Builds a `@- <value>` query fragment, selecting the column of a
#' square matrix whose row key equals `value`.
#'
#' @param value Row key (character or numeric scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' SquareRowIs("M1")
#' @seealso [SquareColumnIs()]
#' @export
SquareRowIs <- .make_value_op("SquareRowIs", .qop_square_row_is)

#' As-axis query operation.
#'
#' Builds a `=@` (optionally `=@ <axis_name>`) query fragment, treating
#' the prior vector's values as entries of an axis.
#'
#' @param axis_name Optional axis name (character scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' AsAxis()
#' AsAxis("cell")
#' Axis("cell") |> LookupVector("donor") |> AsAxis()
#' @seealso [Axis()]
#' @export
AsAxis <- .make_optional_string_op(
    "AsAxis", .qop_as_axis,
    param_name = "axis_name"
)

#' If-not query operation.
#'
#' Builds a `??` (optionally `?? <value>`) query fragment, providing a
#' fallback when a prior boolean query yields `FALSE`.
#'
#' @param value Optional fallback value (character scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' IfNot()
#' IfNot("fallback")
#' @seealso [IfMissing()]
#' @export
IfNot <- .make_optional_string_op("IfNot", .qop_if_not)

#' Lookup-scalar query operation.
#'
#' Builds a `.` (optionally `. <name>`) query fragment, looking up the
#' named scalar property.
#'
#' @param name Optional scalar name (character scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' LookupScalar()
#' LookupScalar("version")
#' @seealso [LookupVector()], [LookupMatrix()]
#' @export
LookupScalar <- .make_optional_string_op(
    "LookupScalar", .qop_lookup_scalar,
    param_name = "name"
)

#' Lookup-vector query operation.
#'
#' Builds a `:` (optionally `: <name>`) query fragment, looking up the
#' named vector property of the prior axis.
#'
#' @param name Optional vector name (character scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' LookupVector()
#' LookupVector("age")
#' Axis("cell") |> LookupVector("age")
#' @seealso [LookupScalar()], [LookupMatrix()]
#' @export
LookupVector <- .make_optional_string_op(
    "LookupVector", .qop_lookup_vector,
    param_name = "name"
)

#' Lookup-matrix query operation.
#'
#' Builds a `::` (optionally `:: <name>`) query fragment, looking up
#' the named matrix property indexed by the prior two axes.
#'
#' @param name Optional matrix name (character scalar), or a piped
#'   [DafrQuery].
#' @param ... Optional piped [DafrQuery].
#' @return A [DafrQuery].
#' @examples
#' LookupMatrix()
#' LookupMatrix("UMIs")
#' Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs")
#' @seealso [LookupScalar()], [LookupVector()]
#' @export
LookupMatrix <- .make_optional_string_op(
    "LookupMatrix", .qop_lookup_matrix,
    param_name = "name"
)
