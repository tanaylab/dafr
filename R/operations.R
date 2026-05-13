#' @include utils.R
NULL

.ops_env <- new.env(parent = emptyenv())
.ops_env$reductions <- list()
.ops_env$eltwise <- list()
# Track the source location (file:line) where each op was first
# registered, so conflict errors mirror Julia's
# `conflicting registrations for the <kind> operation: <name>\n
#  first in: <file>:<line>\nsecond in: <file>:<line>` template.
.ops_env$sources_reductions <- list()
.ops_env$sources_eltwise    <- list()

# Best-effort source-location capture for the calling frame. Returns
# `file:line` if the call has a srcref, otherwise `"<unknown>:0"`.
.caller_source <- function(n = 2L) {
    sref <- tryCatch(attr(sys.call(-n), "srcref"), error = function(e) NULL)
    if (is.null(sref)) sref <- tryCatch(
        attr(sys.call(-(n + 1L)), "srcref"), error = function(e) NULL)
    if (is.null(sref)) return("<unknown>:0")
    file <- attr(sref, "srcfile")$filename %||% "<unknown>"
    line <- sref[[1L]]
    sprintf("%s:%d", file, line)
}

#' Register a reduction operation.
#'
#' Stores a user-defined reduction function under the given name so that it
#' can be retrieved by [get_reduction()] and invoked during query evaluation.
#'
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector or
#'   matrix column and `...` collects named parameters.
#' @param overwrite Logical scalar; set to `TRUE` to replace an already-
#'   registered operation.
#' @param source Optional `"file:line"` string identifying where the
#'   registration happened. Used in the conflict error message;
#'   auto-captured from the caller's srcref if omitted.
#' @return Invisibly `NULL`.
#' @examples
#' register_reduction("Median_example", function(x, ...) median(x, ...), overwrite = TRUE)
#' registered_reductions()
#' @export
register_reduction <- function(name, fn, overwrite = FALSE, source = NULL) {
    .assert_name(name, "reduction name")
    if (!is.function(fn)) {
        stop(sprintf("`fn` must be a function (for reduction %s)", sQuote(name)),
            call. = FALSE
        )
    }
    if (is.null(source)) source <- .caller_source()
    if (!isTRUE(overwrite) && !is.null(.ops_env$reductions[[name]])) {
        stop(sprintf(
            "conflicting registrations for the reduction operation: %s\nfirst in: %s\nsecond in: %s",
            name, .ops_env$sources_reductions[[name]] %||% "<unknown>:0",
            source
        ), call. = FALSE)
    }
    .ops_env$reductions[[name]] <- fn
    .ops_env$sources_reductions[[name]] <- source
    invisible(NULL)
}

#' Register an eltwise operation.
#'
#' Stores a user-defined element-wise function under the given name so that it
#' can be retrieved by [get_eltwise()] and invoked during query evaluation.
#'
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector or
#'   matrix (eltwise ops preserve shape) and `...` collects named parameters.
#' @param overwrite Logical scalar; set to `TRUE` to replace an already-
#'   registered operation.
#' @param source Optional `"file:line"` string identifying where the
#'   registration happened. Used in the conflict error message;
#'   auto-captured from the caller's srcref if omitted.
#' @return Invisibly `NULL`.
#' @examples
#' register_eltwise("Clamp01", function(x, ...) pmin(pmax(x, 0), 1),
#'                  overwrite = TRUE)
#' registered_eltwise()
#' @export
register_eltwise <- function(name, fn, overwrite = FALSE, source = NULL) {
    .assert_name(name, "eltwise name")
    if (!is.function(fn)) {
        stop(sprintf("`fn` must be a function (for eltwise %s)", sQuote(name)),
            call. = FALSE
        )
    }
    if (is.null(source)) source <- .caller_source()
    if (!isTRUE(overwrite) && !is.null(.ops_env$eltwise[[name]])) {
        stop(sprintf(
            "conflicting registrations for the eltwise operation: %s\nfirst in: %s\nsecond in: %s",
            name, .ops_env$sources_eltwise[[name]] %||% "<unknown>:0",
            source
        ), call. = FALSE)
    }
    .ops_env$eltwise[[name]] <- fn
    .ops_env$sources_eltwise[[name]] <- source
    invisible(NULL)
}

#' Register a query operation (eltwise or reduction).
#'
#' Single entry point for adding custom eltwise / reduction operations
#' to the dafr query DSL at runtime. Mirrors Julia's
#' `register_query_operation()` from `DataAxesFormats.Registry`.
#'
#' @param kind Either `"eltwise"` or `"reduction"`.
#' @param name Op name; must match `[A-Z][A-Za-z0-9_]*` to be a valid
#'   query token.
#' @param fn Function `function(x, ...)` where `x` is a numeric value
#'   (scalar / vector / matrix) and `...` collects named parameters.
#' @param overwrite Logical; replace an already-registered op of the
#'   same kind and name.
#' @param source Optional `"file:line"` string identifying where the
#'   registration happened. Used in the conflict error message;
#'   auto-captured from the caller's srcref if omitted.
#' @return Invisibly `NULL`.
#' @examples
#' register_query_operation("eltwise", "Clamp01",
#'   function(x, ...) pmin(pmax(x, 0), 1),
#'   overwrite = TRUE)
#' @export
register_query_operation <- function(kind, name, fn,
                                     overwrite = FALSE,
                                     source = NULL) {
    if (!is.character(kind) || length(kind) != 1L ||
        !(kind %in% c("eltwise", "reduction"))) {
        stop("kind must be either \"eltwise\" or \"reduction\"",
             call. = FALSE)
    }
    if (is.null(source)) source <- .caller_source()
    if (kind == "eltwise") {
        register_eltwise(name, fn, overwrite = overwrite, source = source)
    } else {
        register_reduction(name, fn, overwrite = overwrite, source = source)
    }
}

#' Retrieve a registered reduction operation by name.
#'
#' Looks up a reduction operation previously stored via
#' [register_reduction()]. Raises if the name is not registered.
#'
#' @param name Op name (character scalar).
#' @return The stored function.
#' @examples
#' registered_reductions()   # "Sum" is among the built-ins
#' fn <- get_reduction("Sum")
#' fn(c(1, 2, 3))
#' @export
get_reduction <- function(name) {
    fn <- .ops_env$reductions[[name]]
    if (is.null(fn)) {
        stop(sprintf("unknown reduction operation: %s", sQuote(name)), call. = FALSE)
    }
    fn
}

#' Retrieve a registered eltwise operation by name.
#'
#' Looks up an eltwise operation previously stored via
#' [register_eltwise()]. Raises if the name is not registered.
#'
#' @param name Op name (character scalar).
#' @return The stored function.
#' @examples
#' registered_eltwise()      # "Abs" is among the built-ins
#' fn <- get_eltwise("Abs")
#' fn(c(-1, 2, -3))
#' @export
get_eltwise <- function(name) {
    fn <- .ops_env$eltwise[[name]]
    if (is.null(fn)) {
        stop(sprintf("unknown eltwise operation: %s", sQuote(name)), call. = FALSE)
    }
    fn
}

#' List registered reduction operations.
#'
#' Returns the names of all currently registered reduction operations.
#'
#' @return Sorted character vector of registered reduction names.
#' @examples
#' head(registered_reductions())
#' register_reduction("Median_example", function(x, ...) median(x), overwrite = TRUE)
#' "Median_example" %in% registered_reductions()
#' @export
registered_reductions <- function() sort(names(.ops_env$reductions))

#' List registered eltwise operations.
#'
#' Returns the names of all currently registered eltwise operations.
#'
#' @return Sorted character vector of registered eltwise names.
#' @examples
#' head(registered_eltwise())
#' register_eltwise("Negate_example", function(x, ...) -x, overwrite = TRUE)
#' "Negate_example" %in% registered_eltwise()
#' @export
registered_eltwise <- function() sort(names(.ops_env$eltwise))

# Internal: valid named parameters accepted by an operation's query-DSL form.
# Derived from the function's formal arguments minus the input slot `x` and
# the variadic `...`. The `type` pseudo-parameter is universally accepted —
# it controls output-type conversion at the query layer (see Julia
# DataAxesFormats.jl operations.jl: every reduction/eltwise treats `type`
# the same way).
.op_valid_params <- function(fn) {
    formal_names <- names(formals(fn))
    params <- setdiff(formal_names, c("x", "..."))
    union(params, "type")
}

.op_sum <- function(x, ..., na_rm = FALSE) sum(x, na.rm = na_rm)
.op_mean <- function(x, ..., na_rm = FALSE) mean(x, na.rm = na_rm)
.op_max <- function(x, ..., na_rm = FALSE) max(x, na.rm = na_rm)
.op_min <- function(x, ..., na_rm = FALSE) min(x, na.rm = na_rm)
.op_count <- function(x, ...) length(x)

.op_log <- function(x, ..., eps = 0, base = exp(1), type = NULL) {
    .reject_non_float_type("Log", type)
    .require_numeric_param("Log", "eps", eps)
    .require_numeric_param("Log", "base", base)
    eps <- as.numeric(eps); base <- as.numeric(base)
    if (base <= 0) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: positive\nfor the parameter: base\nfor the operation: Log",
            format(base)
        ), call. = FALSE)
    }
    if (eps < 0) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: eps\nfor the operation: Log",
            format(eps)
        ), call. = FALSE)
    }
    # CO6: reject non-positive `x + eps`; Julia's Log signals
    # `value must be: positive`. Sparse Matrices keep zeros implicit, so
    # only inspect explicit non-zero cells.
    .x <- if (methods::is(x, "Matrix")) x@x else as.numeric(x)
    if (length(.x) > 0L && any(.x + eps <= 0, na.rm = TRUE)) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: positive\nfor the operation: Log",
            format(min(.x))
        ), call. = FALSE)
    }
    .cast_to_type(log(x + eps, base = base), type)
}
.op_abs <- function(x, ..., type = NULL) {
    .reject_non_number_type("Abs", type)
    .cast_to_type(abs(x), type)
}
.op_exp <- function(x, ..., type = NULL) {
    .reject_non_number_type("Exp", type)
    .cast_to_type(exp(x), type)
}
.op_sqrt <- function(x, ..., type = NULL) {
    .reject_non_number_type("Sqrt", type)
    .cast_to_type(sqrt(x), type)
}
.op_round <- function(x, ..., digits = 0, type = NULL) {
    .reject_non_number_type("Round", type)
    .cast_to_type(round(x, digits = digits), type)
}

.op_clamp <- function(x, ..., min = NULL, max = NULL, type = NULL) {
    .reject_non_number_type("Clamp", type)
    .require_numeric_param("Clamp", "min", min)
    .require_numeric_param("Clamp", "max", max)
    min <- if (is.null(min)) -Inf else as.numeric(min)
    max <- if (is.null(max)) Inf else as.numeric(max)
    if (min >= max) {
        stop(sprintf("Clamp: min (%g) must be strictly less than max (%g)", min, max),
            call. = FALSE
        )
    }
    if (methods::is(x, "dgCMatrix")) {
        if (min <= 0 && 0 <= max) {
            out <- x
            out@x <- pmin(pmax(out@x, min), max)
            return(.cast_to_type(out, type))
        }
        x <- as.matrix(x)
    }
    .cast_to_type(pmin(pmax(x, min), max), type)
}

.op_convert <- function(x, ..., type) {
    if (missing(type)) {
        stop("Convert: 'type' parameter is required (one of 'double', 'integer', 'logical', 'integer64'; Julia aliases 'Float32'/'Float64'/'Int32'/'Int64'/'Bool' also accepted)",
            call. = FALSE
        )
    }
    # Normalize Julia type names to R-native canonical form.
    julia_aliases <- c(
        Float32 = "double",
        Float64 = "double",
        Int32 = "integer",
        Int64 = "integer64",
        Bool = "logical"
    )
    if (is.character(type) && length(type) == 1L && type %in% names(julia_aliases)) {
        type <- unname(julia_aliases[type])
    }
    valid_types <- c("double", "integer", "logical", "integer64")
    if (!is.character(type) || length(type) != 1L || !type %in% valid_types) {
        stop(sprintf(
            "Convert: 'type' must be one of 'double', 'integer', 'logical', 'integer64' (or Julia aliases 'Float32'/'Float64'/'Int32'/'Int64'/'Bool'); got %s",
            sQuote(as.character(type)[1L])
        ), call. = FALSE)
    }
    # integer64 path: densify sparse input (no sparse integer64 class exists).
    # Preserve dim/dimnames — bit64::as.integer64 strips them on matrix input.
    if (type == "integer64") {
        if (methods::is(x, "dgCMatrix")) {
            x <- as.matrix(x)
        }
        out <- bit64::as.integer64(x)
        if (!is.null(dim(x))) {
            dim(out) <- dim(x)
            dimnames(out) <- dimnames(x)
        }
        return(out)
    }
    # Sparse preservation for dgCMatrix
    if (methods::is(x, "dgCMatrix")) {
        if (type == "double") return(x)
        if (type == "integer") {
            if (length(x@x) > 0L && any(x@x != floor(x@x))) {
                stop("Convert: non-integer value in integer coercion",
                    call. = FALSE)
            }
            x@x <- as.double(as.integer(x@x))
            return(x)
        }
        if (type == "logical") {
            x@x <- as.double(x@x != 0)
            return(Matrix::drop0(x))
        }
    }
    # Dense or vector: existing behaviour via storage.mode
    storage.mode(x) <- type
    x
}

## ---- Param-validation helpers (Julia parity) ------------------------------
#
# These mirror the Julia `parse_*_type_value` / `parse_number_value`
# helpers in DataAxesFormats.jl/src/operations.jl. Error wording follows
# the Julia template:
#   invalid value: "<val>"
#   value must be: <constraint>
#   for the parameter: <name>
#   for the operation: <op>

.NUMBER_TYPES <- c(
    "Bool", "String",
    "Int8", "Int16", "Int32", "Int64",
    "UInt8", "UInt16", "UInt32", "UInt64",
    "Float32", "Float64",
    "double", "integer", "logical", "integer64"
)
.NUMBER_TYPES_FILTERED_NUMERIC <- setdiff(.NUMBER_TYPES, c("String"))

# CO5 parity: honor `type =` on numeric eltwise ops by coercing the
# result. Matches Julia DAF where `% Op type Float32` returns a
# Float32-storage result.
.cast_to_type <- function(value, type) {
    if (is.null(type) || is.null(value)) return(value)
    if (methods::is(value, "Matrix")) return(value)  # leave Matrix alone
    target <- switch(as.character(type)[[1L]],
        integer = , Int8 = , Int16 = , Int32 = , Int64 = ,
        UInt8 = , UInt16 = , UInt32 = , UInt64 = "integer",
        numeric = , double = , Float32 = , Float64 = "double",
        logical = , Bool = "logical",
        character = , String = "character",
        NULL
    )
    if (is.null(target)) return(value)
    if (is.matrix(value)) {
        dn <- dimnames(value)
        storage.mode(value) <- target
        dimnames(value) <- dn
        return(value)
    }
    nm <- names(value)
    out <- as.vector(value, mode = target)
    names(out) <- nm
    out
}

.reject_non_float_type <- function(op_name, type) {
    if (is.null(type)) return(invisible())
    type <- as.character(type)[[1L]]
    float_types <- c("Float32", "Float64", "double")
    if (!type %in% float_types) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: a float type\nfor the parameter: type\nfor the operation: %s",
            type, op_name
        ), call. = FALSE)
    }
    invisible()
}

.reject_non_number_type <- function(op_name, type) {
    if (is.null(type)) return(invisible())
    type <- as.character(type)[[1L]]
    if (!type %in% .NUMBER_TYPES_FILTERED_NUMERIC) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: a number type\nfor the parameter: type\nfor the operation: %s",
            type, op_name
        ), call. = FALSE)
    }
    invisible()
}

.require_numeric_param <- function(op_name, param_name, value) {
    if (is.null(value)) return(invisible())
    if (is.numeric(value) && length(value) == 1L) return(invisible())
    n <- suppressWarnings(as.numeric(value))
    if (length(value) == 1L && !is.na(n)) return(invisible())
    stop(sprintf(
        "invalid value: \"%s\"\nvalue must be: a number\nfor the parameter: %s\nfor the operation: %s",
        as.character(value)[[1L]], param_name, op_name
    ), call. = FALSE)
}

.op_fraction <- function(x, ..., type = NULL) {
    # Julia parity: only float types are accepted via the `type`
    # parameter (DataAxesFormats.jl/src/operations.jl Fraction
    # constructor uses parse_float_type_value).
    if (!is.null(type)) {
        type <- as.character(type)[[1L]]
        float_types <- c("Float32", "Float64", "double")
        if (!type %in% float_types) {
            stop(sprintf(
                "invalid value: \"%s\"\nvalue must be: a float type\nfor the parameter: type\nfor the operation: Fraction",
                type
            ), call. = FALSE)
        }
    }
    if (is.null(dim(x)) && length(x) == 1L) {
        stop("applying Fraction eltwise operation to a scalar",
            call. = FALSE)
    }
    if (methods::is(x, "dgCMatrix")) {
        out <- x
        col_sums <- Matrix::colSums(out)
        for (j in seq_along(col_sums)) {
            start <- out@p[j] + 1L
            end <- out@p[j + 1L]
            if (start <= end) {
                if (col_sums[j] != 0) {
                    out@x[start:end] <- out@x[start:end] / col_sums[j]
                } else {
                    out@x[start:end] <- 0
                }
            }
        }
        return(out)
    }
    if (is.matrix(x)) {
        col_sums <- colSums(x)
        out <- x
        storage.mode(out) <- "double"
        for (j in seq_len(ncol(out))) {
            out[, j] <- if (col_sums[j] == 0) 0 else out[, j] / col_sums[j]
        }
        return(out)
    }
    total <- sum(x)
    if (total == 0) return(rep(0, length(x)))
    x / total
}

.significant_vec <- function(v, high, low) {
    if (max(abs(v)) < high) {
        return(rep(0, length(v)))
    }
    v[abs(v) < low] <- 0
    v
}

.op_significant <- function(x, ..., high, low = high) {
    if (missing(high)) {
        stop("Significant: 'high' parameter is required", call. = FALSE)
    }
    # Julia parity (DataAxesFormats.jl operations.jl Significant):
    # parameter validation messages use the
    #   invalid value: "<val>"
    #   value must be: <constraint>
    #   for the parameter: <name>
    #   for the operation: Significant
    # template.
    if (!is.numeric(high) || length(high) != 1L || high <= 0) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: positive\nfor the parameter: high\nfor the operation: Significant",
            as.character(high)[1L]
        ), call. = FALSE)
    }
    if (!is.numeric(low) || length(low) != 1L) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: low\nfor the operation: Significant",
            as.character(low)[1L]
        ), call. = FALSE)
    }
    if (low < 0) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: low\nfor the operation: Significant",
            as.character(low)[1L]
        ), call. = FALSE)
    }
    if (low > high) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: at most high (%s)\nfor the parameter: low\nfor the operation: Significant",
            as.character(low)[1L], format(as.numeric(high))
        ), call. = FALSE)
    }
    if (is.null(dim(x)) && length(x) == 1L) {
        stop("applying Significant eltwise operation to a scalar",
            call. = FALSE)
    }
    if (methods::is(x, "dgCMatrix")) {
        out <- x
        for (j in seq_len(ncol(out))) {
            start <- out@p[j] + 1L
            end <- out@p[j + 1L]
            if (start <= end) {
                out@x[start:end] <- .significant_vec(out@x[start:end], high, low)
            }
        }
        return(Matrix::drop0(out))
    }
    if (is.matrix(x)) {
        out <- x
        storage.mode(out) <- "double"
        for (j in seq_len(ncol(out))) {
            out[, j] <- .significant_vec(out[, j], high, low)
        }
        return(out)
    }
    .significant_vec(x, high, low)
}

.var_uncorrected <- function(x, na_rm) {
    if (na_rm) x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0L) return(NA_real_)
    if (anyNA(x)) return(NA_real_)
    mu <- sum(x) / n
    sum((x - mu)^2) / n
}

.op_var <- function(x, ..., na_rm = FALSE) {
    .var_uncorrected(as.numeric(x), isTRUE(na_rm))
}

.op_std <- function(x, ..., na_rm = FALSE) {
    v <- .var_uncorrected(as.numeric(x), isTRUE(na_rm))
    if (is.na(v)) v else sqrt(v)
}

.assert_non_negative_eps <- function(eps, op_name = NULL) {
    if (!is.numeric(eps) || length(eps) != 1L || is.na(eps) || eps < 0) {
        if (!is.null(op_name)) {
            # Julia parity error template.
            stop(sprintf(
                "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: eps\nfor the operation: %s",
                as.character(eps)[1L], op_name
            ), call. = FALSE)
        }
        stop(sprintf("'eps' must be a non-negative number (got %s)",
            as.character(eps)[1L]
        ), call. = FALSE)
    }
}

.op_varn <- function(x, ..., na_rm = FALSE, eps = 0) {
    .assert_non_negative_eps(eps)
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    v <- .var_uncorrected(x, na_rm = FALSE)
    mu <- if (length(x) == 0L) NA_real_ else sum(x) / length(x)
    v / (mu + eps)
}

.op_stdn <- function(x, ..., na_rm = FALSE, eps = 0) {
    .assert_non_negative_eps(eps)
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    v <- .var_uncorrected(x, na_rm = FALSE)
    mu <- if (length(x) == 0L) NA_real_ else sum(x) / length(x)
    sqrt(v) / (mu + eps)
}

.op_median <- function(x, ..., na_rm = FALSE) {
    stats::median(as.numeric(x), na.rm = isTRUE(na_rm))
}

.op_quantile <- function(x, ..., p, na_rm = FALSE) {
    if (missing(p)) {
        stop("Quantile: 'p' parameter is required (a value in [0, 1])",
            call. = FALSE
        )
    }
    if (!is.numeric(p) || length(p) != 1L || is.na(p) || p < 0 || p > 1) {
        stop(sprintf("Quantile: 'p' must be in [0, 1] (got %s)",
            as.character(p)[1L]
        ), call. = FALSE)
    }
    unname(stats::quantile(as.numeric(x), probs = p, na.rm = isTRUE(na_rm)))
}

.op_geomean <- function(x, ..., eps = 0, na_rm = FALSE, type = NULL) {
    .reject_non_float_type("GeoMean", type)
    .assert_non_negative_eps(eps, "GeoMean")
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    if (eps == 0) {
        exp(mean(log(x)))
    } else {
        exp(mean(log(x + eps))) - eps
    }
}

.op_mode <- function(x, ...) {
    # Julia's Mode supports strings explicitly (operations.jl:1058-1066,
    # `supports_strings(::Mode) = true`). Mirror that, normalizing factor
    # to character at the boundary as Julia does for CategoricalVector
    # (anndata_format.jl:403).
    if (is.factor(x)) x <- as.character(x)
    if (!is.numeric(x) && !is.logical(x) && !is.character(x)) {
        stop("Mode: only numeric, logical, and character input are supported; got ",
            sQuote(typeof(x)), call. = FALSE
        )
    }
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

attr(.op_sum, ".dafr_builtin") <- "Sum"
attr(.op_mean, ".dafr_builtin") <- "Mean"
attr(.op_max, ".dafr_builtin") <- "Max"
attr(.op_min, ".dafr_builtin") <- "Min"
attr(.op_count, ".dafr_builtin") <- "Count"
attr(.op_log, ".dafr_builtin") <- "Log"
attr(.op_abs, ".dafr_builtin") <- "Abs"
attr(.op_exp, ".dafr_builtin") <- "Exp"
attr(.op_sqrt, ".dafr_builtin") <- "Sqrt"
attr(.op_round, ".dafr_builtin") <- "Round"
attr(.op_clamp, ".dafr_builtin") <- "Clamp"
attr(.op_convert, ".dafr_builtin") <- "Convert"
attr(.op_fraction, ".dafr_builtin") <- "Fraction"
attr(.op_significant, ".dafr_builtin") <- "Significant"
attr(.op_var, ".dafr_builtin") <- "Var"
attr(.op_std, ".dafr_builtin") <- "Std"
attr(.op_varn, ".dafr_builtin") <- "VarN"
attr(.op_stdn, ".dafr_builtin") <- "StdN"
attr(.op_median, ".dafr_builtin") <- "Median"
attr(.op_quantile, ".dafr_builtin") <- "Quantile"
attr(.op_geomean, ".dafr_builtin") <- "GeoMean"
attr(.op_mode, ".dafr_builtin") <- "Mode"

.register_default_ops <- function() {
    register_reduction("Sum", .op_sum, overwrite = TRUE)
    register_reduction("Mean", .op_mean, overwrite = TRUE)
    register_reduction("Max", .op_max, overwrite = TRUE)
    register_reduction("Min", .op_min, overwrite = TRUE)
    register_reduction("Count", .op_count, overwrite = TRUE)
    register_reduction("Var", .op_var, overwrite = TRUE)
    register_reduction("Std", .op_std, overwrite = TRUE)
    register_reduction("VarN", .op_varn, overwrite = TRUE)
    register_reduction("StdN", .op_stdn, overwrite = TRUE)
    register_reduction("Median", .op_median, overwrite = TRUE)
    register_reduction("Quantile", .op_quantile, overwrite = TRUE)
    register_reduction("GeoMean", .op_geomean, overwrite = TRUE)
    register_reduction("Mode", .op_mode, overwrite = TRUE)

    register_eltwise("Log", .op_log, overwrite = TRUE)
    register_eltwise("Abs", .op_abs, overwrite = TRUE)
    register_eltwise("Exp", .op_exp, overwrite = TRUE)
    register_eltwise("Sqrt", .op_sqrt, overwrite = TRUE)
    register_eltwise("Round", .op_round, overwrite = TRUE)
    register_eltwise("Clamp", .op_clamp, overwrite = TRUE)
    register_eltwise("Convert", .op_convert, overwrite = TRUE)
    register_eltwise("Fraction", .op_fraction, overwrite = TRUE)
    register_eltwise("Significant", .op_significant, overwrite = TRUE)

    invisible(NULL)
}
