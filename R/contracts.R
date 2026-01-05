# contracts.R - DAF Contract Support for dafr
#
# Wraps the DataAxesFormats.Contracts module from Julia to provide
# contract-based data validation in R.

#' Contract Expectation Types
#'
#' These constants correspond to Julia's ContractExpectation enum
#' @export
RequiredInput <- "RequiredInput"

#' @rdname RequiredInput
#' @export
OptionalInput <- "OptionalInput"

#' @rdname RequiredInput
#' @export
GuaranteedOutput <- "GuaranteedOutput"

#' @rdname RequiredInput
#' @export
OptionalOutput <- "OptionalOutput"

#' Create a Contract specification
#'
#' Creates a contract specification that can be used to validate DAF data.
#' This corresponds to Julia's DataAxesFormats.Contracts.Contract.
#'
#' @param axes List of axis specifications (see axis_contract)
#' @param data List of data specifications (see vector_contract, matrix_contract, scalar_contract)
#' @param is_relaxed If TRUE, allows additional inputs/outputs not in contract
#'
#' @return A contract object that can be used with verify_contract
#' @export
#'
#' @examples
#' \dontrun{
#' contract <- create_contract(
#'     axes = list(
#'         axis_contract("metacell", RequiredInput, "Metacell identifiers"),
#'         axis_contract("gene", RequiredInput, "Gene identifiers")
#'     ),
#'     data = list(
#'         vector_contract(
#'             "metacell", "type", RequiredInput, "character",
#'             "Cell type per metacell"
#'         ),
#'         matrix_contract(
#'             "metacell", "gene", "UMIs", RequiredInput, "numeric",
#'             "UMI counts matrix"
#'         )
#'     )
#' )
#' }
create_contract <- function(axes = list(), data = list(), is_relaxed = FALSE) {
    contract <- list(
        axes = axes,
        data = data,
        is_relaxed = is_relaxed
    )
    class(contract) <- c("DafContract", "list")
    return(contract)
}

#' Create an axis contract specification
#'
#' @param name Axis name
#' @param expectation One of RequiredInput, OptionalInput, GuaranteedOutput, OptionalOutput
#' @param description Human-readable description
#'
#' @return Axis specification list
#' @export
axis_contract <- function(name, expectation, description) {
    list(
        type = "axis",
        name = name,
        expectation = expectation,
        description = description
    )
}

#' Create a scalar contract specification
#'
#' @param name Scalar name
#' @param expectation One of RequiredInput, OptionalInput, GuaranteedOutput, OptionalOutput
#' @param dtype Data type (e.g., "character", "numeric", "integer", "logical")
#' @param description Human-readable description
#'
#' @return Scalar specification list
#' @export
scalar_contract <- function(name, expectation, dtype, description) {
    list(
        type = "scalar",
        name = name,
        expectation = expectation,
        dtype = dtype,
        description = description
    )
}

#' Create a vector contract specification
#'
#' @param axis Axis name the vector belongs to
#' @param name Vector name
#' @param expectation One of RequiredInput, OptionalInput, GuaranteedOutput, OptionalOutput
#' @param dtype Data type (e.g., "character", "numeric", "integer", "logical")
#' @param description Human-readable description
#'
#' @return Vector specification list
#' @export
vector_contract <- function(axis, name, expectation, dtype, description) {
    list(
        type = "vector",
        axis = axis,
        name = name,
        expectation = expectation,
        dtype = dtype,
        description = description
    )
}

#' Create a matrix contract specification
#'
#' @param rows_axis Rows axis name
#' @param cols_axis Columns axis name
#' @param name Matrix name
#' @param expectation One of RequiredInput, OptionalInput, GuaranteedOutput, OptionalOutput
#' @param dtype Data type (e.g., "numeric", "integer")
#' @param description Human-readable description
#'
#' @return Matrix specification list
#' @export
matrix_contract <- function(rows_axis, cols_axis, name, expectation, dtype, description) {
    list(
        type = "matrix",
        rows_axis = rows_axis,
        cols_axis = cols_axis,
        name = name,
        expectation = expectation,
        dtype = dtype,
        description = description
    )
}

#' Create a tensor contract specification
#'
#' A tensor is a 3D data structure stored as per-entry matrices along a main axis.
#' For example, if the main axis is "batch", then for each batch entry there would
#' be a matrix named "batchN_name" for some name.
#'
#' @param main_axis Main axis name (the axis along which matrices are stored)
#' @param rows_axis Rows axis name for each matrix
#' @param cols_axis Columns axis name for each matrix
#' @param name Tensor name (individual matrices will be named "entry_name")
#' @param expectation One of RequiredInput, OptionalInput, GuaranteedOutput, OptionalOutput
#' @param dtype Data type (e.g., "numeric", "integer")
#' @param description Human-readable description
#'
#' @return Tensor specification list
#' @export
tensor_contract <- function(main_axis, rows_axis, cols_axis, name, expectation, dtype, description) {
    list(
        type = "tensor",
        main_axis = main_axis,
        rows_axis = rows_axis,
        cols_axis = cols_axis,
        name = name,
        expectation = expectation,
        dtype = dtype,
        description = description
    )
}

#' Create a contract-aware wrapper for a Daf
#'
#' Wraps a Daf data set with a contract for a specific computation.
#' The returned object can be used with verify_input and verify_output
#' to validate the data before and after computation.
#'
#' @param computation Name of the computation (used for error messages)
#' @param contract Contract created with create_contract
#' @param daf Daf object to wrap
#' @param overwrite If TRUE, allows overwriting existing output data
#'
#' @return A list with class "ContractDaf" containing:
#'   - computation: The computation name
#'   - contract: The contract specification
#'   - daf: The wrapped Daf object
#'   - overwrite: The overwrite flag
#'
#' @details This function provides a simplified R interface similar to Julia's
#'   contractor function. Use verify_input() before running your computation
#'   and verify_output() after to validate the data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' contract <- create_contract(
#'     axes = list(axis_contract("gene", RequiredInput, "Genes")),
#'     data = list(vector_contract("gene", "score", GuaranteedOutput, "numeric", "Scores"))
#' )
#' contract_daf <- contractor("my.computation", contract, daf)
#' verify_input(contract_daf$daf, contract_daf$contract)
#' # ... run computation ...
#' verify_output(contract_daf$daf, contract_daf$contract)
#' }
contractor <- function(computation, contract, daf, overwrite = FALSE) {
    if (!inherits(contract, "DafContract")) {
        stop("contract must be a DafContract object created with create_contract()")
    }
    if (!inherits(daf, "Daf")) {
        stop("daf must be a Daf object")
    }

    result <- list(
        computation = computation,
        contract = contract,
        daf = daf,
        overwrite = overwrite
    )
    class(result) <- c("ContractDaf", "list")
    return(result)
}

#' Verify a DAF object against a contract
#'
#' Validates that a DAF object meets the contract requirements.
#'
#' @param daf_obj DAF object to validate
#' @param contract Contract created with create_contract
#' @param check_unused If TRUE, warns about required data that wasn't accessed
#'
#' @return List with valid (logical), errors (character vector), warnings (character vector)
#' @export
verify_contract <- function(daf_obj, contract, check_unused = FALSE) {
    if (!inherits(daf_obj, "Daf")) {
        return(list(
            valid = FALSE,
            errors = "Object is not a valid DAF object",
            warnings = character()
        ))
    }

    if (!inherits(contract, "DafContract")) {
        return(list(
            valid = FALSE,
            errors = "Contract is not a valid DafContract object",
            warnings = character()
        ))
    }

    errors <- character()
    warnings <- character()

    # Validate axes
    for (spec in contract$axes) {
        has_it <- has_axis(daf_obj, spec$name)

        if (spec$expectation == RequiredInput && !has_it) {
            errors <- c(
                errors,
                sprintf("Missing required axis: %s", spec$name)
            )
        } else if (spec$expectation == OptionalInput && !has_it) {
            # Optional, no error
        } else if (spec$expectation %in% c(GuaranteedOutput, OptionalOutput)) {
            # Output expectations - check if shouldn't exist
            if (has_it && spec$expectation == GuaranteedOutput) {
                warnings <- c(
                    warnings,
                    sprintf("Pre-existing output axis: %s", spec$name)
                )
            }
        }
    }

    # Validate data (scalars, vectors, matrices)
    for (spec in contract$data) {
        if (spec$type == "scalar") {
            has_it <- has_scalar(daf_obj, spec$name)
            if (spec$expectation == RequiredInput && !has_it) {
                errors <- c(
                    errors,
                    sprintf("Missing required scalar: %s", spec$name)
                )
            }
        } else if (spec$type == "vector") {
            has_it <- has_axis(daf_obj, spec$axis) &&
                has_vector(daf_obj, spec$axis, spec$name)
            if (spec$expectation == RequiredInput && !has_it) {
                errors <- c(
                    errors,
                    sprintf("Missing required vector: %s.%s", spec$axis, spec$name)
                )
            }
        } else if (spec$type == "matrix") {
            has_it <- has_axis(daf_obj, spec$rows_axis) &&
                has_axis(daf_obj, spec$cols_axis) &&
                has_matrix(daf_obj, spec$rows_axis, spec$cols_axis, spec$name)
            if (spec$expectation == RequiredInput && !has_it) {
                errors <- c(
                    errors,
                    sprintf(
                        "Missing required matrix: %s,%s.%s",
                        spec$rows_axis, spec$cols_axis, spec$name
                    )
                )
            }
        }
    }

    return(list(
        valid = length(errors) == 0,
        errors = errors,
        warnings = warnings
    ))
}

#' Verify DAF input against contract
#'
#' Convenience wrapper for verify_contract for input validation.
#'
#' @param daf_obj DAF object to validate
#' @param contract Contract to validate against
#'
#' @return Invisible TRUE if valid, throws error otherwise
#' @export
verify_input <- function(daf_obj, contract) {
    result <- verify_contract(daf_obj, contract)
    if (!result$valid) {
        stop(paste(c("DAF input validation failed:", result$errors), collapse = "\n"))
    }
    invisible(TRUE)
}

#' Verify DAF output against contract
#'
#' Convenience wrapper for verify_contract for output validation.
#'
#' @param daf_obj DAF object to validate
#' @param contract Contract to validate against
#'
#' @return Invisible TRUE if valid, throws error otherwise
#' @export
verify_output <- function(daf_obj, contract) {
    result <- verify_contract(daf_obj, contract, check_unused = TRUE)
    if (!result$valid) {
        stop(paste(c("DAF output validation failed:", result$errors), collapse = "\n"))
    }
    if (length(result$warnings) > 0) {
        warning(paste(result$warnings, collapse = "\n"))
    }
    invisible(TRUE)
}

#' Print method for DafContract
#'
#' @param x A DafContract object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#' @export
print.DafContract <- function(x, ...) {
    cat("DAF Contract\n")
    cat("============\n")

    if (x$is_relaxed) {
        cat("(Relaxed - allows additional data)\n")
    }

    cat("\n")

    # Print axes
    if (length(x$axes) > 0) {
        cat("Axes:\n")
        for (spec in x$axes) {
            cat(sprintf(
                "  %s (%s): %s\n",
                spec$name, spec$expectation, spec$description
            ))
        }
        cat("\n")
    }

    # Group data by type
    scalars <- Filter(function(s) s$type == "scalar", x$data)
    vectors <- Filter(function(s) s$type == "vector", x$data)
    matrices <- Filter(function(s) s$type == "matrix", x$data)
    tensors <- Filter(function(s) s$type == "tensor", x$data)

    if (length(scalars) > 0) {
        cat("Scalars:\n")
        for (spec in scalars) {
            cat(sprintf(
                "  %s (%s, %s): %s\n",
                spec$name, spec$expectation, spec$dtype, spec$description
            ))
        }
        cat("\n")
    }

    if (length(vectors) > 0) {
        cat("Vectors:\n")
        for (spec in vectors) {
            cat(sprintf(
                "  %s.%s (%s, %s): %s\n",
                spec$axis, spec$name, spec$expectation, spec$dtype, spec$description
            ))
        }
        cat("\n")
    }

    if (length(matrices) > 0) {
        cat("Matrices:\n")
        for (spec in matrices) {
            cat(sprintf(
                "  %s,%s.%s (%s, %s): %s\n",
                spec$rows_axis, spec$cols_axis, spec$name,
                spec$expectation, spec$dtype, spec$description
            ))
        }
        cat("\n")
    }

    if (length(tensors) > 0) {
        cat("Tensors:\n")
        for (spec in tensors) {
            cat(sprintf(
                "  %s;%s,%s.%s (%s, %s): %s\n",
                spec$main_axis, spec$rows_axis, spec$cols_axis, spec$name,
                spec$expectation, spec$dtype, spec$description
            ))
        }
    }

    invisible(x)
}

#' Convert contract to documentation string
#'
#' @param contract A DafContract object
#' @param format Output format: "markdown" or "text"
#'
#' @return Character string with documentation
#' @export
contract_docs <- function(contract, format = c("markdown", "text")) {
    format <- match.arg(format)

    lines <- character()

    if (format == "markdown") {
        lines <- c(lines, "# DAF Contract")
        lines <- c(lines, "")

        if (contract$is_relaxed) {
            lines <- c(lines, "*Relaxed contract - allows additional data*")
            lines <- c(lines, "")
        }

        if (length(contract$axes) > 0) {
            lines <- c(lines, "## Axes")
            lines <- c(lines, "")
            lines <- c(lines, "| Name | Required | Description |")
            lines <- c(lines, "|------|----------|-------------|")
            for (spec in contract$axes) {
                req <- if (spec$expectation == RequiredInput) "Yes" else "No"
                lines <- c(lines, sprintf(
                    "| `%s` | %s | %s |",
                    spec$name, req, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        # Group data by type
        scalars <- Filter(function(s) s$type == "scalar", contract$data)
        vectors <- Filter(function(s) s$type == "vector", contract$data)
        matrices <- Filter(function(s) s$type == "matrix", contract$data)
        tensors <- Filter(function(s) s$type == "tensor", contract$data)

        if (length(scalars) > 0) {
            lines <- c(lines, "## Scalars")
            lines <- c(lines, "")
            lines <- c(lines, "| Name | Required | Type | Description |")
            lines <- c(lines, "|------|----------|------|-------------|")
            for (spec in scalars) {
                req <- if (spec$expectation == RequiredInput) "Yes" else "No"
                lines <- c(lines, sprintf(
                    "| `%s` | %s | %s | %s |",
                    spec$name, req, spec$dtype, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        if (length(vectors) > 0) {
            lines <- c(lines, "## Vectors")
            lines <- c(lines, "")
            lines <- c(lines, "| Axis | Name | Required | Type | Description |")
            lines <- c(lines, "|------|------|----------|------|-------------|")
            for (spec in vectors) {
                req <- if (spec$expectation == RequiredInput) "Yes" else "No"
                lines <- c(lines, sprintf(
                    "| `%s` | `%s` | %s | %s | %s |",
                    spec$axis, spec$name, req, spec$dtype, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        if (length(matrices) > 0) {
            lines <- c(lines, "## Matrices")
            lines <- c(lines, "")
            lines <- c(lines, "| Rows | Cols | Name | Required | Type | Description |")
            lines <- c(lines, "|------|------|------|----------|------|-------------|")
            for (spec in matrices) {
                req <- if (spec$expectation == RequiredInput) "Yes" else "No"
                lines <- c(lines, sprintf(
                    "| `%s` | `%s` | `%s` | %s | %s | %s |",
                    spec$rows_axis, spec$cols_axis, spec$name,
                    req, spec$dtype, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        if (length(tensors) > 0) {
            lines <- c(lines, "## Tensors")
            lines <- c(lines, "")
            lines <- c(lines, "| Main | Rows | Cols | Name | Required | Type | Description |")
            lines <- c(lines, "|------|------|------|------|----------|------|-------------|")
            for (spec in tensors) {
                req <- if (spec$expectation == RequiredInput) "Yes" else "No"
                lines <- c(lines, sprintf(
                    "| `%s` | `%s` | `%s` | `%s` | %s | %s | %s |",
                    spec$main_axis, spec$rows_axis, spec$cols_axis, spec$name,
                    req, spec$dtype, spec$description
                ))
            }
        }
    } else {
        # Text format
        lines <- c(lines, "DAF Contract")
        lines <- c(lines, "============")
        lines <- c(lines, "")

        if (contract$is_relaxed) {
            lines <- c(lines, "(Relaxed - allows additional data)")
            lines <- c(lines, "")
        }

        if (length(contract$axes) > 0) {
            lines <- c(lines, "Axes:")
            for (spec in contract$axes) {
                lines <- c(lines, sprintf(
                    "  %s (%s): %s",
                    spec$name, spec$expectation, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        # Similar for scalars, vectors, matrices, tensors...
        scalars <- Filter(function(s) s$type == "scalar", contract$data)
        vectors <- Filter(function(s) s$type == "vector", contract$data)
        matrices <- Filter(function(s) s$type == "matrix", contract$data)
        tensors <- Filter(function(s) s$type == "tensor", contract$data)

        if (length(scalars) > 0) {
            lines <- c(lines, "Scalars:")
            for (spec in scalars) {
                lines <- c(lines, sprintf(
                    "  %s (%s, %s): %s",
                    spec$name, spec$expectation, spec$dtype, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        if (length(vectors) > 0) {
            lines <- c(lines, "Vectors:")
            for (spec in vectors) {
                lines <- c(lines, sprintf(
                    "  %s.%s (%s, %s): %s",
                    spec$axis, spec$name, spec$expectation, spec$dtype, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        if (length(matrices) > 0) {
            lines <- c(lines, "Matrices:")
            for (spec in matrices) {
                lines <- c(lines, sprintf(
                    "  %s,%s.%s (%s, %s): %s",
                    spec$rows_axis, spec$cols_axis, spec$name,
                    spec$expectation, spec$dtype, spec$description
                ))
            }
            lines <- c(lines, "")
        }

        if (length(tensors) > 0) {
            lines <- c(lines, "Tensors:")
            for (spec in tensors) {
                lines <- c(lines, sprintf(
                    "  %s;%s,%s.%s (%s, %s): %s",
                    spec$main_axis, spec$rows_axis, spec$cols_axis, spec$name,
                    spec$expectation, spec$dtype, spec$description
                ))
            }
        }
    }

    paste(lines, collapse = "\n")
}
