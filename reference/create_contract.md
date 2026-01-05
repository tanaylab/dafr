# Create a Contract specification

Creates a contract specification that can be used to validate DAF data.
This corresponds to Julia's DataAxesFormats.Contracts.Contract.

## Usage

``` r
create_contract(axes = list(), data = list(), is_relaxed = FALSE)
```

## Arguments

- axes:

  List of axis specifications (see axis_contract)

- data:

  List of data specifications (see vector_contract, matrix_contract,
  scalar_contract)

- is_relaxed:

  If TRUE, allows additional inputs/outputs not in contract

## Value

A contract object that can be used with verify_contract

## Examples

``` r
if (FALSE) { # \dontrun{
contract <- create_contract(
    axes = list(
        axis_contract("metacell", RequiredInput, "Metacell identifiers"),
        axis_contract("gene", RequiredInput, "Gene identifiers")
    ),
    data = list(
        vector_contract(
            "metacell", "type", RequiredInput, "character",
            "Cell type per metacell"
        ),
        matrix_contract(
            "metacell", "gene", "UMIs", RequiredInput, "numeric",
            "UMI counts matrix"
        )
    )
)
} # }
```
