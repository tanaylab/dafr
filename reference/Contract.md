# A contract describing a computation's inputs and outputs.

A contract describing a computation's inputs and outputs.

## Usage

``` r
Contract(name = "", is_relaxed = FALSE, axes = list(), data = list())
```

## Arguments

- name:

  Optional name.

- is_relaxed:

  If TRUE, unknown properties don't error.

- axes:

  Named list: axis -\> list(expectation, description).

- data:

  List of contract_scalar()/contract_vector()/contract_matrix() records.

## Examples

``` r
ct <- create_contract(
    scalars = list(contract_scalar("organism", RequiredInput, "character",
                                   "Species name."))
)
inherits(ct, "dafr::Contract")
#> [1] TRUE
```
