# Create a contract-aware wrapper for a Daf

Wraps a Daf data set with a contract for a specific computation. The
returned object can be used with verify_input and verify_output to
validate the data before and after computation.

## Usage

``` r
contractor(computation, contract, daf, overwrite = FALSE)
```

## Arguments

- computation:

  Name of the computation (used for error messages)

- contract:

  Contract created with create_contract

- daf:

  Daf object to wrap

- overwrite:

  If TRUE, allows overwriting existing output data

## Value

A list with class "ContractDaf" containing:

- computation: The computation name

- contract: The contract specification

- daf: The wrapped Daf object

- overwrite: The overwrite flag

## Details

This function provides a simplified R interface similar to Julia's
contractor function. Use verify_input() before running your computation
and verify_output() after to validate the data.

## Examples

``` r
if (FALSE) { # \dontrun{
contract <- create_contract(
    axes = list(axis_contract("gene", RequiredInput, "Genes")),
    data = list(vector_contract("gene", "score", GuaranteedOutput, "numeric", "Scores"))
)
contract_daf <- contractor("my.computation", contract, daf)
verify_input(contract_daf$daf, contract_daf$contract)
# ... run computation ...
verify_output(contract_daf$daf, contract_daf$contract)
} # }
```
