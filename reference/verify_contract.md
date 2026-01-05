# Verify a DAF object against a contract

Validates that a DAF object meets the contract requirements.

## Usage

``` r
verify_contract(daf_obj, contract, check_unused = FALSE)
```

## Arguments

- daf_obj:

  DAF object to validate

- contract:

  Contract created with create_contract

- check_unused:

  If TRUE, warns about required data that wasn't accessed

## Value

List with valid (logical), errors (character vector), warnings
(character vector)
