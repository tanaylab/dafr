# Single-pass contract verification (input + output).

Wraps `daf` in a fresh
[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md)
call using `contract`, then runs
[`verify_input()`](https://tanaylab.github.io/dafr/reference/verify_input.md)
and
[`verify_output()`](https://tanaylab.github.io/dafr/reference/verify_input.md).
Errors early with a diagnostic on contract violation; returns
`invisible(daf)` on success.

## Usage

``` r
verify_contract(contract, daf)
```

## Arguments

- contract:

  A
  [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md).

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

## Value

Invisibly `daf`.

## See also

[`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md),
[`verify_input()`](https://tanaylab.github.io/dafr/reference/verify_input.md),
[`verify_output()`](https://tanaylab.github.io/dafr/reference/verify_input.md),
[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md)

## Examples

``` r
withr::with_options(list(dafr.enforce_contracts = TRUE), {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    c <- create_contract(
        axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
        vectors = list(contract_vector("cell", "donor", RequiredInput,
            "character", "donor id"))
    )
    verify_contract(c, d)
})
```
