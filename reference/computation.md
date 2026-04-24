# Wrap a function so a contract is enforced on every call.

Returns a closure that, on each call, wraps its first (`daf`) argument
in a
[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md),
runs
[`verify_input()`](https://tanaylab.github.io/dafr/reference/verify_input.md),
executes `fn`, then runs
[`verify_output()`](https://tanaylab.github.io/dafr/reference/verify_input.md),
and returns `fn`'s result. When contract enforcement is disabled (env
`DAF_ENFORCE_CONTRACTS` / option `dafr.enforce_contracts` are both
falsy), the wrapping is still performed but verification is a no-op, so
the wrapper is cheap to leave in place.

## Usage

``` r
computation(name, contract, fn)
```

## Arguments

- name:

  Human-readable computation name (character scalar). Used in contract
  violation messages and as a suffix on the wrapper's daf name.

- contract:

  A
  [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md)
  describing the inputs and outputs.

- fn:

  The function to wrap. Must accept a `DafReader`/`DafWriter` as its
  first argument.

## Value

A function with the same signature as `fn`. The bound `contract` and
`name` are attached as attributes (`dafr_contract`,
`dafr_computation_name`).

## Details

Scope: single contract (matching Julia's exported
`@computation Contract` form). Two-contract / three-contract variants
(Julia UNTESTED) are not supported in this slice; pass a single
`Contract` or use a merged one via `merge_contracts`.

## See also

[`function_contract()`](https://tanaylab.github.io/dafr/reference/function_contract.md),
[`contract_description()`](https://tanaylab.github.io/dafr/reference/contract_description.md),
[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md).

## Examples

``` r
withr::with_options(list(dafr.enforce_contracts = TRUE), {
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_vector("cell", "donor",
            RequiredInput, "character", "donor id"))
    )
    read_donors <- computation("read_donors", c,
        function(daf) get_vector(daf, "cell", "donor"))
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    read_donors(d)
})
#>   c1   c2 
#> "d1" "d2" 
```
