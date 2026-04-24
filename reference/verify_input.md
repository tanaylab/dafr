# Verify contract inputs / outputs

`verify_input()` should be called before running a computation and
`verify_output()` after. Both are no-ops when `daf` is not a
[ContractDaf](https://tanaylab.github.io/dafr/reference/ContractDaf.md)
(i.e. enforcement is disabled). `verify_output()` additionally fails on
unused `RequiredInput` properties.

## Usage

``` r
verify_input(daf)

verify_output(daf)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md)
  — if it is a
  [ContractDaf](https://tanaylab.github.io/dafr/reference/ContractDaf.md),
  the associated contract is verified; otherwise the call is a silent
  no-op.

## Value

[`invisible()`](https://rdrr.io/r/base/invisible.html).

## Examples

``` r
withr::with_options(list(dafr.enforce_contracts = TRUE), {
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_vector("cell", "donor",
            RequiredInput, "character", "donor id"))
    )
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    cd <- contractor("demo", c, d)
    verify_input(cd)
    get_vector(cd, "cell", "donor")
    verify_output(cd)
})
```
