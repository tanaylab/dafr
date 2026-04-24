# Wrap a daf with a contract for a computation

When contract enforcement is enabled (env `DAF_ENFORCE_CONTRACTS=1` or
`options(dafr.enforce_contracts = TRUE)`), returns a
[ContractDaf](https://tanaylab.github.io/dafr/reference/ContractDaf.md)
that tracks access and rejects operations outside `contract`. Otherwise
returns `daf` unchanged.

## Usage

``` r
contractor(computation, contract, daf, name = NULL, overwrite = FALSE)
```

## Arguments

- computation:

  Name of the computation (character scalar).

- contract:

  A
  [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md)
  describing expected inputs and outputs.

- daf:

  The underlying
  [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md) /
  [DafWriter](https://tanaylab.github.io/dafr/reference/DafWriter.md).

- name:

  Optional name for the wrapper; defaults to `<daf-name>.<computation>`.

- overwrite:

  If `TRUE`, pre-existing `CreatedOutput` properties are allowed.

## Value

`daf` itself, or a
[ContractDaf](https://tanaylab.github.io/dafr/reference/ContractDaf.md)
wrapping it.

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
    class(cd)[[1]]
})
#> [1] "dafr::ContractDaf"
```
