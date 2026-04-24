# Construct a [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md) from typed per-category argument lists.

User-friendly constructor that concatenates `scalars`, `vectors`,
`matrices`, and `tensors` into the flat `data` slot of
[`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md),
and converts `axes` (a list of
[`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md)
records) into the named-list form the underlying class expects.

## Usage

``` r
create_contract(
  scalars = list(),
  vectors = list(),
  matrices = list(),
  tensors = list(),
  axes = list(),
  is_relaxed = FALSE
)
```

## Arguments

- scalars:

  List of
  [`contract_scalar()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  records.

- vectors:

  List of
  [`contract_vector()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  records.

- matrices:

  List of
  [`contract_matrix()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  records.

- tensors:

  List of
  [`tensor_contract()`](https://tanaylab.github.io/dafr/reference/tensor_contract.md)
  records.

- axes:

  List of
  [`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md)
  records.

- is_relaxed:

  Logical; if `TRUE`, accesses to properties outside the contract are
  allowed at enforcement time.

## Value

A [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md)
object.

## See also

[`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md),
[`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md),
[`tensor_contract()`](https://tanaylab.github.io/dafr/reference/tensor_contract.md),
[`verify_contract()`](https://tanaylab.github.io/dafr/reference/verify_contract.md),
[`contract_docs()`](https://tanaylab.github.io/dafr/reference/contract_docs.md)

## Examples

``` r
create_contract(
    axes = list(
        axis_contract("cell", RequiredInput, "per-cell axis"),
        axis_contract("gene", RequiredInput, "per-gene axis")
    ),
    scalars = list(contract_scalar("organism", RequiredInput, "character", "species")),
    vectors = list(contract_vector("cell", "donor", RequiredInput, "character", "donor id")),
    matrices = list(contract_matrix("cell", "gene", "UMIs", RequiredInput, "integer", "UMIs"))
)
#> <dafr::Contract>
#>  @ name      : chr ""
#>  @ is_relaxed: logi FALSE
#>  @ axes      :List of 2
#>  .. $ cell:List of 2
#>  ..  ..$ : chr "RequiredInput"
#>  ..  ..$ : chr "per-cell axis"
#>  .. $ gene:List of 2
#>  ..  ..$ : chr "RequiredInput"
#>  ..  ..$ : chr "per-gene axis"
#>  @ data      :List of 3
#>  .. $ :List of 5
#>  ..  ..$ kind       : chr "scalar"
#>  ..  ..$ name       : chr "organism"
#>  ..  ..$ expectation: chr "RequiredInput"
#>  ..  ..$ type       : chr "character"
#>  ..  ..$ description: chr "species"
#>  .. $ :List of 6
#>  ..  ..$ kind       : chr "vector"
#>  ..  ..$ axis       : chr "cell"
#>  ..  ..$ name       : chr "donor"
#>  ..  ..$ expectation: chr "RequiredInput"
#>  ..  ..$ type       : chr "character"
#>  ..  ..$ description: chr "donor id"
#>  .. $ :List of 7
#>  ..  ..$ kind        : chr "matrix"
#>  ..  ..$ rows_axis   : chr "cell"
#>  ..  ..$ columns_axis: chr "gene"
#>  ..  ..$ name        : chr "UMIs"
#>  ..  ..$ expectation : chr "RequiredInput"
#>  ..  ..$ type        : chr "integer"
#>  ..  ..$ description : chr "UMIs"
```
