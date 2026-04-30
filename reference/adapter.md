# Apply a computation to a renaming view of a DafWriter.

Mirrors Julia
`adapter(computation, daf; input_axes, input_data, capture, output_axes, output_data, empty, relayout, overwrite, name)`.
The typical use is to run a `@computation` (R:
[`computation()`](https://tanaylab.github.io/dafr/reference/computation.md)-wrapped)
function whose expected property names differ from the names stored in
`daf`.

## Usage

``` r
adapter(
  daf,
  fn,
  input_axes = NULL,
  input_data = NULL,
  output_axes = NULL,
  output_data = NULL,
  capture = memory_daf,
  empty = NULL,
  relayout = TRUE,
  overwrite = FALSE,
  name = ".adapter"
)
```

## Arguments

- daf:

  A `DafWriter` — the base data to read from and write into.

- fn:

  A function taking a single `DafWriter` argument (the `adapted` chain).
  Return value passes through.

- input_axes, input_data:

  Passed through to
  [`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) for
  the input view. At least one of these or `output_axes` / `output_data`
  must be non-NULL (otherwise `adapter()` degenerates to `fn(daf)`).

- output_axes, output_data:

  Passed through to
  [`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) for
  the output view.

- capture:

  Factory function returning a fresh `DafWriter`. Default `memory_daf`.

- empty:

  Named list
  `list("<axis>|<vector>" = default, "<r>|<c>|<m>" = default)` supplying
  default values for entries present in `daf`'s axis but absent from the
  source view's. NULL (default) disables the feature.

- relayout:

  If `TRUE` (default), matrix copies also write the transposed layout.

- overwrite:

  If `TRUE`, pre-existing destination entries are replaced.

- name:

  Human-readable name for the input/capture/adapted dafs. Default
  `".adapter"`.

## Value

The return value of `fn(adapted)`.

## Details

Flow:

1.  `input = viewer(daf, axes = input_axes, data = input_data)` exposes
    the subset the computation consumes, possibly under renamed axes /
    names.

2.  `capture = capture_factory(name = "<base>.capture")` is a fresh
    writable.

3.  `adapted = chain_writer(list(input, capture))` — reads fall through
    to `input`, writes go to `capture`.

4.  `result = fn(adapted)` — the computation's return value.

5.  `output = viewer(adapted, axes = output_axes, data = output_data)` —
    selects + renames the outputs.

6.  Copy `output` into `daf` via an internal helper. Honors `overwrite`,
    `relayout`, and `empty`.

7.  Return `result`.

## See also

[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md),
[`chain_writer()`](https://tanaylab.github.io/dafr/reference/chain_writer.md),
[`computation()`](https://tanaylab.github.io/dafr/reference/computation.md).

## Examples

``` r
d <- memory_daf(name = "base")
add_axis(d, "cell", c("c1", "c2", "c3"))
set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
adapter(d,
    function(adapted) {
        entries <- axis_vector(adapted, "obs")
        set_vector(adapted, "obs", "rank", seq_along(entries))
    },
    input_axes  = list(list("obs", "@ cell"), list("cell", NULL)),
    input_data  = VIEW_ALL_VECTORS,
    output_axes = list(list("cell", "@ obs"), list("obs", NULL)),
    output_data = list(list(ALL_VECTORS, NULL), list(c("cell", "rank"), "="))
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "base.adapter.adapted"
#>  @ internal              :<environment: 0x559554abffc0> 
#>  @ cache                 :<environment: 0x559554abc4d8> 
#>  @ axis_version_counter  :<environment: 0x559554abe068> 
#>  @ vector_version_counter:<environment: 0x559554aba510> 
#>  @ matrix_version_counter:<environment: 0x559554aba7e8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::ViewDaf>
#>  ..  ..@ name                  : chr "base.adapter.input"
#>  ..  ..@ internal              :<environment: 0x559554b589c8> 
#>  ..  ..@ cache                 :<environment: 0x559554ca97e0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x559554ca7620> 
#>  ..  ..@ vector_version_counter:<environment: 0x559554ca78f8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x559554ca7c08> 
#>  ..  ..@ base                  : <dafr::MemoryDaf>
#>  .. .. .. @ name                  : chr "base"
#>  .. .. .. @ internal              :<environment: 0x559554cac228> 
#>  .. .. .. @ cache                 :<environment: 0x559554ca97e0> 
#>  .. .. .. @ axis_version_counter  :<environment: 0x559554ca7620> 
#>  .. .. .. @ vector_version_counter:<environment: 0x559554ca78f8> 
#>  .. .. .. @ matrix_version_counter:<environment: 0x559554ca7c08> 
#>  ..  ..@ view_axes             :List of 1
#>  .. .. .. $ obs: chr "@ cell"
#>  ..  ..@ view_axis_renames     :List of 1
#>  .. .. .. $ obs: chr "cell"
#>  ..  ..@ view_axis_indices     :List of 1
#>  .. .. .. $ obs: int [1:3] 1 2 3
#>  ..  ..@ view_scalars          : list()
#>  ..  ..@ view_vectors          :List of 1
#>  .. .. .. $ obs|donor:List of 4
#>  .. .. ..  ..$ view_axis: chr "obs"
#>  .. .. ..  ..$ base_axis: chr "cell"
#>  .. .. ..  ..$ name     : chr "donor"
#>  .. .. ..  ..$ query    : chr "="
#>  ..  ..@ view_matrices         : list()
#>  .. $ : <dafr::MemoryDaf>
#>  ..  ..@ name                  : chr "base.adapter.capture"
#>  ..  ..@ internal              :<environment: 0x559554aff2e8> 
#>  ..  ..@ cache                 :<environment: 0x559554afc7c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x559554afe318> 
#>  ..  ..@ vector_version_counter:<environment: 0x559554afa7c0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x559554afaa98> 
#>  @ writer                : <dafr::MemoryDaf>
#>  .. @ name                  : chr "base.adapter.capture"
#>  .. @ internal              :<environment: 0x559554aff2e8> 
#>  .. @ cache                 :<environment: 0x559554afc7c0> 
#>  .. @ axis_version_counter  :<environment: 0x559554afe318> 
#>  .. @ vector_version_counter:<environment: 0x559554afa7c0> 
#>  .. @ matrix_version_counter:<environment: 0x559554afaa98> 
get_vector(d, "cell", "rank")
#> c1 c2 c3 
#>  1  2  3 
```
