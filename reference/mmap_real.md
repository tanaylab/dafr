# Open a binary file as a read-only mmap-backed vector.

`mmap_real` opens a file of IEEE 754 doubles, `mmap_int` opens int32,
`mmap_lgl` opens int32 interpreted as R logicals. `length` is the
element count; the file must contain at least `length * sizeof(element)`
bytes.

## Usage

``` r
mmap_real(path, length, offset = 0)

mmap_int(path, length, offset = 0)

mmap_lgl(path, length, offset = 0)
```

## Arguments

- path:

  Path to the binary file.

- length:

  Number of elements to map.

- offset:

  Byte offset at which the elements begin (default 0). Lets a sub-region
  of a file - e.g. a contiguous HDF5 dataset at a known offset - be
  mapped. Must leave at least `length * sizeof(element)` bytes to EOF.

## Value

An ALTREP-backed R vector sharing the file's memory.

## Details

Length is passed through as `double` to accommodate `R_xlen_t`; values
above 2^53 lose precision. Pass only integer-valued doubles.

## Examples

``` r
f <- tempfile(fileext = ".bin")
writeBin(c(1.5, 2.5, 3.5), f)
v <- mmap_real(f, 3L)
v[]
#> [1] 1.5 2.5 3.5
unlink(f)
```
