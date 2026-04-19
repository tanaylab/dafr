# dafr

Native R implementation of the DataAxesFormats (DAF) data model.

**Status:** pre-alpha, under active development. Not yet installable.

For the existing Julia-facade version, see `DafJuliaWrapper`.

## Development

```r
pkgbuild::clean_dll(); pkgbuild::compile_dll(debug = FALSE)
devtools::load_all()
alutil::tst(parallel = TRUE)
```
