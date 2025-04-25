
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dafr - Data in Axes in Formats for R

<!-- badges: start -->

[![R-CMD-check](https://github.com/tanaylab/dafr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tanaylab/dafr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of dafr is to provide R bindings for the
[DataAxesFormats.jl](https://github.com/tanaylab/DataAxesFormats.jl)
Julia package, which provides a uniform generic interface for accessing
1D and 2D data arranged along some set of axes. This is a much-needed
generalization of the [AnnData](https://github.com/scverse/anndata)
functionality.

The dafr package enables R users to access `Daf` data using familiar R
interfaces while leveraging the powerful implementation provided by the
Julia package. The package follows a functional interface similar to the
Julia implementation.

## Installation

You can install the development version of dafr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tanaylab/dafr")
```

Note that dafr requires the Julia programming language and the
DataAxesFormats.jl package to be installed. The package uses
[JuliaCall](https://github.com/Non-Contradiction/JuliaCall) to interface
with Julia from R.

## Usage

The R package provides an interface similar to the Julia package, with
functions that mirror the Julia API:

``` r
library(dafr)
# Setup connection to Julia
setup_daf()
```

``` r
# Create a DAF object in memory
daf <- memory_daf("example")

# Add axes
add_axis(daf, "obs", c("cell1", "cell2", "cell3"))
add_axis(daf, "var", c("gene1", "gene2"))

# Add vector data
set_vector(daf, "obs", "score", c(0.1, 0.5, 0.9))

# Add matrix data
mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
set_matrix(daf, "obs", "var", "counts", mat)

# Access data
get_vector(daf, "obs", "score")
#> [1] 0.1 0.5 0.9
get_matrix(daf, "obs", "var", "counts")
#>       gene1 gene2
#> cell1     1     4
#> cell2     2     5
#> cell3     3     6

# Get a dataframe view
get_dataframe(daf, "obs", c("score"))
#>       score
#> cell1   0.1
#> cell2   0.5
#> cell3   0.9
```

## Querying Data

One of the powerful features of dafr is its query language. You can use
queries to access and transform data flexibly. The package provides a
convenient `[` operator for concise query syntax:

``` r
# Create a sample dataset
daf <- memory_daf("query_examples")
add_axis(daf, "cell", c("A", "B", "C"))
add_axis(daf, "gene", c("X", "Y", "Z"))
set_vector(daf, "cell", "age", c(1, 5, 10))
set_vector(daf, "cell", "type", c("T", "B", "NK"))
set_scalar(daf, "version", "1.0")
set_matrix(daf, "cell", "gene", "counts", matrix(1:9, nrow = 3, ncol = 3))

# Basic queries using the [ operator
# Get scalar values
daf[": version"] # Get a scalar
#> [1] "1.0"

# Access axes and properties
daf["? axes"] # List all axes
#> [1] "gene" "cell"
daf["/ cell ?"] # List properties of the "cell" axis
#> [1] "type" "age"

# Access vector data
daf["/ cell : age"] # Get the "age" vector for all cells
#> [1]  1  5 10

# Access matrix data
daf["/ cell / gene : counts"] # Get the full counts matrix
#>      [,1] [,2] [,3]
#> [1,]    1    4    7
#> [2,]    2    5    8
#> [3,]    3    6    9

# Filter data
daf["/ cell & age > 2 : type"] # Get types of cells with age > 2
#> [1] "B"  "NK"

# Transform data
daf["/ cell : age % Abs"] # Get absolute values of ages
#> [1]  1  5 10
daf["/ cell : age % Log base 2"] # Get log2 of ages
#> [1] 0.000000 2.321928 3.321928

# Aggregations
daf["/ cell : age %> Max"] # Get maximum age
#> [1] 10
daf["/ cell : age %> Mean"] # Get mean age
#> [1] 5.333333

# Complex operations can be built using the pipe operator or query functions
# Get types of cells with age > 2
Axis("cell") |>
    And("age") |>
    IsGreater(2) |>
    Lookup("type") |>
    get_query(daf)
#> [1] "B"  "NK"
```

The query syntax follows a simple pattern:

- `/ axis` selects an axis
- `: property` retrieves a property
- `% operation` applies an element-wise operation
- `%> reduction` applies a reduction operation
- `& property` filters by a property
- `= value`, `> value`, etc. apply comparison operations

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html)
for more details about the query language.

### Important Note About Data Copying

Due to limitations in JuliaCall, data is copied between R and Julia
during function calls. This means that operations involving large
matrices or vectors will incur memory overhead. For large datasets,
consider performing more operations directly on the Julia side to
minimize data transfers.

## Key Features

The dafr package inherits the key features of DataAxesFormats.jl:

- Support for both in-memory and persistent data storage
- Thread-safe implementation using read/write locks
- Ability to import and export AnnData objects
- Support for views and adapters
- Chaining repositories for efficient reuse of data
- Flexible data model based on axes, vectors, matrices, and scalar data
- Explicit support for grouped axes
- Simple query language for common operations
- Control over data layout and support for both dense and sparse
  matrices

## Status

This package is in early development. The API is subject to change based
on user feedback. Comments, bug reports, and PRs are welcome!

### Future Plans

In future releases, we plan to implement a dplyr-like API for dafr to
make it even more intuitive for R users. This will allow you to use
familiar verbs like `filter()`, `select()`, `mutate()`, and
`summarize()` with dafr objects, combining the power of the Julia
implementation with the ergonomics of the tidyverse.

## License (MIT)

Copyright © 2025 Weizmann Institute of Science

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
