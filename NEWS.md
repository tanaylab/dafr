# dafr 0.0.3

## New Functions

### New Operations
* Added `Count()` for counting non-zero elements, with optional `type` parameter
* Added `GeoMean()` for geometric mean reduction, with optional `type` and `eps` parameters
* Added `Mode()` for most common value reduction

### Query Utilities
* Added `escape_value()` for escaping special characters in query strings
* Added `unescape_value()` for reversing `escape_value()`
* Added `query_requires_relayout()` to check if a query needs data relayout

### Empty Data Functions
* Added `get_empty_dense_vector()` for getting empty dense vectors for in-place filling
* Added `get_empty_sparse_vector()` for getting empty sparse vectors for in-place filling
* Added `get_empty_dense_matrix()` for getting empty dense matrices for in-place filling
* Added `get_empty_sparse_matrix()` for getting empty sparse matrices for in-place filling
* Added `filled_empty_sparse_vector()` for committing filled sparse vectors back to Daf
* Added `filled_empty_sparse_matrix()` for committing filled sparse matrices back to Daf
* Exported `get_frame()` for direct use

## Enhanced Parameters

* Added `type` parameter to all reduction operations (`Abs`, `Sum`, `Mean`, `Median`, `Quantile`, `Var`, `VarN`, `Std`, `StdN`, `Min`, `Max`, `Count`, `Fraction`, `Round`, `Clamp`, `Log`)
* Added `eps` parameter to `VarN`, `StdN`, `GeoMean`
* Added `type` and `insist` parameters to `copy_scalar()`
* Added `eltype`, `bestify`, `min_sparse_saving_fraction`, and `insist` parameters to `copy_vector()`
* Added `eltype`, `bestify`, `min_sparse_saving_fraction`, and `insist` parameters to `copy_matrix()`
* Added `relayout`, `bestify`, `min_sparse_saving_fraction` parameters to `copy_tensor()`
* Added `X_eltype` parameter to `daf_as_h5ad()`

## CI/CD

* Added GitHub Actions workflows for R CMD check, conda build, and pkgdown site deployment
* Added conda recipe for building and distributing conda packages
* Fixed CI test execution to properly install package before running testthat tests

## Tests

* Added comprehensive tests for all new functions and parameters
* New test files: `test-operations.R`, `test-copies.R`, `test-data-writers.R`, `test-queries.R`, `test-anndata_format.R`

# dafr 0.0.2

## New Features

### Contract System
* Added `create_contract()` for defining data contracts with axes and data specifications
* Added `axis_contract()`, `scalar_contract()`, `vector_contract()`, `matrix_contract()` for specifying contract requirements
* Added `tensor_contract()` for 3D tensor specifications
* Added `verify_contract()`, `verify_input()`, `verify_output()` for validating Daf objects against contracts
* Added `contractor()` for creating contract-aware Daf wrappers
* Added `contract_docs()` for generating markdown/text documentation from contracts
* Added expectation types: `RequiredInput`, `OptionalInput`, `GuaranteedOutput`, `OptionalOutput`

### Group Functions
* Added `group_names()` for generating unique deterministic names for groups based on members
* Added `collect_group_members()` for converting group indices to member lists
* Added `compact_groups()` for compacting non-consecutive group indices to 1..N

### View Constants
* Added `VIEW_ALL_AXES`, `VIEW_ALL_SCALARS`, `VIEW_ALL_VECTORS`, `VIEW_ALL_MATRICES`, `VIEW_ALL_DATA` constants for creating views of complete data

### Query Utilities
* Added `is_axis_query()` to check if a query targets only axes
* Added `query_axis_name()` to extract axis name from axis-only queries

### Version Counters
* Added `axis_version_counter()`, `vector_version_counter()`, `matrix_version_counter()` for tracking data modifications

### Complete/Open Functions
* Added `complete_daf()` for opening complete chains of Daf repositories
* Added `open_daf()` for smart opening of files-based or HDF5-based Daf

### Example Data
* Added `example_chain_daf()` for creating example chain data

## Improvements

* Added `tensors` parameter to `description()` function
* Fixed issue with error when returning only names (#6)
* Improved test coverage with 1201 tests (up from baseline)

# dafr 0.0.1

* Initial WIP

