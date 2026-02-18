# Changelog

## dafr 0.0.3

### New Functions

#### New Operations

- Added [`Count()`](https://tanaylab.github.io/dafr/reference/Count.md)
  for counting non-zero elements, with optional `type` parameter
- Added
  [`GeoMean()`](https://tanaylab.github.io/dafr/reference/GeoMean.md)
  for geometric mean reduction, with optional `type` and `eps`
  parameters
- Added [`Mode()`](https://tanaylab.github.io/dafr/reference/Mode.md)
  for most common value reduction

#### Query Utilities

- Added
  [`escape_value()`](https://tanaylab.github.io/dafr/reference/escape_value.md)
  for escaping special characters in query strings
- Added
  [`unescape_value()`](https://tanaylab.github.io/dafr/reference/unescape_value.md)
  for reversing
  [`escape_value()`](https://tanaylab.github.io/dafr/reference/escape_value.md)
- Added
  [`query_requires_relayout()`](https://tanaylab.github.io/dafr/reference/query_requires_relayout.md)
  to check if a query needs data relayout

#### Empty Data Functions

- Added
  [`get_empty_dense_vector()`](https://tanaylab.github.io/dafr/reference/get_empty_dense_vector.md)
  for getting empty dense vectors for in-place filling
- Added
  [`get_empty_sparse_vector()`](https://tanaylab.github.io/dafr/reference/get_empty_sparse_vector.md)
  for getting empty sparse vectors for in-place filling
- Added
  [`get_empty_dense_matrix()`](https://tanaylab.github.io/dafr/reference/get_empty_dense_matrix.md)
  for getting empty dense matrices for in-place filling
- Added
  [`get_empty_sparse_matrix()`](https://tanaylab.github.io/dafr/reference/get_empty_sparse_matrix.md)
  for getting empty sparse matrices for in-place filling
- Added
  [`filled_empty_sparse_vector()`](https://tanaylab.github.io/dafr/reference/filled_empty_sparse_vector.md)
  for committing filled sparse vectors back to Daf
- Added
  [`filled_empty_sparse_matrix()`](https://tanaylab.github.io/dafr/reference/filled_empty_sparse_matrix.md)
  for committing filled sparse matrices back to Daf
- Exported
  [`get_frame()`](https://tanaylab.github.io/dafr/reference/get_frame.md)
  for direct use

### Enhanced Parameters

- Added `type` parameter to all reduction operations (`Abs`, `Sum`,
  `Mean`, `Median`, `Quantile`, `Var`, `VarN`, `Std`, `StdN`, `Min`,
  `Max`, `Count`, `Fraction`, `Round`, `Clamp`, `Log`)
- Added `eps` parameter to `VarN`, `StdN`, `GeoMean`
- Added `type` and `insist` parameters to
  [`copy_scalar()`](https://tanaylab.github.io/dafr/reference/copy_scalar.md)
- Added `eltype`, `bestify`, `min_sparse_saving_fraction`, and `insist`
  parameters to
  [`copy_vector()`](https://tanaylab.github.io/dafr/reference/copy_vector.md)
- Added `eltype`, `bestify`, `min_sparse_saving_fraction`, and `insist`
  parameters to
  [`copy_matrix()`](https://tanaylab.github.io/dafr/reference/copy_matrix.md)
- Added `relayout`, `bestify`, `min_sparse_saving_fraction` parameters
  to
  [`copy_tensor()`](https://tanaylab.github.io/dafr/reference/copy_tensor.md)
- Added `X_eltype` parameter to
  [`daf_as_h5ad()`](https://tanaylab.github.io/dafr/reference/daf_as_h5ad.md)

### CI/CD

- Added GitHub Actions workflows for R CMD check, conda build, and
  pkgdown site deployment
- Added conda recipe for building and distributing conda packages
- Fixed CI test execution to properly install package before running
  testthat tests

### Tests

- Added comprehensive tests for all new functions and parameters
- New test files: `test-operations.R`, `test-copies.R`,
  `test-data-writers.R`, `test-queries.R`, `test-anndata_format.R`

## dafr 0.0.2

### New Features

#### Contract System

- Added
  [`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md)
  for defining data contracts with axes and data specifications
- Added
  [`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md),
  [`scalar_contract()`](https://tanaylab.github.io/dafr/reference/scalar_contract.md),
  [`vector_contract()`](https://tanaylab.github.io/dafr/reference/vector_contract.md),
  [`matrix_contract()`](https://tanaylab.github.io/dafr/reference/matrix_contract.md)
  for specifying contract requirements
- Added
  [`tensor_contract()`](https://tanaylab.github.io/dafr/reference/tensor_contract.md)
  for 3D tensor specifications
- Added
  [`verify_contract()`](https://tanaylab.github.io/dafr/reference/verify_contract.md),
  [`verify_input()`](https://tanaylab.github.io/dafr/reference/verify_input.md),
  [`verify_output()`](https://tanaylab.github.io/dafr/reference/verify_output.md)
  for validating Daf objects against contracts
- Added
  [`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md)
  for creating contract-aware Daf wrappers
- Added
  [`contract_docs()`](https://tanaylab.github.io/dafr/reference/contract_docs.md)
  for generating markdown/text documentation from contracts
- Added expectation types: `RequiredInput`, `OptionalInput`,
  `GuaranteedOutput`, `OptionalOutput`

#### Group Functions

- Added
  [`group_names()`](https://tanaylab.github.io/dafr/reference/group_names.md)
  for generating unique deterministic names for groups based on members
- Added
  [`collect_group_members()`](https://tanaylab.github.io/dafr/reference/collect_group_members.md)
  for converting group indices to member lists
- Added
  [`compact_groups()`](https://tanaylab.github.io/dafr/reference/compact_groups.md)
  for compacting non-consecutive group indices to 1..N

#### View Constants

- Added `VIEW_ALL_AXES`, `VIEW_ALL_SCALARS`, `VIEW_ALL_VECTORS`,
  `VIEW_ALL_MATRICES`, `VIEW_ALL_DATA` constants for creating views of
  complete data

#### Query Utilities

- Added
  [`is_axis_query()`](https://tanaylab.github.io/dafr/reference/is_axis_query.md)
  to check if a query targets only axes
- Added
  [`query_axis_name()`](https://tanaylab.github.io/dafr/reference/query_axis_name.md)
  to extract axis name from axis-only queries

#### Version Counters

- Added
  [`axis_version_counter()`](https://tanaylab.github.io/dafr/reference/axis_version_counter.md),
  [`vector_version_counter()`](https://tanaylab.github.io/dafr/reference/vector_version_counter.md),
  [`matrix_version_counter()`](https://tanaylab.github.io/dafr/reference/matrix_version_counter.md)
  for tracking data modifications

#### Complete/Open Functions

- Added
  [`complete_daf()`](https://tanaylab.github.io/dafr/reference/complete_daf.md)
  for opening complete chains of Daf repositories
- Added
  [`open_daf()`](https://tanaylab.github.io/dafr/reference/open_daf.md)
  for smart opening of files-based or HDF5-based Daf

#### Example Data

- Added
  [`example_chain_daf()`](https://tanaylab.github.io/dafr/reference/example_chain_daf.md)
  for creating example chain data

### Improvements

- Added `tensors` parameter to
  [`description()`](https://tanaylab.github.io/dafr/reference/description.md)
  function
- Fixed issue with error when returning only names
  ([\#6](https://github.com/tanaylab/dafr/issues/6))
- Improved test coverage with 1201 tests (up from baseline)

## dafr 0.0.1

- Initial WIP
