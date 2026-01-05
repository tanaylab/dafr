# Changelog

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
