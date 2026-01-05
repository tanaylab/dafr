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

