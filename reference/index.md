# Package index

## Core DAF Functionality

Core functions for working with Daf objects

- [`Daf()`](https://tanaylab.github.io/dafr/reference/Daf.md) : Daf
  (Data Axes Format) S3 object
- [`is_daf()`](https://tanaylab.github.io/dafr/reference/is_daf.md) :
  Check if object is a Daf
- [`print(`*`<Daf>`*`)`](https://tanaylab.github.io/dafr/reference/print.Daf.md)
  : Print method for Daf objects
- [`description()`](https://tanaylab.github.io/dafr/reference/description.md)
  : Get description of a Daf object
- [`` `[`( ``*`<Daf>`*`)`](https://tanaylab.github.io/dafr/reference/sub-.Daf.md)
  : Extract results from a Daf object using a query
- [`setup_daf()`](https://tanaylab.github.io/dafr/reference/setup_daf.md)
  : Set up of the Julia environment needed for DataAxesFormats and
  TanayLabUtilities
- [`use_default_julia_environment()`](https://tanaylab.github.io/dafr/reference/use_default_julia_environment.md)
  : Use default or custom Julia environment
- [`install_daf_packages()`](https://tanaylab.github.io/dafr/reference/install_daf_packages.md)
  : Install Julia packages needed for DataAxesFormats and
  TanayLabUtilities
- [`load_daf_packages()`](https://tanaylab.github.io/dafr/reference/load_daf_packages.md)
  : Load Julia packages needed for DataAxesFormats and TanayLabUtilities
- [`name()`](https://tanaylab.github.io/dafr/reference/name.md) : Gets
  the name of a Daf object

## Example Data

Functions for loading example data

- [`example_cells_daf()`](https://tanaylab.github.io/dafr/reference/example_cells_daf.md)
  : Load example cells data into a Daf object
- [`example_metacells_daf()`](https://tanaylab.github.io/dafr/reference/example_metacells_daf.md)
  : Load example metacells data into a Daf object
- [`example_chain_daf()`](https://tanaylab.github.io/dafr/reference/example_chain_daf.md)
  : Load example chain data into a Daf object

## Data Access and Query

Functions for querying and retrieving data from Daf objects

- [`get_dataframe()`](https://tanaylab.github.io/dafr/reference/get_dataframe.md)
  [`get_tidy()`](https://tanaylab.github.io/dafr/reference/get_dataframe.md)
  : Get a dataframe from a Daf object
- [`get_dataframe_query()`](https://tanaylab.github.io/dafr/reference/get_dataframe_query.md)
  : Apply a query to a Daf object and return result as a data.frame
- [`get_empty_dense_matrix()`](https://tanaylab.github.io/dafr/reference/get_empty_dense_matrix.md)
  : Get an empty dense matrix for filling
- [`get_empty_dense_vector()`](https://tanaylab.github.io/dafr/reference/get_empty_dense_vector.md)
  : Get an empty dense vector for filling
- [`get_empty_sparse_matrix()`](https://tanaylab.github.io/dafr/reference/get_empty_sparse_matrix.md)
  : Get an empty sparse matrix for filling
- [`get_empty_sparse_vector()`](https://tanaylab.github.io/dafr/reference/get_empty_sparse_vector.md)
  : Get an empty sparse vector for filling
- [`get_frame()`](https://tanaylab.github.io/dafr/reference/get_frame.md)
  : Get a dataframe from a Daf object (Julia-style)
- [`get_matrix()`](https://tanaylab.github.io/dafr/reference/get_matrix.md)
  : Get matrix from a Daf object
- [`get_query()`](https://tanaylab.github.io/dafr/reference/get_query.md)
  : Apply a query to a Daf object
- [`get_scalar()`](https://tanaylab.github.io/dafr/reference/get_scalar.md)
  : Get scalar value from a Daf object
- [`get_vector()`](https://tanaylab.github.io/dafr/reference/get_vector.md)
  : Get vector from a Daf object
- [`has_query()`](https://tanaylab.github.io/dafr/reference/has_query.md)
  : Check if a query can be applied to a Daf object
- [`parse_query()`](https://tanaylab.github.io/dafr/reference/parse_query.md)
  : Parse a query string into a query object
- [`query_result_dimensions()`](https://tanaylab.github.io/dafr/reference/query_result_dimensions.md)
  : Get the number of dimensions of a query result
- [`is_axis_query()`](https://tanaylab.github.io/dafr/reference/is_axis_query.md)
  : Check if a query returns axis entries
- [`query_axis_name()`](https://tanaylab.github.io/dafr/reference/query_axis_name.md)
  : Get the axis name from a query
- [`query_requires_relayout()`](https://tanaylab.github.io/dafr/reference/query_requires_relayout.md)
  : Check if a query requires relayout
- [`escape_value()`](https://tanaylab.github.io/dafr/reference/escape_value.md)
  : Escape a value for use in a query string
- [`unescape_value()`](https://tanaylab.github.io/dafr/reference/unescape_value.md)
  : Unescape a value from a query string

## Readers and Formats

Reading Daf data from different sources and formats

- [`memory_daf()`](https://tanaylab.github.io/dafr/reference/memory_daf.md)
  : Create a Daf object with in-memory storage
- [`h5df()`](https://tanaylab.github.io/dafr/reference/h5df.md) : Create
  a Daf object with HDF5-based storage
- [`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
  : Create a Daf object with file-based storage
- [`h5ad_as_daf()`](https://tanaylab.github.io/dafr/reference/h5ad_as_daf.md)
  : Convert h5ad file to a Daf object
- [`read_only()`](https://tanaylab.github.io/dafr/reference/read_only.md)
  : Create a read-only wrapper for a Daf object
- [`chain_reader()`](https://tanaylab.github.io/dafr/reference/chain_reader.md)
  : Create a read-only chain wrapper of DafReader objects
- [`complete_daf()`](https://tanaylab.github.io/dafr/reference/complete_daf.md)
  : Complete chain of Daf repositories
- [`open_daf()`](https://tanaylab.github.io/dafr/reference/open_daf.md)
  : Open a Daf repository based on path

## Writers

Writing Daf data to different formats

- [`daf_as_h5ad()`](https://tanaylab.github.io/dafr/reference/daf_as_h5ad.md)
  : Convert Daf object to h5ad file
- [`chain_writer()`](https://tanaylab.github.io/dafr/reference/chain_writer.md)
  : Create a writable chain wrapper of DafReader objects

## Constants and Handlers

Constants and handler functions

- [`ALL_AXES`](https://tanaylab.github.io/dafr/reference/ALL_AXES.md) :
  All axes specifier
- [`ALL_MATRICES`](https://tanaylab.github.io/dafr/reference/ALL_MATRICES.md)
  : All matrices specifier
- [`ALL_SCALARS`](https://tanaylab.github.io/dafr/reference/ALL_SCALARS.md)
  : All scalars specifier
- [`ALL_VECTORS`](https://tanaylab.github.io/dafr/reference/ALL_VECTORS.md)
  : All vectors specifier
- [`VIEW_ALL_AXES`](https://tanaylab.github.io/dafr/reference/VIEW_ALL_AXES.md)
  : View all axes specifier
- [`VIEW_ALL_DATA`](https://tanaylab.github.io/dafr/reference/VIEW_ALL_DATA.md)
  : View all data specifier
- [`VIEW_ALL_MATRICES`](https://tanaylab.github.io/dafr/reference/VIEW_ALL_MATRICES.md)
  : View all matrices specifier
- [`VIEW_ALL_SCALARS`](https://tanaylab.github.io/dafr/reference/VIEW_ALL_SCALARS.md)
  : View all scalars specifier
- [`VIEW_ALL_VECTORS`](https://tanaylab.github.io/dafr/reference/VIEW_ALL_VECTORS.md)
  : View all vectors specifier
- [`IGNORE_HANDLER`](https://tanaylab.github.io/dafr/reference/abnormal_handlers.md)
  [`WARN_HANDLER`](https://tanaylab.github.io/dafr/reference/abnormal_handlers.md)
  [`ERROR_HANDLER`](https://tanaylab.github.io/dafr/reference/abnormal_handlers.md)
  : Handler types for abnormal operations
- [`inefficient_action_handler()`](https://tanaylab.github.io/dafr/reference/inefficient_action_handler.md)
  : Set the handler for inefficient matrix access

## Axes and Properties

Functions for managing axes and properties in Daf objects

- [`has_axis()`](https://tanaylab.github.io/dafr/reference/has_axis.md)
  : Check if an axis exists in a Daf object
- [`add_axis()`](https://tanaylab.github.io/dafr/reference/add_axis.md)
  : Add axis to a Daf object
- [`delete_axis()`](https://tanaylab.github.io/dafr/reference/delete_axis.md)
  : Delete axis from a Daf object
- [`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md)
  : Create an axis contract specification
- [`axis_dict()`](https://tanaylab.github.io/dafr/reference/axis_dict.md)
  : Get dictionary of axis entries to indices
- [`axis_entries()`](https://tanaylab.github.io/dafr/reference/axis_entries.md)
  : Get entry names for indices in an axis
- [`axis_indices()`](https://tanaylab.github.io/dafr/reference/axis_indices.md)
  : Get indices of entries in an axis
- [`axis_length()`](https://tanaylab.github.io/dafr/reference/axis_length.md)
  : Get length of an axis in a Daf object
- [`axis_vector()`](https://tanaylab.github.io/dafr/reference/axis_vector.md)
  : Get vector of axis entries from a Daf object
- [`axis_version_counter()`](https://tanaylab.github.io/dafr/reference/axis_version_counter.md)
  : Get axis version counter
- [`axes_set()`](https://tanaylab.github.io/dafr/reference/axes_set.md)
  : Get set of axis names from a Daf object
- [`matrices_set()`](https://tanaylab.github.io/dafr/reference/matrices_set.md)
  : Get set of matrix names for axes in a Daf object
- [`scalars_set()`](https://tanaylab.github.io/dafr/reference/scalars_set.md)
  : Get set of scalar names from a Daf object
- [`vectors_set()`](https://tanaylab.github.io/dafr/reference/vectors_set.md)
  : Get set of vector names for an axis in a Daf object
- [`has_scalar()`](https://tanaylab.github.io/dafr/reference/has_scalar.md)
  : Check if a scalar exists in a Daf object
- [`has_vector()`](https://tanaylab.github.io/dafr/reference/has_vector.md)
  : Check if a vector exists in a Daf object
- [`has_matrix()`](https://tanaylab.github.io/dafr/reference/has_matrix.md)
  : Check if a matrix exists in a Daf object
- [`delete_matrix()`](https://tanaylab.github.io/dafr/reference/delete_matrix.md)
  : Delete matrix from a Daf object
- [`delete_scalar()`](https://tanaylab.github.io/dafr/reference/delete_scalar.md)
  : Delete scalar from a Daf object
- [`delete_vector()`](https://tanaylab.github.io/dafr/reference/delete_vector.md)
  : Delete vector from a Daf object
- [`get_matrix()`](https://tanaylab.github.io/dafr/reference/get_matrix.md)
  : Get matrix from a Daf object
- [`get_scalar()`](https://tanaylab.github.io/dafr/reference/get_scalar.md)
  : Get scalar value from a Daf object
- [`get_vector()`](https://tanaylab.github.io/dafr/reference/get_vector.md)
  : Get vector from a Daf object
- [`set_matrix()`](https://tanaylab.github.io/dafr/reference/set_matrix.md)
  : Set matrix in a Daf object
- [`set_scalar()`](https://tanaylab.github.io/dafr/reference/set_scalar.md)
  : Set scalar value in a Daf object
- [`set_vector()`](https://tanaylab.github.io/dafr/reference/set_vector.md)
  : Set vector in a Daf object
- [`relayout_matrix()`](https://tanaylab.github.io/dafr/reference/relayout_matrix.md)
  : Relayout matrix in a Daf object
- [`matrix_version_counter()`](https://tanaylab.github.io/dafr/reference/matrix_version_counter.md)
  : Get matrix version counter
- [`vector_version_counter()`](https://tanaylab.github.io/dafr/reference/vector_version_counter.md)
  : Get vector version counter
- [`get_empty_dense_vector()`](https://tanaylab.github.io/dafr/reference/get_empty_dense_vector.md)
  : Get an empty dense vector for filling
- [`get_empty_sparse_vector()`](https://tanaylab.github.io/dafr/reference/get_empty_sparse_vector.md)
  : Get an empty sparse vector for filling
- [`get_empty_dense_matrix()`](https://tanaylab.github.io/dafr/reference/get_empty_dense_matrix.md)
  : Get an empty dense matrix for filling
- [`get_empty_sparse_matrix()`](https://tanaylab.github.io/dafr/reference/get_empty_sparse_matrix.md)
  : Get an empty sparse matrix for filling
- [`filled_empty_sparse_vector()`](https://tanaylab.github.io/dafr/reference/filled_empty_sparse_vector.md)
  : Signal that an empty sparse vector has been filled
- [`filled_empty_sparse_matrix()`](https://tanaylab.github.io/dafr/reference/filled_empty_sparse_matrix.md)
  : Signal that an empty sparse matrix has been filled

## Query Operations - Element-wise

Element-wise operations that preserve data shape but transform values

- [`Abs()`](https://tanaylab.github.io/dafr/reference/Abs.md) : Abs
  query operation
- [`Clamp()`](https://tanaylab.github.io/dafr/reference/Clamp.md) :
  Clamp query operation
- [`Convert()`](https://tanaylab.github.io/dafr/reference/Convert.md) :
  Convert query operation
- [`Fraction()`](https://tanaylab.github.io/dafr/reference/Fraction.md)
  : Fraction query operation
- [`Log()`](https://tanaylab.github.io/dafr/reference/Log.md) : Log
  query operation
- [`Round()`](https://tanaylab.github.io/dafr/reference/Round.md) :
  Round query operation
- [`Significant()`](https://tanaylab.github.io/dafr/reference/Significant.md)
  : Significant query operation

## Query Operations - Reduction

Reduction operations that reduce dimensions (matrix to vector, vector to
scalar)

- [`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md) : Sum
  query operation
- [`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md) : Mean
  query operation
- [`Median()`](https://tanaylab.github.io/dafr/reference/Median.md) :
  Median query operation
- [`Min()`](https://tanaylab.github.io/dafr/reference/Min.md) : Min
  query operation
- [`Max()`](https://tanaylab.github.io/dafr/reference/Max.md) : Max
  query operation
- [`Var()`](https://tanaylab.github.io/dafr/reference/Var.md) : Var
  query operation
- [`VarN()`](https://tanaylab.github.io/dafr/reference/VarN.md) : VarN
  query operation
- [`Std()`](https://tanaylab.github.io/dafr/reference/Std.md) : Std
  query operation
- [`StdN()`](https://tanaylab.github.io/dafr/reference/StdN.md) : StdN
  query operation
- [`Quantile()`](https://tanaylab.github.io/dafr/reference/Quantile.md)
  : Quantile query operation
- [`Count()`](https://tanaylab.github.io/dafr/reference/Count.md) :
  Count query operation
- [`GeoMean()`](https://tanaylab.github.io/dafr/reference/GeoMean.md) :
  GeoMean query operation
- [`Mode()`](https://tanaylab.github.io/dafr/reference/Mode.md) : Mode
  query operation
- [`CountBy()`](https://tanaylab.github.io/dafr/reference/CountBy.md) :
  CountBy query operation
- [`GroupBy()`](https://tanaylab.github.io/dafr/reference/GroupBy.md) :
  GroupBy query operation

## Query Operations - Logical

Logical operations for filtering and combining data

- [`And()`](https://tanaylab.github.io/dafr/reference/And.md) : And
  query operation
- [`AndNot()`](https://tanaylab.github.io/dafr/reference/AndNot.md) :
  AndNot query operation
- [`Or()`](https://tanaylab.github.io/dafr/reference/Or.md) : Or query
  operation
- [`OrNot()`](https://tanaylab.github.io/dafr/reference/OrNot.md) :
  OrNot query operation
- [`Xor()`](https://tanaylab.github.io/dafr/reference/Xor.md) : Xor
  query operation
- [`XorNot()`](https://tanaylab.github.io/dafr/reference/XorNot.md) :
  XorNot query operation
- [`IsEqual()`](https://tanaylab.github.io/dafr/reference/IsEqual.md) :
  IsEqual query operation
- [`IsGreater()`](https://tanaylab.github.io/dafr/reference/IsGreater.md)
  : IsGreater query operation
- [`IsGreaterEqual()`](https://tanaylab.github.io/dafr/reference/IsGreaterEqual.md)
  : IsGreaterEqual query operation
- [`IsLess()`](https://tanaylab.github.io/dafr/reference/IsLess.md) :
  IsLess query operation
- [`IsLessEqual()`](https://tanaylab.github.io/dafr/reference/IsLessEqual.md)
  : IsLessEqual query operation
- [`IsMatch()`](https://tanaylab.github.io/dafr/reference/IsMatch.md) :
  IsMatch query operation
- [`IsNotEqual()`](https://tanaylab.github.io/dafr/reference/IsNotEqual.md)
  : IsNotEqual query operation
- [`IsNotMatch()`](https://tanaylab.github.io/dafr/reference/IsNotMatch.md)
  : IsNotMatch query operation

## Query Operations - Selection

Operations for selecting data from axes and properties

- [`Axis()`](https://tanaylab.github.io/dafr/reference/Axis.md) : Axis
  query operation
- [`Lookup()`](https://tanaylab.github.io/dafr/reference/Lookup.md) :
  Lookup query operation
- [`Names()`](https://tanaylab.github.io/dafr/reference/Names.md) :
  Names query operation
- [`AsAxis()`](https://tanaylab.github.io/dafr/reference/AsAxis.md) :
  AsAxis query operation
- [`Fetch()`](https://tanaylab.github.io/dafr/reference/Fetch.md) :
  Fetch query operation
- [`IfMissing()`](https://tanaylab.github.io/dafr/reference/IfMissing.md)
  : IfMissing query operation
- [`IfNot()`](https://tanaylab.github.io/dafr/reference/IfNot.md) :
  IfNot query operation
- [`MaskSlice()`](https://tanaylab.github.io/dafr/reference/MaskSlice.md)
  : MaskSlice query operation
- [`SquareMaskColumn()`](https://tanaylab.github.io/dafr/reference/SquareMaskColumn.md)
  : SquareMaskColumn query operation
- [`SquareMaskRow()`](https://tanaylab.github.io/dafr/reference/SquareMaskRow.md)
  : SquareMaskRow query operation

## Views and Adapters

Creating views and adapters for Daf objects

- [`adapter()`](https://tanaylab.github.io/dafr/reference/adapter.md) :
  Invoke a computation on a view of some data set and return the result
- [`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) :
  Create a read-only view of a Daf data set
- [`reconstruct_axis()`](https://tanaylab.github.io/dafr/reference/reconstruct_axis.md)
  : Reconstruct implicit axes

## Contracts

Contract-based validation for Daf objects

- [`RequiredInput`](https://tanaylab.github.io/dafr/reference/RequiredInput.md)
  [`OptionalInput`](https://tanaylab.github.io/dafr/reference/RequiredInput.md)
  [`GuaranteedOutput`](https://tanaylab.github.io/dafr/reference/RequiredInput.md)
  [`OptionalOutput`](https://tanaylab.github.io/dafr/reference/RequiredInput.md)
  : Contract Expectation Types
- [`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md)
  : Create a Contract specification
- [`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md)
  : Create a contract-aware wrapper for a Daf
- [`contract_docs()`](https://tanaylab.github.io/dafr/reference/contract_docs.md)
  : Convert contract to documentation string
- [`verify_contract()`](https://tanaylab.github.io/dafr/reference/verify_contract.md)
  : Verify a DAF object against a contract
- [`verify_input()`](https://tanaylab.github.io/dafr/reference/verify_input.md)
  : Verify DAF input against contract
- [`verify_output()`](https://tanaylab.github.io/dafr/reference/verify_output.md)
  : Verify DAF output against contract
- [`print(`*`<DafContract>`*`)`](https://tanaylab.github.io/dafr/reference/print.DafContract.md)
  : Print method for DafContract
- [`scalar_contract()`](https://tanaylab.github.io/dafr/reference/scalar_contract.md)
  : Create a scalar contract specification
- [`vector_contract()`](https://tanaylab.github.io/dafr/reference/vector_contract.md)
  : Create a vector contract specification
- [`matrix_contract()`](https://tanaylab.github.io/dafr/reference/matrix_contract.md)
  : Create a matrix contract specification
- [`tensor_contract()`](https://tanaylab.github.io/dafr/reference/tensor_contract.md)
  : Create a tensor contract specification

## Groups

Functions for working with groups of axis entries

- [`group_names()`](https://tanaylab.github.io/dafr/reference/group_names.md)
  : Generate names for groups of axis entries
- [`compact_groups()`](https://tanaylab.github.io/dafr/reference/compact_groups.md)
  : Compact group indices
- [`collect_group_members()`](https://tanaylab.github.io/dafr/reference/collect_group_members.md)
  : Collect group members from group indices

## Data Operations

Operations for transforming and combining Daf objects

- [`concatenate()`](https://tanaylab.github.io/dafr/reference/concatenate.md)
  : Concatenate multiple Daf data sets along some axis
- [`copy_all()`](https://tanaylab.github.io/dafr/reference/copy_all.md)
  : Copy all content from source to destination
- [`copy_axis()`](https://tanaylab.github.io/dafr/reference/copy_axis.md)
  : Copy an axis from source to destination
- [`copy_matrix()`](https://tanaylab.github.io/dafr/reference/copy_matrix.md)
  : Copy a matrix from source to destination
- [`copy_scalar()`](https://tanaylab.github.io/dafr/reference/copy_scalar.md)
  : Copy a scalar from source to destination
- [`copy_tensor()`](https://tanaylab.github.io/dafr/reference/copy_tensor.md)
  : Copy a tensor from source to destination
- [`copy_vector()`](https://tanaylab.github.io/dafr/reference/copy_vector.md)
  : Copy a vector from source to destination

## Utilities

Utility functions for working with Daf objects

- [`empty_cache()`](https://tanaylab.github.io/dafr/reference/empty_cache.md)
  : Empty cache in a Daf object
- [`set_seed()`](https://tanaylab.github.io/dafr/reference/set_seed.md)
  : Set a seed both in Julia and R
- [`setup_logger()`](https://tanaylab.github.io/dafr/reference/setup_logger.md)
  : Set up a global logger
