# dafr: Data in Axes in Formats (DAF) for R

A native R implementation of the 'DataAxesFormats.jl'
<https://github.com/tanaylab/DataAxesFormats.jl> data model, providing a
uniform interface for accessing 1D and 2D data arranged along arbitrary
axes. This generalizes and extends 'AnnData'-like functionality,
supporting both scalars and multiple data modalities (e.g. RNA-seq,
ATAC-seq), efficient memory management via memory-mapped reads, and a
composable query DSL. Unlike the 'dafJuliaWrapper'
<https://github.com/tanaylab/dafJuliaWrapper> interface, 'dafr' has no
'Julia' dependency; storage, query evaluation, and reductions are
implemented directly in R and C++ with optional 'OpenMP' parallelism.
The package ships in-memory ('MemoryDaf') and file-backed ('FilesDaf')
storage, an 'AnnData' facade with h5ad read/write, computation contracts
for pre/post-condition validation, and a 'dplyr' backend for tidy-verbs
access to axis data.

## See also

Useful links:

- <https://tanaylab.github.io/dafr/>

- <https://github.com/tanaylab/dafr>

- Report bugs at <https://github.com/tanaylab/dafr/issues>

## Author

**Maintainer**: Aviezer Lifshitz <aviezer.lifshitz@weizmann.ac.il>
([ORCID](https://orcid.org/0000-0002-8458-9507))

Authors:

- Oren Ben-Kiki <oren@ben-kiki.org>

Other contributors:

- Weizmann Institute of Science \[copyright holder\]
