% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{Lookup}
\alias{Lookup}
\title{Lookup query operation}
\usage{
Lookup(property, ...)
}
\arguments{
\item{property}{String specifying the property name to look up}

\item{...}{Additional arguments needed to support usage of pipe operator}
}
\value{
A query operation object that can be used in a query sequence
}
\description{
A query operation for looking up the value of a property with a specific name.
This operation retrieves the data associated with the specified property for the current axis context.
It is typically used after an \code{Axis} operation to select a property from that axis.
See the Julia \href{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Lookup}{documentation}
for details.
}
