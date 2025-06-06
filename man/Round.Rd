% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operations.R
\name{Round}
\alias{Round}
\title{Round query operation}
\usage{
Round(...)
}
\arguments{
\item{...}{Additional arguments needed to support usage of pipe operator}
}
\value{
A query operation object that can be used in a query sequence
}
\description{
Element-wise operation that converts every element to the nearest integer value.
This operation preserves the shape of the data (scalar, vector, or matrix) but
rounds each value to the nearest integer. See the Julia
\href{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Round}{documentation} for
details.
}
