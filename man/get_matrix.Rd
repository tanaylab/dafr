% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_matrix}
\alias{get_matrix}
\title{Get matrix from a Daf object}
\usage{
get_matrix(daf, rows_axis, columns_axis, name, default = NULL, relayout = TRUE)
}
\arguments{
\item{daf}{A Daf object}

\item{rows_axis}{Name of rows axis}

\item{columns_axis}{Name of columns axis}

\item{name}{Name of the matrix property}

\item{default}{Default value if matrix doesn't exist (NULL by default)}

\item{relayout}{Whether to allow retrieving matrix with flipped axes (TRUE by default)}
}
\value{
A matrix with row and column names set to the axis entry names,
or the default value if the property doesn't exist
}
\description{
Retrieves a matrix property with the specified name for the given axes from the Daf data set.
}
\details{
Matrix properties store two-dimensional data along two axes.
If the matrix doesn't exist and default is NA, a matrix of NAs with appropriate dimensions is returned.
If \code{relayout} is TRUE and the matrix exists with flipped axes, it will be transposed automatically.
See the Julia \href{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.get_matrix}{documentation} for details.
}
