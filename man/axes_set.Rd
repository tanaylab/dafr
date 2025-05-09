% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{axes_set}
\alias{axes_set}
\title{Get set of axis names from a Daf object}
\usage{
axes_set(daf)
}
\arguments{
\item{daf}{A Daf object}
}
\value{
A character vector of axis names
}
\description{
Returns the names of all axes in the Daf data set.
}
\details{
This function provides the complete set of available axes in the Daf data set.
Common axis names might include "gene", "cell", "batch", etc., depending on the data.
See the Julia \href{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axes_set}{documentation} for details.
}
