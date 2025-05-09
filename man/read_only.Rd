% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{read_only}
\alias{read_only}
\title{Create a read-only wrapper for a Daf object}
\usage{
read_only(daf, name = NULL)
}
\arguments{
\item{daf}{A Daf object}

\item{name}{Optional name for the read-only wrapper (defaults to the original name)}
}
\value{
A read-only Daf object
}
\description{
Creates a read-only view of a Daf object to protect it against accidental modification.
}
\details{
This function wraps a Daf object with a read-only interface to protect against
accidental modification. Any attempt to modify the data will result in an error.
The read-only wrapper can be efficiently created as it shares data with the original object.
See the Julia \href{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/read_only.html#DataAxesFormats.ReadOnly.read_only}{documentation} for details.
}
