% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reconstruction.R
\name{reconstruct_axis}
\alias{reconstruct_axis}
\title{Reconstruct implicit axes}
\usage{
reconstruct_axis(
  daf,
  existing_axis,
  implicit_axis,
  rename_axis = NULL,
  empty_implicit = NULL,
  implicit_properties = NULL,
  skipped_properties = NULL
)
}
\arguments{
\item{daf}{A Daf object}

\item{existing_axis}{Name of the existing axis containing implicit axis data}

\item{implicit_axis}{Name of the property in existing_axis that contains implicit axis values}

\item{rename_axis}{Optional new name for the implicit axis (default: NULL, uses implicit_axis name)}

\item{empty_implicit}{Value to consider as empty/NA in the implicit axis values (default: NULL)}

\item{implicit_properties}{Optional set of properties to copy (default: NULL for all properties)}

\item{skipped_properties}{Optional set of properties to skip (default: NULL for none)}
}
\value{
A named list of properties and their associated values
}
\description{
Given an existing axis with a property representing an implicit axis,
create a new axis. See the Julia
\href{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/reconstruction.html}{documentation} for details.
}
