% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handlers.R
\name{inefficient_action_handler}
\alias{inefficient_action_handler}
\title{Set the handler for inefficient matrix access}
\usage{
inefficient_action_handler(handler)
}
\arguments{
\item{handler}{The handler to use, one of "IgnoreHandler", "WarnHandler", or "ErrorHandler"}
}
\value{
The previous handler name
}
\description{
Specify the handler to use when accessing a matrix in an inefficient way ("against the grain").
Returns the previous handler as a string.
}
