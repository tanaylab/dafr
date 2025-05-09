% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{set_seed}
\alias{set_seed}
\title{Set a seed both in Julia and R}
\usage{
set_seed(seed)
}
\arguments{
\item{seed}{seed to be used}
}
\value{
No return value, called for side effects.
}
\description{
Set a seed both in Julia and R
}
\examples{
\dontrun{
## Needs previous call to `setup_daf` which is time
## consuming and requires Julia
setup_daf(installJulia = TRUE, seed = 123)
set_seed(123)
}
}
