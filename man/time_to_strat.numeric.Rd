% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_to_strat.numeric.R
\name{time_to_strat.numeric}
\alias{time_to_strat.numeric}
\title{transform vectors from time to height domain}
\usage{
\method{time_to_strat}{numeric}(obj, x, ...)
}
\arguments{
\item{obj}{a numeric vector, interpreted as timing of events}

\item{x}{an \emph{adm} object}

\item{...}{options passed to \emph{get_height}}
}
\value{
a numeric vector - stratigraphic position of the events
}
\description{
This function transforms numeric vectors from the time to the stratigraphic domain
Fundamentally a wrapper around get_height for consistent syntax
}
\examples{
# see vignette("admtools") for an example

}
\seealso{
\code{\link[=strat_to_time.numeric]{strat_to_time.numeric()}} for the transformation from height to time domain, \code{\link[=time_to_strat.phylo]{time_to_strat.phylo()}} and \code{\link[=time_to_strat.list]{time_to_strat.list()}} for transformations of phylogenetic trees and lists. See \code{\link[=get_height]{get_height()}} for the underlying procedure.
}
