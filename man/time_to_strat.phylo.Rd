% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_to_strat.phylo.R
\name{time_to_strat.phylo}
\alias{time_to_strat.phylo}
\title{transform phylo object}
\usage{
\method{time_to_strat}{phylo}(obj, x, ...)
}
\arguments{
\item{obj}{the phylo object to be transformed}

\item{x}{age-depth model}

\item{...}{other parameters, currently ignored}
}
\value{
a phylo object, representation of the tree in the strat domain
}
\description{
transform phylo object from the time domain to the stratigraphic domain
}
\seealso{
\code{\link[=get_height]{get_height()}} for the underlying procedure, \code{\link[=time_to_strat]{time_to_strat()}} for the higher level function, and \code{\link[=strat_to_time.phylo]{strat_to_time.phylo()}} for the transformation of phylo objects from strat domain to the time domain. See \code{\link[=time_to_strat.list]{time_to_strat.list()}} and \code{\link[=time_to_strat.numeric]{time_to_strat.numeric()}} for the transformation of lists and numeric vectors
}
