% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sed_rate_t.R
\name{sed_rate_t}
\alias{sed_rate_t}
\title{sedimentation rate in time domain}
\usage{
sed_rate_t(x, t, mode = "rcll")
}
\arguments{
\item{x}{adm or multiadm object}

\item{t}{vector of times at which sedimentation rates are determined}

\item{mode}{string, "rcll" or "lcrl". at non-differential points, is the sed rate left or right continuous?}
}
\value{
for adm objects, a vector giving sed. accumulation rates at time t. For multiadm objects, a list with accumulation rates
}
\description{
infers the instantaneous sedimentation rate from adm objects
}
