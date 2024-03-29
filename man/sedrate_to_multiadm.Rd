% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sedrate_to_multiadm.R
\name{sedrate_to_multiadm}
\alias{sedrate_to_multiadm}
\title{Estimate age-depth model from sedimentation rates & tie points}
\usage{
sedrate_to_multiadm(
  h_tp,
  t_tp,
  sed_rate_gen,
  h,
  no_of_rep = 100L,
  subdivisions = 100L,
  stop.on.error = TRUE,
  T_unit = NULL,
  L_unit = NULL
)
}
\arguments{
\item{h_tp}{: function, returns stratigraphic positions of tie points}

\item{t_tp}{: function, returns times of tie points}

\item{sed_rate_gen}{: function, returns function describing sedimentation rate}

\item{h}{: numeric, heights where the adm is calculated}

\item{no_of_rep}{: integer, number of repetitions}

\item{subdivisions}{maximum no of subintervals used in numeric integration. passed to \emph{integrate}, see ?stats::integrate for details}

\item{stop.on.error}{logical passed to \emph{integrate}, see ?stats::integrate for details}

\item{T_unit}{time unit}

\item{L_unit}{length unit}
}
\value{
object of class multiadm
}
\description{
Combines information on tie points and sedimentation rates to estimate age-depth models and their associated uncertainty. For an example, see \code{vignette("adm_from_sedrate")}.
}
\examples{
\dontrun{
# see this vignette for an example
vignette("adm_from_sedrate")
}

}
