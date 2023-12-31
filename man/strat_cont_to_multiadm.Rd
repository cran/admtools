% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strat_cont_to_multiadm.R
\name{strat_cont_to_multiadm}
\alias{strat_cont_to_multiadm}
\title{estimate age-depth model from stratigraphic contents}
\usage{
strat_cont_to_multiadm(
  h_tp,
  t_tp,
  strat_cont_gen,
  time_cont_gen,
  h,
  no_of_rep = 100
)
}
\arguments{
\item{h_tp}{function generating height tie poitns}

\item{t_tp}{function generating time tie points}

\item{strat_cont_gen}{function, generating stratigraphic contents}

\item{time_cont_gen}{function, generating the hypothesis on content input in time}

\item{h}{heights where the adm is evaluated}

\item{no_of_rep}{integer, number of repetititons}
}
\value{
Object of class multiadm
}
\description{
estimate age-depth model from stratigraphic contents
}
\examples{
\dontrun{
# see this vignette for an example
vignette("adm_from_trace_cont")
}

}
