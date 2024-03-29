% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strat_cont_gen_from_tracer.R
\name{strat_cont_gen_from_tracer}
\alias{strat_cont_gen_from_tracer}
\title{proxy values in strat domain}
\usage{
strat_cont_gen_from_tracer(
  bin_borders,
  df,
  distribution = "normal",
  cap = TRUE,
  cap_val = 0
)
}
\arguments{
\item{bin_borders}{borders of sampling bins}

\item{df}{data frame with proxy records}

\item{distribution}{character, currently only "normal" implemented. Specifies the distribution of proxies}

\item{cap}{logical. Should values below \code{cap_val}be replaced?}

\item{cap_val}{numeric. If \code{cap = TRUE}, values below \code{cap_val}will be replaced by \code{cap_val}}
}
\value{
a functional for usage with strat_cont_to_multiadm
}
\description{
Generates a function factory for usage with \emph{strat_cont_to_multiadm} based on empirical tracer measurements in the section
}
\examples{
\dontrun{
# see this vignette for a use case
vignette("adm_from_trace_cont")
}
}
\seealso{
\code{\link[=flux_const]{flux_const()}}, \code{\link[=flux_linear]{flux_linear()}}, \code{\link[=flux_quad]{flux_quad()}} to define tracer fluxes
}
