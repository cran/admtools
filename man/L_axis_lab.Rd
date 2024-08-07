% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/L_axis_lab.R
\name{L_axis_lab}
\alias{L_axis_lab}
\title{plot height axis label}
\usage{
L_axis_lab(
  label = "Height",
  unit = TRUE,
  sep = " ",
  brac = c("[", "]"),
  line = 2,
  outer = FALSE,
  at = NA,
  adj = NA,
  padj = NA,
  cex = NA,
  col = NA,
  font = NA,
  ...
)
}
\arguments{
\item{label}{Axis label}

\item{unit}{Logical or character, should unit be plotted}

\item{sep}{separator between label and unit}

\item{brac}{brackets surrounding unit}

\item{line}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{outer}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{at}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{adj}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{padj}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{cex}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{col}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{font}{parameter passed to \emph{mtext}, see ?mtext for details}

\item{...}{further graphical parameters passed to \emph{mtext}, see ?mtext for details}
}
\value{
invisible NULL
}
\description{
plot height axis label
}
\seealso{
\code{\link[=plot.adm]{plot.adm()}} for plotting of adms
}
