% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot_tools.R
\name{label_significance}
\alias{label_significance}
\title{Label Statistical Significance}
\usage{
label_significance(
  results,
  p_col = "p",
  alpha = 0.05,
  label_values = c(0.001, 0.01, 0.05),
  ns_label = "NS"
)
}
\arguments{
\item{results}{Results data frame}

\item{p_col}{Column name containing p-values}

\item{alpha}{Significance threshold}

\item{label_values}{Vector of p-value cutpoints to label}

\item{ns_label}{Label for non-significant results}
}
\value{
Data frame with added significance labels
}
\description{
Label Statistical Significance
}
