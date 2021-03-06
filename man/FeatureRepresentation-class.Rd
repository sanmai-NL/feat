% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureRepresentation-class.R
\docType{class}
\name{FeatureRepresentation-class}
\alias{FeatureRepresentation}
\alias{FeatureRepresentation-class}
\title{A virtual class for feature representations}
\description{
A virtual class for feature representations
}
\section{Slots}{

\describe{
\item{\code{DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D}}{The fraction of the number of singular vectors (\eqn{m \times k} with \eqn{k = (min(n, m) - 1) *}\code{DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D}) to keep after having applied Singular Value Decomposition (SVD). A value of \code{1.0} implies not to apply SVD. Sensible values are in the interval \eqn{(0, 1]}.}

\item{\code{SCORING_STR}}{Either 'boolean' for boolean scores or 'Pr' for probability estimates.}
}}

