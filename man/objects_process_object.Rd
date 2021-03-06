% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objects.R
\name{objects_process_object}
\alias{objects_process_object}
\title{To process an object}
\usage{
objects_process_object(OBJECT_ID_I, DT, FEATURES_EXTRACTION_PARAMETERS,
  FEATURE_REPRESENTATION, OBJECT_OUTPUT_DIR_PATH_STR)
}
\arguments{
\item{OBJECT_ID_I}{The identifier of an object, as in the \emph{object_ID} column of \code{DT}.}

\item{DT}{The annotations table as a \code{\link[data.table]{data.table}}. See: \code{\link{Annotations}}.}

\item{FEATURES_EXTRACTION_PARAMETERS}{See: \code{\link{FeaturesExtractionParameters}}.}

\item{FEATURE_REPRESENTATION}{See: \code{\link{FeatureRepresentation}}.}

\item{OBJECT_OUTPUT_DIR_PATH_STR}{The directory path for output about the object.}
}
\value{
\code{OBJECT_FEATURES_DVEC} Feature scores, with named elements.
}
\description{
To process an object
}
\keyword{file}

