#' @importFrom methods setGeneric
NULL
# TODO: use ...?

#' To extract features of an object
#'
#' Extracts features of an object given a feature representation.
#'
#' @keywords methods
#' @param object See: \code{\link{StrctFeatureRepresentation}} and \code{\link{SrfFeatureRepresentation}}.
#' @param FEATURES_EXTRACTION_PARAMETERS See: \code{\link{FeaturesExtractionParameters}}.
#' @param OBJECT_ID_I The \emph{object_ID} of the object.
#' @param OBJECT_OUTPUT_DIR_PATH_STR The directory path for output about the object.
#' @param RELATIVE_FILE_PATHS_CVEC The file paths relative to the documents root directory that collectively represent the object.
#' @return \code{OBJECT_FEATURES_DVEC} Object features. May be \code{DUMMY_OBJECT_FEATURES_DVEC}, a single zero score for the feature named after the empty string. This is the case when from one or more objects no structural features could be extracted given the feature representation parameters.
#' @name extract_features
#' @export
setGeneric(
    name='extract_features',
    def=function(
        object,
        FEATURES_EXTRACTION_PARAMETERS,
        OBJECT_ID_I,
        OBJECT_OUTPUT_DIR_PATH_STR,
        RELATIVE_FILE_PATHS_CVEC)

        base::standardGeneric('extract_features'))