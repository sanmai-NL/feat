# TODO: validate
#' A virtual class for feature representations
#'
#' @export FeatureRepresentation
#' @exportClass FeatureRepresentation
FeatureRepresentation <-
    methods::setClass(
        Class='FeatureRepresentation',
        slots=base::list(
            NAME_STR='character'),
        prototype=base::list(NAME_STR=NULL),
        contains='VIRTUAL',
        sealed=TRUE)