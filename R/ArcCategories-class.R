# TODO: validate
#' A set of arc category sets
#'
#' @concept structural
#' @slot DT_LST A list of \code{\link[data.table]{data.table}} objects, one for each arc category, with rows describing arc labels and the first column being the arc label (i.e., its name) and the second a description. This table in essence descibes an enumeration. It is necessary that the arc labels do not contain whitespace, and it is advisable to use very brief arc labels in order to keep space requirements down.
#' @slot PRIOR_PROBABILITY_DVEC Ordered by \code{DT_LST}, for each arc category the prior probability of observing an arc labeled by any of the arc labels in this arc category. It is assumed to be the uniform probability.
#' @export ArcCategories
#' @exportClass ArcCategories
ArcCategories <-
    methods::setClass(
        Class='ArcCategories',
        slots=base::list(
            DT_LST='list',
            PRIOR_PROBABILITY_DVEC='numeric'),
        prototype=base::list(
            DT_LST=NULL,
            PRIOR_PROBABILITY_DVEC=NULL),
        sealed=TRUE)