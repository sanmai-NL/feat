# TODO: validate
#' A virtual class for feature representations
#'
#' @slot DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D The fraction of the number of singular vectors (\eqn{m \times k} with \eqn{k = (min(n, m) - 1) *}\code{DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D}) to keep after having applied Singular Value Decomposition (SVD). A value of \code{1.0} implies not to apply SVD. Sensible values are in the interval \eqn{(0, 1]}.
#' @slot SCORING_STR Either 'boolean' for boolean scores or 'Pr' for probability estimates.
#' @export FeatureRepresentation
#' @exportClass FeatureRepresentation
FeatureRepresentation <-
    methods::setClass(
        Class='FeatureRepresentation',
        slots=base::list(
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D='numeric',
            NAME_STR='character',
            SCORING_STR='character'),
        prototype=base::list(
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D=NULL,
            NAME_STR=NULL,
            SCORING_STR=NULL),
        contains='VIRTUAL',
        sealed=TRUE)