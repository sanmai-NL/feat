#' A design matrix
#'
#' @slot DMAT A design matrix with feature scores, with objects as rows indexed by their \emph{object_ID} and named columns as features. May contain a column named the empty string, with all-zero values (\code{DUMMY_OBJECT_FEATURES_DVEC} in the code). This is the case when from one or more objects no structural features could be extracted given the feature representation parameters.
#' @slot LOWER_RANK_APPROXIMATION_DMAT A lower rank approximation of \code{DMAT}, based on \code{SVD}. See: \code{\link{FeatureRepresentation}}.
#' @slot SVD A \emph{Singular Value Decomposition} (SVD) of \code{DMAT} as returned by \code{\link[base]{svd}}. Will be \code{NULL} if SVD has not been applied (\code{\link{FeatureRepresentation}}).
#' @importClassesFrom Matrix Matrix
#' @export DesignMatrix
#' @exportClass DesignMatrix
DesignMatrix <-
    methods::setClass(
        Class='DesignMatrix',
        slots=base::list(
            DMAT='dMatrix',
            LOWER_RANK_APPROXIMATION_DMAT='dMatrix',
            SVD='list'),
        prototype=base::list(
            DMAT=NULL,
            LOWER_RANK_APPROXIMATION_DMAT=NULL,
            SVD=NULL),
        sealed=TRUE)