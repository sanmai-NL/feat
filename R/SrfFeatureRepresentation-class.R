#' @include FeatureRepresentation-class.R
NULL

# TODO: validate
#' Surface ('SRF') feature representation
#'
#' @concept surface
#' @slot NAME_STR \emph{private} \code{'SRF'}
#' @slot N_I The maximum value \eqn{N} for which \eqn{n}-grams will be extracted from object string sequences, with \eqn{n \in \{1,...,N\}}.
#' @slot OBJECT_SEPARATOR_STR The separator used when collapsing strings representing single observations into object string sequences (e.g., texts).
#' @slot SRILM_NGRAMCOUNT_PARAMETERS_CVEC Extra command line parameters to SRILM \emph{ngram-count}. This utility is run to extract features.
#' @slot SRILM_NGRAM_PARAMETERS_CVEC Extra command line parameters to SRILM \emph{ngram}. This command is run to calculate feature scores.
#' @slot XPATH_STR XPath used to extract the surface strings in the XML documents that represent observations. It ought to point to a text node.
#' @export SrfFeatureRepresentation
#' @exportClass SrfFeatureRepresentation
SrfFeatureRepresentation <-
    methods::setClass(
        Class='SrfFeatureRepresentation',
        slots=base::list(
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D='numeric',
            N_I='integer',
            OBJECT_SEPARATOR_STR='character',
            SCORING_STR='character',
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC='character',
            SRILM_NGRAM_PARAMETERS_CVEC='character',
            XPATH_STR='character'),
        prototype=base::list(
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D=NULL,
            N_I=NULL,
            OBJECT_SEPARATOR_STR=NULL,
            SCORING_STR=NULL,
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC=NULL,
            SRILM_NGRAM_PARAMETERS_CVEC=NULL,
            XPATH_STR=NULL,
            NAME_STR='SRF'),
        contains='FeatureRepresentation',
        sealed=TRUE)