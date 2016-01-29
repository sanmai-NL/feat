#' @include ArcCategories-class.R FeatureRepresentation-class.R
NULL

# TODO: validity=function(object) all(lengths(attributes(object)) > 0L),
# TODO: use ....
# TODO: value of ARC_CATEGORIES_CVEC?
#' Structural ('STRCT') feature representation
#'
#' @concept structural
#' @slot ARC_CATEGORIES \emph{private} \link{ArcCategories} read based on \code{ARC_CATEGORIES_CVEC} and \code{ARC_CATEGORY_TABLES_DIR_PATH}.
#' @slot ALL_NGRAMS_UP_TO_N_I_B Extract all \eqn{n}-grams with \eqn{n \in \{1,...,N\}}? If false, extract all \eqn{N}-grams with \eqn{N=}\code{N_I}.
#' @slot NAME_STR \emph{private} \code{'STRCT'}.
#' @slot N_I A scalar integer \eqn{N}.
#' @slot M_I How long should trails be, at most? If the current arc in the current trail goes out to a terminal vertex yet has not reached this length, the trail is accumulated as is.
#' @slot I_I The weight to scale the number of iterations taken for random sampling of trails in a linguistic network by, a multiplier of the number of arcs in the network.
#' @slot SRILM_NGRAMCOUNT_PARAMETERS_CVEC Extra command line parameters to SRILM \emph{ngram-count}. This utility is run to extract features.
#' @slot SRILM_NGRAM_PARAMETERS_CVEC Extra command line parameters to SRILM \emph{ngram}. This utility is run to calculate feature scores.
#' @slot XPATH_STR \emph{XPath} expression used to extract the surface strings in the XML documents that represent observations. It ought to point to a text node, and must be written in a syntax version supported by \emph{libxml2}.
#' @export StrctFeatureRepresentation
#' @exportClass StrctFeatureRepresentation
StrctFeatureRepresentation <-
    methods::setClass(
        Class='StrctFeatureRepresentation',
        slots=base::list(
            ALL_NGRAMS_UP_TO_N_I_B='logical',
            ARC_CATEGORIES='ArcCategories',
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D='numeric',
            N_I='integer',
            M_I='integer',
            I_I='integer',
            SCORING_STR='character',
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC='character',
            SRILM_NGRAM_PARAMETERS_CVEC='character',
            XPATH_STR='character'),
        prototype=base::list(
            ALL_NGRAMS_UP_TO_N_I_B=NULL,
            ARC_CATEGORIES=NULL,
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D=NULL,
            N_I=NULL,
            M_I=NULL,
            I_I=NULL,
            SCORING_STR=NULL,
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC=NULL,
            SRILM_NGRAM_PARAMETERS_CVEC=NULL,
            XPATH_STR=NULL,
            NAME_STR='STRCT'),
        contains='FeatureRepresentation',
        sealed=TRUE)