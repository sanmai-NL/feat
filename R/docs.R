#' To process documents.
#'
#' @name feat-docs
NULL

ALL_SEGMENT_TYPES <-
    'all'
FEATURE_REPRESENTATIONS_CVEC <-
    base::c('FLATSEQ', 'STRCT')

methods::setClass(
    'ObjectFeatureSet',
    methods::representation(
        COUNTS_AND_LM='CountsAndLM',
        SEQUENCES_AND_SCORES='SequencesAndScores'),
    sealed=TRUE)

methods::setClass(
    'ObjectFeatureSets',
    methods::representation(
        FLATSEQ='ObjectFeatureSet',
        STRCT='ObjectFeatureSet'),
    sealed=TRUE)

#' @export
docs_parse_Alpino_XML <- function(ALPINO_XML_FILE_PATH_STR=NULL) {
    check_args(fun=docs_parse_Alpino_XML)

    XML_DOCUMENT <-
        xml2::read_xml(ALPINO_XML_FILE_PATH_STR)
    XML_DOCUMENT_LST <-
        base::list(XML_DOCUMENT)

    return(XML_DOCUMENT_LST)
}

docs_process_object <- function(SENTENCE_COLLAPSING_FML_INDEX_I, ARC_CATEGORIES=NULL, DOCUMENT_ANNOTATION_DT=NULL, DOCUMENT_DIR_PATH_STR=NULL, FLATSEQ_SRILM_PARAMETERS=NULL, SEED_I=NULL, SENTENCE_COLLAPSING_FML=SENTENCE_COLLAPSING_FML_LST[[SENTENCE_COLLAPSING_FML_INDEX_I]], SENTENCE_COLLAPSING_FML_LST=NULL, STRCT_N=NULL, STRCT_SRILM_PARAMETERS=NULL) {
    check_args(fun=docs_process_object)
    if (base::is.na(STRCT_SRILM_PARAMETERS) && base::is.na(FLATSEQ_SRILM_PARAMETERS)) {
        base::stop('FLATSEQ_SRILM_PARAMETERS and STRCT_SRILM_PARAMETERS may not both be NA-like. ')
    }

    base::message(
        base::sprintf(
            "Processing sentences collapsed to a single object using formula '%s' ... ",
            base::deparse(SENTENCE_COLLAPSING_FML)))
    SEGMENT_SENTENCE_IDS_ANNOTATION_DT <-
        dplyr::select(
            dplyr::filter_(
                DOCUMENT_ANNOTATION_DT,
                SENTENCE_COLLAPSING_FML),
            sentence_ID)

    if (base::nrow(SEGMENT_SENTENCE_IDS_ANNOTATION_DT) > 0L) {
        SENTENCE_FILE_NAMES_CVEC <-
            base::sprintf(
                '%d.xml',
                SEGMENT_SENTENCE_IDS_ANNOTATION_DT[[1L]])
        ALPINO_XML_FILES_PATHS_CVEC <-
            base::file.path(
                DOCUMENT_DIR_PATH_STR,
                SENTENCE_FILE_NAMES_CVEC)
        ALPINO_XML_DOC_LST <-
            base::vapply(
                ALPINO_XML_FILES_PATHS_CVEC,
                FUN=docs_parse_Alpino_XML,
                FUN.VALUE=TEMPLATES_1_LST,
                USE.NAMES=FALSE)

        if (!base::anyNA(FLATSEQ_SRILM_PARAMETERS)) {
            FLATSEQ_COUNTS_AND_LM <-
                flatseq_extract(
                    ALPINO_XML_DOC_LST=ALPINO_XML_DOC_LST,
                    OUTPUT_DIR_PATH_STR=DOCUMENT_DIR_PATH_STR,
                    SENTENCE_COLLAPSING_FML_INDEX_I=SENTENCE_COLLAPSING_FML_INDEX_I,
                    SRILM_PARAMETERS=FLATSEQ_SRILM_PARAMETERS)

            FLATSEQ_SEQUENCES_AND_SCORES <-
                SRILM_write_feature_scores(
                    COUNTS_AND_LM=FLATSEQ_COUNTS_AND_LM,
                    SEED_I=SEED_I)

            FLATSEQ_SEGMENT_FEATURE_SET <-
                methods::new(
                    'ObjectFeatureSet',
                    COUNTS_AND_LM=FLATSEQ_COUNTS_AND_LM,
                    SEQUENCES_AND_SCORES=FLATSEQ_SEQUENCES_AND_SCORES)
        }

        if (!base::anyNA(STRCT_SRILM_PARAMETERS)) {
            STRCT_COUNTS_AND_LM <-
                strct_extract(
                    ALPINO_XML_DOC_LST=ALPINO_XML_DOC_LST,
                    ARC_CATEGORIES=ARC_CATEGORIES,
                    N=STRCT_N,
                    OUTPUT_DIR_PATH_STR=DOCUMENT_DIR_PATH_STR,
                    SENTENCE_COLLAPSING_FML_INDEX_I=SENTENCE_COLLAPSING_FML_INDEX_I,
                    SRILM_PARAMETERS=STRCT_SRILM_PARAMETERS)

            STRCT_SEQUENCES_AND_SCORES <-
                SRILM_write_feature_scores(
                    COUNTS_AND_LM=STRCT_COUNTS_AND_LM,
                    SEED_I=SEED_I)

            STRCT_SEGMENT_FEATURE_SET <-
                methods::new(
                    'ObjectFeatureSet',
                    COUNTS_AND_LM=STRCT_COUNTS_AND_LM,
                    SEQUENCES_AND_SCORES=STRCT_SEQUENCES_AND_SCORES)
            SEGMENT_FEATURE_SETS <-
                base::list(STRCT_SEGMENT_FEATURE_SET)
        }

        if (base::anyNA(STRCT_SRILM_PARAMETERS)) {
            return(base::list(FLATSEQ_SEGMENT_FEATURE_SET))
        } else if (base::anyNA(FLATSEQ_SRILM_PARAMETERS)) {
            return(base::list(STRCT_SEGMENT_FEATURE_SET))
        } else {
            SEGMENT_FEATURE_SETS <-
                methods::new(
                    'ObjectFeatureSets',
                    FLATSEQ=FLATSEQ_SEGMENT_FEATURE_SET,
                    STRCT=STRCT_SEGMENT_FEATURE_SET)

            return(base::list(SEGMENT_FEATURE_SETS))
        }
    } else {
        ## If no sentences match SENTENCE_COLLAPSING_FML, return None result.
        return(TEMPLATES_1_LST)
    }
}

docs_process_document <- function(DOCUMENT_NAME=NULL, ANNOTATION_DT=NULL, ARC_CATEGORIES=NULL, DOCUMENT_DIR_PATH_STR=DOCUMENT_DIR_PATHS_CVEC[DOCUMENT_NAME], DOCUMENT_DIR_PATHS_CVEC=NULL, FLATSEQ_SRILM_PARAMETERS=NULL, SEED_I=NULL, SENTENCE_COLLAPSING_FML_LST=NULL, STRCT_N=NULL, STRCT_SRILM_PARAMETERS=NULL) {
    check_args(fun=docs_process_document)

    base::message(
        base::sprintf(
            "Processing document '%s' ... ",
            DOCUMENT_NAME))

    DOCUMENT_ANNOTATION_DT <-
        dplyr::filter(ANNOTATION_DT, document_name == DOCUMENT_NAME)

    SEGMENT_FEATURE_SETS_LST <-
        base::vapply(
            base::seq_along(SENTENCE_COLLAPSING_FML_LST),
            FUN=docs_process_object,
            DOCUMENT_ANNOTATION_DT=DOCUMENT_ANNOTATION_DT,
            DOCUMENT_DIR_PATH_STR=DOCUMENT_DIR_PATH_STR,
            ARC_CATEGORIES=ARC_CATEGORIES,
            FLATSEQ_SRILM_PARAMETERS=FLATSEQ_SRILM_PARAMETERS,
            STRCT_SRILM_PARAMETERS=STRCT_SRILM_PARAMETERS,
            SENTENCE_COLLAPSING_FML_LST=SENTENCE_COLLAPSING_FML_LST,
            STRCT_N=STRCT_N,
            SEED_I=SEED_I,
            FUN.VALUE=TEMPLATES_1_LST,
            USE.NAMES=FALSE)

    base::cat('\a')
    return(SEGMENT_FEATURE_SETS_LST)
}

#' @export
docs_process_collection <- function(ANNOTATION_DT=NULL, ARC_CATEGORIES=NULL, DOCUMENTS_DIR_PATH_STR=NULL, FLATSEQ_SRILM_PARAMETERS=NULL, SEED_I=NULL, SENTENCE_COLLAPSING_FML_LST=NULL, STRCT_N=NULL, STRCT_SRILM_PARAMETERS=NULL) {
    check_args(fun=docs_process_collection)

    base::message(
        base::sprintf(
            "Processing documents in '%s' ... ",
            DOCUMENTS_DIR_PATH_STR))

    # TODO: Use S4 class for ANNOTATION_DT?
    # TODO: Verify that all relevant segment types occur in ANNOTATION_DT.
    DOCUMENT_NAMES_CVEC <-
        base::levels(ANNOTATION_DT[['document_name']])

    document_dir_paths_cvec <-
        base::file.path(
            DOCUMENTS_DIR_PATH_STR,
            base::levels(
                ANNOTATION_DT[['document_name']]))

    base::names(document_dir_paths_cvec) <-
        DOCUMENT_NAMES_CVEC

    FEATURE_SET_LST_LST <-
        parallel::mclapply(
            X=DOCUMENT_NAMES_CVEC,
            FUN=docs_process_document,
            ANNOTATION_DT=ANNOTATION_DT,
            ARC_CATEGORIES=ARC_CATEGORIES,
            DOCUMENT_DIR_PATHS_CVEC=document_dir_paths_cvec,
            FLATSEQ_SRILM_PARAMETERS=FLATSEQ_SRILM_PARAMETERS,
            SEED_I=SEED_I,
            SENTENCE_COLLAPSING_FML_LST=SENTENCE_COLLAPSING_FML_LST,
            STRCT_N=STRCT_N,
            STRCT_SRILM_PARAMETERS=STRCT_SRILM_PARAMETERS)

    ## Remove None results.
    FEATURE_SET_LST_LST <-
        base::unlist(
            FEATURE_SET_LST_LST,
            recursive=TRUE,
            use.names=FALSE)

    return(FEATURE_SET_LST_LST)
}