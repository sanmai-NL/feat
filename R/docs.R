#' To process documents.
#'
#' @name feat-docs
NULL

ALL_SEGMENT_TYPES <-
    'all'

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

docs_process_segment <- function(CURRENT_SEGMENT_TYPE=NULL, DOCUMENT_SENTENCE_ROWS_DT=NULL, DOCUMENT_DIR_PATH_STR=NULL, ARC_CATEGORIES=NULL, FLATSEQ_SRILM_PARAMETERS=NULL, STRCT_SRILM_PARAMETERS=NULL, STRCT_N=NULL, SEED_I=NULL) {
    check_args(fun=docs_process_segment)
    if (base::is.na(STRCT_SRILM_PARAMETERS) && base::is.na(FLATSEQ_SRILM_PARAMETERS)) {
        base::stop('FLATSEQ_SRILM_PARAMETERS and STRCT_SRILM_PARAMETERS may not both be NA. ')
    }

    base::message(
        base::sprintf(
            "Processing segment '%s' ... ",
            CURRENT_SEGMENT_TYPE))

    ## Note: a single TRUE as row index returns all rows in data.table.
    SEGMENT_ROWS_BVEC <-
        if (CURRENT_SEGMENT_TYPE == ALL_SEGMENT_TYPES) TRUE
        else DOCUMENT_SENTENCE_ROWS_DT[['segment_type']] %in% CURRENT_SEGMENT_TYPE
    SENTENCE_FILE_NAMES_CVEC <-
        base::sprintf(
            '%d.xml',
            DOCUMENT_SENTENCE_ROWS_DT[
                SEGMENT_ROWS_BVEC,
                'sentence_ID',
                with=FALSE][[1L]])

    ALPINO_XML_FILES_PATHS_CVEC <-
        base::file.path(DOCUMENT_DIR_PATH_STR, SENTENCE_FILE_NAMES_CVEC)
    ALPINO_XML_DOC_LST <-
        base::vapply(
            ALPINO_XML_FILES_PATHS_CVEC,
            FUN=docs_parse_Alpino_XML,
            FUN.VALUE=TEMPLATES_1_LST,
            USE.NAMES=FALSE)

    if (!base::anyNA(FLATSEQ_SRILM_PARAMETERS)) {
        FLATSEQ_COUNTS_AND_LM <-
            flatseq_extract(
                OUTPUT_DIR_PATH_STR=DOCUMENT_DIR_PATH_STR,
                ALPINO_XML_DOC_LST=ALPINO_XML_DOC_LST,
                SEGMENT_TYPE=CURRENT_SEGMENT_TYPE,
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
                OUTPUT_DIR_PATH_STR=DOCUMENT_DIR_PATH_STR,
                ALPINO_XML_DOC_LST=ALPINO_XML_DOC_LST,
                SEGMENT_TYPE=CURRENT_SEGMENT_TYPE,
                ARC_CATEGORIES=ARC_CATEGORIES,
                SRILM_PARAMETERS=STRCT_SRILM_PARAMETERS,
                N=STRCT_N)

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
}

docs_process_document <- function(DOCUMENT_NAME=NULL, DOCUMENT_DIR_PATHS_CVEC=NULL, DOCUMENT_DIR_PATH_STR=DOCUMENT_DIR_PATHS_CVEC[DOCUMENT_NAME], ANNOTATION_DT=NULL, ARC_CATEGORIES=NULL, EXTRACTABLE_SEGMENT_TYPES_CVEC=NULL, FLATSEQ_SRILM_PARAMETERS=NULL, STRCT_SRILM_PARAMETERS=NULL, STRCT_N=NULL, SEED_I=NULL) {
    check_args(fun=docs_process_document)

    base::message(
        base::sprintf(
            "Processing document '%s' ... ",
            DOCUMENT_NAME))

    # TODO: use select()
    DOCUMENT_SENTENCE_ROWS_DT <-
        ANNOTATION_DT[ANNOTATION_DT[['document_name']] %in% DOCUMENT_NAME,]

    SEGMENT_FEATURE_SETS_LST <-
        base::vapply(
            EXTRACTABLE_SEGMENT_TYPES_CVEC,
            FUN=docs_process_segment,
            DOCUMENT_SENTENCE_ROWS_DT=DOCUMENT_SENTENCE_ROWS_DT,
            DOCUMENT_DIR_PATH_STR=DOCUMENT_DIR_PATH_STR,
            ARC_CATEGORIES=ARC_CATEGORIES,
            FLATSEQ_SRILM_PARAMETERS=FLATSEQ_SRILM_PARAMETERS,
            STRCT_SRILM_PARAMETERS=STRCT_SRILM_PARAMETERS,
            STRCT_N=STRCT_N,
            SEED_I=SEED_I,
            FUN.VALUE=TEMPLATES_1_LST,
            USE.NAMES=FALSE)

    cat('\a')
    return(SEGMENT_FEATURE_SETS_LST)
}

#' @export
docs_process_collection <- function(ANNOTATION_DT=NULL, ARC_CATEGORIES=NULL, DOCUMENTS_DIR_PATH_STR=NULL, RELEVANT_SEGMENT_TYPES_CVEC=NULL, FLATSEQ_SRILM_PARAMETERS=NULL, STRCT_SRILM_PARAMETERS=NULL, STRCT_N=NULL, SEED_I=NULL) {
    check_args(fun=docs_process_collection)

    base::message(
        base::sprintf(
            "Processing documents in '%s' ... ",
            DOCUMENTS_DIR_PATH_STR))

    # TODO: use S4 class for ANNOTATION_DT?
    DOCUMENT_NAMES_CVEC <-
        base::levels(
            ANNOTATION_DT[['document_name']])

    # TODO: verify that all relevant segment types occur in ANNOTATION_DT
    EXTRACTABLE_SEGMENT_TYPES_CVEC <-
        base::c(
            RELEVANT_SEGMENT_TYPES_CVEC,
            ALL_SEGMENT_TYPES)

    # TODO: skip documents already processed
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
            DOCUMENT_DIR_PATHS_CVEC=document_dir_paths_cvec,
            ANNOTATION_DT=ANNOTATION_DT,
            ARC_CATEGORIES=ARC_CATEGORIES,
            EXTRACTABLE_SEGMENT_TYPES_CVEC=EXTRACTABLE_SEGMENT_TYPES_CVEC,
            FLATSEQ_SRILM_PARAMETERS=FLATSEQ_SRILM_PARAMETERS,
            STRCT_SRILM_PARAMETERS=STRCT_SRILM_PARAMETERS,
            STRCT_N=STRCT_N,
            SEED_I=SEED_I)

    return(FEATURE_SET_LST_LST)
}







############################################
read_sequences_scores_dvec_file <- function(SEQUENCES_SCORES_DVEC_FILE_PATH_STR, SEGMENT_TYPE_STR) {
    SEQUENCES_SCORES_DVEC_FILE_NAME_STR <-
        base::basename(SEQUENCES_SCORES_DVEC_FILE_PATH_STR)
    if (stringi::stri_startswith_fixed(
        str=SEQUENCES_SCORES_DVEC_FILE_NAME_STR,
        pattern=SEGMENT_TYPE_STR)) {
        return(SEQUENCES_SCORES_DVEC_FILE_PATH_STR)
    } else {
        return(NA_character_)
    }
}

#' @export
docs_produce_design_matrix_d_f <- function(FEATURE_SET_LST_LST=NULL, ANNOTATION_DT=NULL, SEGMENT_TYPE=NULL) {
    check_args(fun=docs_produce_design_matrix_d_f)

    FEATURE_SET_LST <-
        base::unlist(
            FEATURE_SET_LST_LST,
            recursive=FALSE)

    SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC <-
        stats::na.omit(
            base::vapply(
                FEATURE_SET_LST,
                FUN=function(x) read_sequences_scores_dvec_file(x@FLATSEQ@SEQUENCES_AND_SCORES@SEQUENCES_SCORES_DVEC_FILE_PATH_STR, SEGMENT_TYPE),
                FUN.VALUE=character(length=1L)))

    FEATURE_D_F_LST <-
        base::lapply(
            SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC,
            FUN=function(FEATURE_SCORES_DVEC_FILES_PATH_STR)
                base::do.call(
                    dplyr::data_frame,
                    base::as.list(
                        base::readRDS(
                            FEATURE_SCORES_DVEC_FILES_PATH_STR))))

    design_matrix_d_f <-
        dplyr::bind_rows(FEATURE_D_F_LST)
    design_matrix_d_f[base::is.na(design_matrix_d_f)] <- 0.0

    # TODO: do not simply select the document's first sentence's conclusion type, but handle the case of multiple conclusion types.simply
    CONCLUSION_TYPE_FAC <-
        dplyr::distinct(ANNOTATION_DT, document_name)[['conclusion_type']]

    # browser()
    DESIGN_MATRIX_Y_D_F <-
        dplyr::mutate(
            design_matrix_d_f,
            conclusion_type=CONCLUSION_TYPE_FAC)

    return(DESIGN_MATRIX_Y_D_F)
}