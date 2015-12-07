#' To prepare feature vectors for analysis.
#'
#' @name feat-preanalysis
NULL

methods::setClass(
    'FeatureNamesAndScoresFilePaths',
    methods::representation(
        FEATURES_CVEC_LST='list',
        SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC='character'),
    sealed=TRUE)

preanalysis_extract_sequences_scores_dvec_file_paths_cvec <- function(OBJECT_FEATURE_SETS=NULL, MY_FEATURE_REPRESENTATIONS_CVEC=NULL, OBJECT_FILES_FILTER_REX_STR=NULL) {
    check_args(fun=preanalysis_extract_sequences_scores_dvec_file_paths_cvec)

    SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC <-
        base::vapply(
            MY_FEATURE_REPRESENTATIONS_CVEC,
            FUN=function(FEATURE_REPRESENTATION_STR) {
                SEQUENCES_SCORES_DVEC_FILE_PATH_STR <-
                	methods::slot(
                   		OBJECT_FEATURE_SETS,
                   		FEATURE_REPRESENTATION_STR)@SEQUENCES_AND_SCORES@SEQUENCES_SCORES_DVEC_FILE_PATH_STR
                SEQUENCES_SCORES_DVEC_FILE_NAME_STR <-
                    base::basename(SEQUENCES_SCORES_DVEC_FILE_PATH_STR)

                if (stringi::stri_detect_regex(
                    str=SEQUENCES_SCORES_DVEC_FILE_NAME_STR,
                    pattern=OBJECT_FILES_FILTER_REX_STR)) {
                    return(SEQUENCES_SCORES_DVEC_FILE_PATH_STR)
                } else {
                    return(NA_character_)
                }
            },
        FUN.VALUE=TEMPLATES_1_CVEC)

    return(SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC)
}

#' @export
preanalysis_extract_feature_names_and_scores_file_paths <- function(FEATURE_SET_LST=NULL, MY_FEATURE_REPRESENTATIONS_CVEC=NULL, OBJECT_FILES_FILTER_REX_STR=NULL) {
    check_args(fun=preanalysis_extract_feature_names_and_scores_file_paths)
    if (!base::all(MY_FEATURE_REPRESENTATIONS_CVEC %in% FEATURE_REPRESENTATIONS_CVEC)) {
        base::stop('Unknown feature representation specified. ')
    }

    SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC <-
        stats::na.omit(
            base::c(
                base::vapply(
                    FEATURE_SET_LST,
                    FUN=preanalysis_extract_sequences_scores_dvec_file_paths_cvec,
                    MY_FEATURE_REPRESENTATIONS_CVEC=FEATURE_REPRESENTATIONS_CVEC,
                    OBJECT_FILES_FILTER_REX_STR=OBJECT_FILES_FILTER_REX_STR,
                    FUN.VALUE=FEATURE_REPRESENTATIONS_CVEC,
                    USE.NAMES=FALSE)))
    FEATURES_CVEC_LST <-
        parallel::mclapply(
            SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC,
            FUN=function(SEQUENCE_SCORES_DVEC_FILE_PATH_STR)
                base::names(
                    base::readRDS(
                        SEQUENCE_SCORES_DVEC_FILE_PATH_STR)))

    FEATURE_NAMES_AND_SCORES_FILE_PATHS <-
        methods::new(
            'FeatureNamesAndScoresFilePaths',
            SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC=SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC,
            FEATURES_CVEC_LST=FEATURES_CVEC_LST)

    return(FEATURE_NAMES_AND_SCORES_FILE_PATHS)
}

#' @export
preanalysis_produce_design_mat <- function(ANNOTATION_DT=NULL, FEATURE_SET_LST_LST=NULL, MY_FEATURE_REPRESENTATIONS_CVEC=NULL, OBJECT_FILES_FILTER_REX_STR=NULL) {
    check_args(fun=preanalysis_produce_design_mat)

    FEATURE_SET_LST <-
        base::unlist(
            FEATURE_SET_LST_LST,
            recursive=FALSE,
            use.names=FALSE)

    FEATURE_NAMES_AND_SCORES_FILE_PATHS <-
        preanalysis_extract_feature_names_and_scores_file_paths(
            FEATURE_SET_LST=FEATURE_SET_LST,
            MY_FEATURE_REPRESENTATIONS_CVEC=MY_FEATURE_REPRESENTATIONS_CVEC,
            OBJECT_FILES_FILTER_REX_STR=OBJECT_FILES_FILTER_REX_STR)

    FEATURE_TYPES_CVEC <-
        stringi::stri_unique(
            base::unlist(
                FEATURE_NAMES_AND_SCORES_FILE_PATHS@FEATURES_CVEC_LST,
                use.names=FALSE,
                recursive=FALSE))

    DOCUMENT_NAMES_CVEC <-
        base::levels(ANNOTATION_DT[['document_name']])

    design_mat <-
        Matrix::Matrix(
            0L,
            nrow=base::length(DOCUMENT_NAMES_CVEC),
            ncol=base::length(FEATURE_TYPES_CVEC),
            sparse=TRUE,
            dimnames=base::list(
                DOCUMENT_NAMES_CVEC,
                FEATURE_TYPES_CVEC))

    ## Set sparse design matrix cells to feature scores (document, feature) of objects
    for (SEQUENCES_SCORES_DVEC_FILE_PATH_STR in FEATURE_NAMES_AND_SCORES_FILE_PATHS@SEQUENCES_SCORES_DVEC_FILE_PATHS_CVEC) {
        FEATURES_SCORES_DVEC <-
            base::readRDS(SEQUENCES_SCORES_DVEC_FILE_PATH_STR)
        DOCUMENT_NAME_STR <-
            base::basename(
                base::dirname(
                    SEQUENCES_SCORES_DVEC_FILE_PATH_STR))
        design_mat[
            DOCUMENT_NAME_STR,
            base::names(FEATURES_SCORES_DVEC)] <-
                FEATURES_SCORES_DVEC
    }

    return(design_mat)
}