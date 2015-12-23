#' @include extract_features-generic.R SrfFeatureRepresentation-class.R SrfFeatureRepresentation-class.R
#' @importFrom methods setMethod
NULL

# TODO: Use list of FilesystemPaths instead of CVECs.
# TODO: Solve bug upstream in roxygen2, cannot use namespaced call methods::setMethod.
## Extract the surface (SRF) features of an object.
#' @keywords methods
#' @concept surface
#' @describeIn extract_features See: \code{\link{SrfFeatureRepresentation}}
#' @export
setMethod(
    f='extract_features',
    signature=methods::signature(
        object='SrfFeatureRepresentation',
        FEATURES_EXTRACTION_PARAMETERS='FeaturesExtractionParameters',
        OBJECT_ID_I='integer',
        OBJECT_OUTPUT_DIR_PATH_STR='character',
        RELATIVE_FILE_PATHS_CVEC='character'),
    definition=
function(
    object,
    FEATURES_EXTRACTION_PARAMETERS,
    OBJECT_ID_I,
    OBJECT_OUTPUT_DIR_PATH_STR,
    RELATIVE_FILE_PATHS_CVEC) {

    DOCUMENT_FILE_PATHS_CVEC <-
        base::file.path(
            FEATURES_EXTRACTION_PARAMETERS@DOCUMENTS_DIR_PATH,
            RELATIVE_FILE_PATHS_CVEC)
    OBJECT_TEXT_FILE_PATH_STR <-
        base::file.path(
            OBJECT_OUTPUT_DIR_PATH_STR,
            object@NAME_STR)

    if (!base::file.exists(OBJECT_TEXT_FILE_PATH_STR)) {
        OBJECT_XML_DOCUMENTS_LST <-
            base::lapply(
                DOCUMENT_FILE_PATHS_CVEC,
                xml2::read_xml)
        OBJECT_TEXTS_CVEC <-
            base::vapply(
                OBJECT_XML_DOCUMENTS_LST,
                srf_cvec_of_XML_document,
                FUN.VALUE=base::character(length=1L),
                XPATH_STR=object@XPATH_STR,
                USE.NAMES=FALSE)
        base::cat(
            OBJECT_TEXTS_CVEC,
            file=OBJECT_TEXT_FILE_PATH_STR,
            sep=object@OBJECT_SEPARATOR_STR)
    } else {
        futile.logger::flog.debug('Reusing text file. ')
    }

    COUNTS_AND_LM <-
        SRILM_count_and_write_ARPA_language_model(
            TEXT_FILE_PATH_STR=OBJECT_TEXT_FILE_PATH_STR,
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC=object@SRILM_NGRAMCOUNT_PARAMETERS_CVEC)
    OBJECT_FEATURES_DVEC <-
        SRILM_read_or_extract_features_vector(
            COUNTS_AND_LM=COUNTS_AND_LM,
            SRILM_NGRAM_PARAMETERS_CVEC=object@SRILM_NGRAM_PARAMETERS_CVEC)

    return(OBJECT_FEATURES_DVEC)
})

## Extract the structural (STRCT) features of an object.
#' @keywords methods
#' @concept structural
#' @describeIn extract_features See: \code{\link{StrctFeatureRepresentation}}
#' @export
setMethod(
    f='extract_features',
    signature=methods::signature(
        object='StrctFeatureRepresentation',
        FEATURES_EXTRACTION_PARAMETERS='FeaturesExtractionParameters',
        OBJECT_ID_I='integer',
        OBJECT_OUTPUT_DIR_PATH_STR='character',
        RELATIVE_FILE_PATHS_CVEC='character'),
    definition=
function(
    object,
    FEATURES_EXTRACTION_PARAMETERS,
    OBJECT_ID_I,
    OBJECT_OUTPUT_DIR_PATH_STR,
    RELATIVE_FILE_PATHS_CVEC) {

    DOCUMENT_FILE_PATHS_CVEC <-
        base::file.path(
            FEATURES_EXTRACTION_PARAMETERS@DOCUMENTS_DIR_PATH,
            RELATIVE_FILE_PATHS_CVEC)
    OBJECT_COUNTS_FILE_PATH_STR <-
        strct_write_sequence_counts(
            DOCUMENT_FILE_PATHS_CVEC=DOCUMENT_FILE_PATHS_CVEC,
            FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS,
            FEATURE_REPRESENTATION=object,
            OBJECT_ID_I=OBJECT_ID_I,
            OBJECT_OUTPUT_DIR_PATH_STR=OBJECT_OUTPUT_DIR_PATH_STR)
    COUNTS_AND_LM <-
        SRILM_write_ARPA_language_model(
            COUNTS_FILE_PATH_STR=OBJECT_COUNTS_FILE_PATH_STR,
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC=object@SRILM_NGRAMCOUNT_PARAMETERS_CVEC)
    OBJECT_FEATURES_DVEC <-
        SRILM_read_or_extract_features_vector(
            COUNTS_AND_LM=COUNTS_AND_LM,
            SRILM_NGRAM_PARAMETERS_CVEC=object@SRILM_NGRAM_PARAMETERS_CVEC)

    return(OBJECT_FEATURES_DVEC)
})