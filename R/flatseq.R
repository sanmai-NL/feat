#' To extract flat, sequential features.
#'
#' @name feat-flatseq
NULL

FLATSEQ_XPATH_STR <-
    '/alpino_ds/sentence/text()'
FLATSEQ_FILE_NAMES_REX_STR <-
    '.+\\.flatseq.counts$'

flatseq_sentences_cvec_of_xml_doc_error <- function(message=NULL) {
    base::write(
        base::sprintf(
            "Error extracting sentence at '%s' from Alpino XML graph. ",
            FLATSEQ_XPATH_STR),
                base::stderr())
    base::stop(message)
}

flatseq_sentences_cvec_of_xml_doc <- function(XML_DOC=NULL)
    base::tryCatch(
        base::as.character(
            xml2::xml_find_all(
                XML_DOC,
                FLATSEQ_XPATH_STR)[[1L]]),
        error=flatseq_sentences_cvec_of_xml_doc_error,
        warning=flatseq_sentences_cvec_of_xml_doc_error)

flatseq_extract <- function(ALPINO_XML_DOC_LST=NULL, OUTPUT_DIR_PATH_STR=NULL, SEGMENT_TYPE=NULL, SRILM_PARAMETERS=NULL) {
    check_args(fun=flatseq_extract)

    SEGMENT_FILE_PATH_STR <-
        base::file.path(
            OUTPUT_DIR_PATH_STR,
            base::paste0(
                SEGMENT_TYPE,
                '.flatseq'))

    if (!base::file.exists(SEGMENT_FILE_PATH_STR)) {
        SENTENCES_CVEC <-
            base::vapply(
                ALPINO_XML_DOC_LST,
                FUN=flatseq_sentences_cvec_of_xml_doc,
                FUN.VALUE=TEMPLATES_1_CVEC,
                USE.NAMES=FALSE)

        if (SEGMENT_TYPE == ALL_SEGMENT_TYPES) {
            SEGMENT_COUNTS_FILES_PATHS_CVEC <-
                base::list.files(
                    full.names=TRUE,
                    no..=TRUE,
                    all.files=FALSE,
                    recursive=FALSE,
                    path=OUTPUT_DIR_PATH_STR,
                    pattern=FLATSEQ_FILE_NAMES_REX_STR)

            EXIT_STATUS <-
                base::system2(
                    'cat',
                    SEGMENT_COUNTS_FILES_PATHS_CVEC,
                    stdout=SEGMENT_FILE_PATH_STR)

            if (EXIT_STATUS != 0L)
                base::stop(
                    base::sprintf(
                        "Error running 'cat'. Exit status: %d. ",
                        EXIT_STATUS))
        }

        base::cat(
            SENTENCES_CVEC,
            file=SEGMENT_FILE_PATH_STR,
            sep='\n')
    }

    COUNTS_AND_LM <-
        SRILM_count_and_write_ARPA_language_model(
            TEXT_FILE_PATH_STR=SEGMENT_FILE_PATH_STR,
            PARAMETERS=SRILM_PARAMETERS)

    return(COUNTS_AND_LM)
}
