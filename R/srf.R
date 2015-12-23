## To extract flat 'surface' features.

srf_cvec_of_XML_document_error <- function(CONDITION)
    base::stop(
        futile.logger::flog.fatal(
            "An error occurred during extraction of the surface string from an XML document: %s",
            CONDITION))

srf_cvec_of_XML_document <- function(
    XML_DOCUMENT,
    XPATH_STR)
    base::tryCatch({
            CVEC <-
                base::as.character(
                    xml2::xml_find_all(
                        x=XML_DOCUMENT,
                        xpath=XPATH_STR)[[1L]])
            base::stopifnot(
                base::length(CVEC) == 1L)
            CVEC },
        error=srf_cvec_of_XML_document_error,
        warning=srf_cvec_of_XML_document_error)
