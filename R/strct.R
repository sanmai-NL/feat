## To extract structural features.

strct_read_arc_category_table <- function(
    ARC_CATEGORY_TABLE_FILE_PATH_STR) {

    futile.logger::flog.debug(
        "Reading arc category table at '%s' ... ",
        ARC_CATEGORY_TABLE_FILE_PATH_STR)

    # TODO: value of stringsAsFactors?
    # TODO: robustness improvements?
    ARC_CATEGORY_DT <-
        data.table::fread(
            encoding='UTF-8',
            header=TRUE,
            input=ARC_CATEGORY_TABLE_FILE_PATH_STR,
            sep='\t',
            stringsAsFactors=TRUE)

    return(ARC_CATEGORY_DT)
}

strct_calculate_arc_category_prior_probability <- function(
    ARC_CATEGORY_DT)
    1.0 / base::nrow(ARC_CATEGORY_DT)

#' To read arc category tables
#'
#' @param ARC_CATEGORIES_CVEC The names used to identitify the arc categories, for which trails may be extracted by \code{\link{extract_features}}.
#' @param ARC_CATEGORY_FILES_NAMES_REX_STR A \emph{PCRE} regular expression that exclusively matches the arc category table files.
#' @param ARC_CATEGORY_TABLES_DIR_PATH A \code{\link{FilesystemPath}} to the directory containing an arc category table file for each arc category in \code{ARC_CATEGORIES_CVEC}.
#' @return \code{ARC_CATEGORIES} See: \code{\link{ArcCategories}}.
#' @export
strct_read_arc_category_tables <- function(
    ARC_CATEGORIES_CVEC,
    ARC_CATEGORY_FILES_NAMES_REX_STR='^arc_category_(\\w+).tsv$',
    ARC_CATEGORY_TABLES_DIR_PATH) {

    ARC_CATEGORY_TABLES_FILES_PATHS_CVEC <-
        base::normalizePath(
            base::list.files(
                full.names=TRUE,
                no..=TRUE,
                all.files=FALSE,
                recursive=FALSE,
                path=ARC_CATEGORY_TABLES_DIR_PATH,
                pattern=ARC_CATEGORY_FILES_NAMES_REX_STR),
        mustWork=TRUE)
    ARC_CATEGORIES_CVEC_LENGTH_I <-
        base::length(ARC_CATEGORIES_CVEC)
    ARC_CATEGORY_TABLES_FILES_PATHS_CVEC_LENGTH_I <-
        base::length(ARC_CATEGORY_TABLES_FILES_PATHS_CVEC)
    if (ARC_CATEGORIES_CVEC_LENGTH_I != ARC_CATEGORY_TABLES_FILES_PATHS_CVEC_LENGTH_I) {
        base::stop(
            futile.logger::flog.error(
                'Could not locate the implied number of arc category table files: %d were requested, but %d were found. ',
                ARC_CATEGORIES_CVEC_LENGTH_I,
                ARC_CATEGORY_TABLES_FILES_PATHS_CVEC_LENGTH_I))
    } else {
        arc_category_dt_lst <-
            base::lapply(
                ARC_CATEGORY_TABLES_FILES_PATHS_CVEC,
                FUN=strct_read_arc_category_table)

        base::names(arc_category_dt_lst) <-
            ARC_CATEGORIES_CVEC
        PRIOR_PROBABILITY_DVEC <-
            base::vapply(
                arc_category_dt_lst,
                FUN=strct_calculate_arc_category_prior_probability,
                FUN.VALUE=base::numeric(length=1L),
                USE.NAMES=TRUE)

        ARC_CATEGORIES <-
            ArcCategories(
                DT_LST=arc_category_dt_lst,
                PRIOR_PROBABILITY_DVEC=PRIOR_PROBABILITY_DVEC)
        return(ARC_CATEGORIES)
    }
}

strct_nodeset_of_XML_document_error <- function(
    CONDITION)
    base::stop(
        futile.logger::flog.fatal(
            'An error occurred during extraction of the STRCT nodeset from an XML document. '))

strct_nodeset_of_XML_document <- function(
    XML_DOCUMENT,
    XPATH_STR) {

    base::tryCatch({
            NODESET <-
                xml2::xml_find_all(
                    x=XML_DOCUMENT,
                    xpath=XPATH_STR)
            base::stopifnot(
                base::length(NODESET) > 0L)
            NODESET },
        error=strct_nodeset_of_XML_document_error,
        warning=strct_nodeset_of_XML_document_error)
}

strct_arcs_dt_of_node <- function(
    CHILD_NODE,
    ARC_CATEGORIES) {

    PARENT_NODE <-
        xml2::xml_parent(CHILD_NODE)
    PARENT_ID_I <-
        base::as.integer(
            xml2::xml_attr(PARENT_NODE, 'id'))
    CHILD_ID_I <-
        base::as.integer(
            xml2::xml_attr(CHILD_NODE, 'id'))

    arc_labels_cvec <-
        xml2::xml_attrs(
            CHILD_NODE)[
                base::names(
                    ARC_CATEGORIES@PRIOR_PROBABILITY_DVEC)]
    arc_labels_cvec <-
        arc_labels_cvec[!base::is.na(arc_labels_cvec)]

    arc_categories_cvec <-
        base::names(arc_labels_cvec)

    arcs_scores_dvec <-
        ARC_CATEGORIES@PRIOR_PROBABILITY_DVEC[
            arc_categories_cvec]

    base::names(arcs_scores_dvec) <-
        stringi::stri_join(
            arc_categories_cvec,
            arc_labels_cvec,
            sep='_')

    ARCS_LST <-
        base::c(
            parent_id=PARENT_ID_I,
            child_id=CHILD_ID_I,
            base::as.list(arcs_scores_dvec))

    return(ARCS_LST)
}

strct_arcs_dt_of_nodeset_lst <- function(
    NODESET_LST,
    FEATURE_REPRESENTATION) {

    NODESET <-
        base::unlist(
            NODESET_LST,
            recursive=FALSE,
            use.names=FALSE)

    ARCS_LST <-
        base::lapply(
            NODESET,
            strct_arcs_dt_of_node,
            ARC_CATEGORIES=FEATURE_REPRESENTATION@ARC_CATEGORIES)

    arcs_dt <- data.table::rbindlist(ARCS_LST, fill=TRUE, use.names=TRUE)

    ## Logarithmic weights. Assume a prior probability of 1.0 (log(1.0) == 0.0)
    ## means 'no observation' to induce matrix sparsity. This does limit representable arc
    ## label categories to those with more than one possible arc label.
    for (COLUMN_INDEX_I in base::seq_along(arcs_dt)) {
        data.table::set(
            arcs_dt,
            i=base::which(base::is.na(arcs_dt[[COLUMN_INDEX_I]])),
            j=COLUMN_INDEX_I,
            value=0.0)
    }

    return(arcs_dt)
}

#' @importFrom Rcpp evalCpp
#' @useDynLib feat
strct_write_sequence_counts <- function(
    DOCUMENT_FILE_PATHS_CVEC,
    FEATURES_EXTRACTION_PARAMETERS,
    FEATURE_REPRESENTATION,
    OBJECT_ID_I,
    OBJECT_OUTPUT_DIR_PATH_STR) {

    COUNTS_FILE_PATH_STR <-
        base::file.path(
            OBJECT_OUTPUT_DIR_PATH_STR,
            base::sprintf('%s.counts',
                FEATURE_REPRESENTATION@NAME_STR))

    if (!base::file.exists(COUNTS_FILE_PATH_STR)) {
        XML_DOCUMENTS_LST <-
            base::lapply(
                DOCUMENT_FILE_PATHS_CVEC,
                xml2::read_xml)
        NODESET_LST <-
            base::lapply(
                XML_DOCUMENTS_LST,
                strct_nodeset_of_XML_document,
                XPATH_STR=FEATURE_REPRESENTATION@XPATH_STR)

        ARCS_DT <-
            strct_arcs_dt_of_nodeset_lst(
                NODESET_LST=NODESET_LST,
                FEATURE_REPRESENTATION=FEATURE_REPRESENTATION)
        ARCS_DT_NROW_I <- base::nrow(ARCS_DT)

        futile.logger::flog.debug(
            "Accumulating %d-grams in random trails sought in linguistic network of %d arcs with %d possible arc labels ... ",
            FEATURE_REPRESENTATION@N_I,
            ARCS_DT_NROW_I,
            base::ncol(ARCS_DT))
        GRAPHVIZ_FILE_PATH_STR <-
            base::file.path(
                OBJECT_OUTPUT_DIR_PATH_STR,
                base::sprintf('%s.gv',
                    FEATURE_REPRESENTATION@NAME_STR))
        USE_BOOLEAN_INSTEAD_OF_REAL_SCORING_B <-
            if (FEATURE_REPRESENTATION@SCORING_STR == 'boolean') TRUE
            else if (FEATURE_REPRESENTATION@SCORING_STR == 'Pr') FALSE
            else NULL
        COUNTS_FILE_PATH_STR <-
            base::tryCatch({
                search_random_trails_in_linguistic_network(
                    arcs_df=ARCS_DT,
                    M=FEATURE_REPRESENTATION@M_I,
                    N=FEATURE_REPRESENTATION@N_I,
                    is_all_ngrams_up_to_n=FEATURE_REPRESENTATION@ALL_NGRAMS_UP_TO_N_I_B,
                    I=ARCS_DT_NROW_I * FEATURE_REPRESENTATION@I_I,
                    use_boolean_instead_of_real_scoring=USE_BOOLEAN_INSTEAD_OF_REAL_SCORING_B,
                    GraphViz_file_path=GRAPHVIZ_FILE_PATH_STR,
                    SRILM_counts_file_path=COUNTS_FILE_PATH_STR)
                COUNTS_FILE_PATH_STR
            },
            error=function(CONDITION) {
                futile.logger::flog.error(
                    'search_random_trails_in_linguistic_network raised an exception: "%s". Suggestion: %s',
                    CONDITION,
                    'The linguistic network is too small to extract STRCT features given the parameters you specified. ')
                return(NULL) })
    } else {
        futile.logger::flog.debug(
            'Reusing counts file. ')
    }

    return(COUNTS_FILE_PATH_STR)
}