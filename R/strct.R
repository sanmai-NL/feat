#' Structural features
#'
#' @name feat-strct
#' @importFrom Rcpp evalCpp
#' @useDynLib feat
NULL

STRCT_XPATH_STR <-
    '/alpino_ds/node//node'

strct_nodeset_of_xml_doc_error <- function(message=NULL) {
    base::write(
        base::sprintf(
            "Error extracting Alpino nodes with XPath '%s' from Alpino XML graph. ",
            STRCT_XPATH_STR),
        base::stderr())
    base::stop(message)
}

strct_nodeset_of_xml_doc <- function(XML_DOC=NULL)
    base::tryCatch(
        base::list(
            xml2::xml_find_all(
                XML_DOC,
                xpath=STRCT_XPATH_STR)),
        error=strct_nodeset_of_xml_doc_error,
        warning=strct_nodeset_of_xml_doc_error)

strct_arc_d_f_of_node <- function(CHILD_NODE=NULL, ARC_CATEGORIES=NULL) {
    PARENT_NODE <-
        xml2::xml_parent(CHILD_NODE)
    PARENT_ID <-
        base::as.integer(
            xml2::xml_attr(PARENT_NODE, 'id'))
    CHILD_ID <-
        base::as.integer(
            xml2::xml_attr(CHILD_NODE, 'id'))

    arc_labels <-
        xml2::xml_attrs(
            CHILD_NODE)[
                base::names(
                    ARC_CATEGORIES@PRIOR_PROBABILITY_DVEC)]
    arc_labels <-
        arc_labels[!base::is.na(arc_labels)]

    arc_scores_dvec <-
        ARC_CATEGORIES@PRIOR_PROBABILITY_DVEC[
            base::names(arc_labels)]

    base::names(arc_scores_dvec) <-
        arc_labels

    ARC_LST <-
        base::c(
            parent_id=PARENT_ID,
            child_id=CHILD_ID,
            base::as.list(
                arc_scores_dvec))

    # TODO: (efficiency)
    return(base::list(dplyr::as_data_frame(ARC_LST)))
}

strct_arcs_d_f_of_nodeset_lst <- function(NODESET_LST=NULL, ARC_CATEGORIES=NULL) {
    NODESET <-
        base::unlist(NODESET_LST,
            recursive=FALSE,
            use.names=FALSE)
    # TODO: (efficiency) much time is spent here, instead create a list.
    arcs_d_f <-
        dplyr::arrange(
            dplyr::bind_rows(
                base::vapply(
                    NODESET,
                    FUN=strct_arc_d_f_of_node,
                    ARC_CATEGORIES=ARC_CATEGORIES,
                    FUN.VALUE=TEMPLATES_1_LST)),
            parent_id,
            child_id)

    ## Logarithmic weights. Assume a prior probability 1.0 (log(1) == 0.0) means 'no observation' to induce matrix sparsity. This does limit representable arc label categories to those with more than one possible arc label.
    arcs_d_f[base::is.na(arcs_d_f)] <- 0.0

    return(arcs_d_f)
}

strct_write_sequence_counts <- function(ALPINO_XML_DOC_LST=NULL, ARC_CATEGORIES=NULL, SEGMENT_TYPE=NULL, OUTPUT_DIR_PATH_STR=NULL, N=NULL, node_child_id_i=0L) {
    # TODO: make n_strides_to_take & n_iterations parameters.
    check_args(fun=strct_write_sequence_counts)

    GRAPHVIZ_FILE_PATH_STR <-
        base::file.path(
            OUTPUT_DIR_PATH_STR,
            base::paste0(
                SEGMENT_TYPE,
                '.gv'))

    SRILM_COUNTS_FILE_PATH_STR <-
        base::file.path(
            OUTPUT_DIR_PATH_STR,
            base::paste0(
                SEGMENT_TYPE,
                '.strct.counts'))

    if (!base::file.exists(GRAPHVIZ_FILE_PATH_STR) ||
        !base::file.exists(SRILM_COUNTS_FILE_PATH_STR)) {
        # TODO: refactor?
        NODESET_LST <-
            base::vapply(
                ALPINO_XML_DOC_LST,
                FUN=strct_nodeset_of_xml_doc,
                FUN.VALUE=TEMPLATES_1_LST,
                USE.NAMES=FALSE)

        ARCS_D_F <-
            strct_arcs_d_f_of_nodeset_lst(
                NODESET_LST=NODESET_LST,
                ARC_CATEGORIES=ARC_CATEGORIES)

        ARCS_D_F_NROW_I <- base::nrow(ARCS_D_F)
        base::message(
            base::sprintf(
                "Counting arc label subsequences among %d arcs, based on walks of length %d ... ",
                ARCS_D_F_NROW_I,
                N))

        ## Arcs scores matrix, each row represents an arc.
        # TODO: use sparse matrix.
        arcs_scores_dmat <- base::as.matrix(ARCS_D_F[, -c(1L, 2L)])

        enumerate_random_trails_on_linguistic_network(
            arcs_list_df=ARCS_D_F,
            arcs_scores_dmat=arcs_scores_dmat,
            n_strides_to_take=N,
            N=N,
            is_all_ngrams_up_to_n=TRUE,
            n_iterations=base::nrow(ARCS_D_F) * N,
            GraphViz_file_path=GRAPHVIZ_FILE_PATH_STR,
            SRILM_counts_file_path=SRILM_COUNTS_FILE_PATH_STR)
    }

    return(SRILM_COUNTS_FILE_PATH_STR)
}

strct_extract <- function(OUTPUT_DIR_PATH_STR=NULL, SEGMENT_TYPE=NULL, ALPINO_XML_DOC_LST=NULL, ARC_CATEGORIES=NULL, SRILM_PARAMETERS=NULL, N=NULL) {
    check_args(fun=strct_extract)

    COUNTS_FILE_PATH_STR <-
        strct_write_sequence_counts(
            ALPINO_XML_DOC_LST=ALPINO_XML_DOC_LST,
            ARC_CATEGORIES=ARC_CATEGORIES,
            SEGMENT_TYPE=SEGMENT_TYPE,
            OUTPUT_DIR_PATH_STR=OUTPUT_DIR_PATH_STR,
            N=N)

    COUNTS_AND_LM <-
        SRILM_write_ARPA_language_model(
            COUNTS_FILE_PATH_STR=COUNTS_FILE_PATH_STR,
            PARAMETERS=SRILM_PARAMETERS)

    return(COUNTS_AND_LM)
}