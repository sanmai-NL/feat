#' To read the analysis' configuration.
#'
#' @name feat-input
NULL

methods::setClass(
    'ArcCategories',
    methods::representation(
        DT='list',
        PRIOR_PROBABILITY_DVEC='numeric'),
        sealed=TRUE)

input_calculate_hash_values <- function(FILES_PATHS_CVEC=NULL)
    base::vapply(
        FILES_PATHS_CVEC,
        FUN=digest::digest,
        algo='sha256',
        file=TRUE,
        FUN.VALUE=TEMPLATES_1_CVEC,
        USE.NAMES=FALSE)

#' @export
input_write_annotation_table_template <- function(ANNOTATION_TABLE_FILE_PATH_STR='annotation_table.tsv', DOCUMENTS_DIR_PATH_STR=NULL) {
    check_args(fun=input_write_annotation_table_template)

    base::message(
        base::sprintf(
            "Writing annotation table template to '%s' ... ",
            ANNOTATION_TABLE_FILE_PATH_STR))

    SENTENCE_FILES_PATHS_CVEC <-
        base::list.files(
            all.files=FALSE,
            full.names=TRUE,
            no..=TRUE,
            path=DOCUMENTS_DIR_PATH_STR,
            pattern='\\d+\\.xml$',
            recursive=TRUE)

    DOCUMENT_DIR_NAMES_CVEC <-
        base::basename(
            base::dirname(
                SENTENCE_FILES_PATHS_CVEC))

    SENTENCE_IDS_IVEC <-
        base::as.integer(
            stringi::stri_match_first_regex(
                base::basename(
                    SENTENCE_FILES_PATHS_CVEC),
                '(\\d+)\\.xml$')[, 2L])

    SHA256_VALUES_CVEC <-
        input_calculate_hash_values(
            FILES_PATHS_CVEC=SENTENCE_FILES_PATHS_CVEC)

    # TODO: separate
    annotation_dt <-
        data.table::data.table(
            SHA256_value=SHA256_VALUES_CVEC,
            document_name=DOCUMENT_DIR_NAMES_CVEC,
            sentence_ID=SENTENCE_IDS_IVEC)

    data.table::setorder(
        annotation_dt,
        document_name,
        sentence_ID)

    utils::write.table(
        col.names=TRUE,
        file=ANNOTATION_TABLE_FILE_PATH_STR,
        fileEncoding='UTF-8',
        row.names=FALSE,
        sep='\t',
        x=annotation_dt)

    return()
}

#' @export
input_verify_annotation_table_consistency <- function(ANNOTATION_DT=NULL, DOCUMENTS_DIR_PATH_STR=NULL) {
    check_args(fun=input_verify_annotation_table_consistency)

    SENTENCE_FILES_PATHS_CVEC <-
        base::file.path(
            DOCUMENTS_DIR_PATH_STR,
            annotation_dt[['document_name']],
            base::sprintf(
                '%d.xml',
                annotation_dt[['sentence_ID']]))

    ACTUAL_SHA256_VALUES_CVEC <-
        base::sort(
            input_calculate_hash_values(
                FILES_PATHS_CVEC=SENTENCE_FILES_PATHS_CVEC))
    # TODO: replace the hash check something better and more efficient.
    # TODO: use setorder() for efficiency.
    if (!base::identical(
            ACTUAL_SHA256_VALUES_CVEC,
            base::sort(ANNOTATION_DT[['SHA256_value']])))
        base::stop(
            base::sprintf(
                'The %d Alpino XML files listed in the annotation table are not of the same number and content as the %d XML files found in the documents directory. ',
                base::nrow(ANNOTATION_DT),
                base::length(ACTUAL_SHA256_VALUES_CVEC)))

    return()
}

#' @export
input_read_annotation_table <- function(ANNOTATION_TABLE_FILE_PATH_STR=NULL, KEPT_ANNOTATION_DT_COLUMN_NAMES_CVEC=NULL) {
    check_args(fun=input_read_annotation_table)

    base::message(
        base::sprintf(
            "Reading annotation table at '%s' ... ",
            ANNOTATION_TABLE_FILE_PATH_STR))

    ANNOTATION_DT <-
        data.table::fread(
            encoding='UTF-8',
            header=TRUE,
            input=ANNOTATION_TABLE_FILE_PATH_STR,
            select=KEPT_ANNOTATION_DT_COLUMN_NAMES_CVEC,
            sep='\t',
            ## TODO: data.table's S3 method for stats::omit.na does not work if NA is contained within factor.
            stringsAsFactors=FALSE,
            verbose=TRUE)

    return(ANNOTATION_DT)
}

input_read_arc_categories_table <- function(ARC_CATEGORIES_FILE_PATH_STR=NULL) {
    check_args(fun=input_read_arc_categories_table)

    base::message(
        base::sprintf(
            "Reading arc categories table at '%s'... ",
            ARC_CATEGORIES_FILE_PATH_STR))

    ARC_CATEGORY_DT <-
        data.table::fread(
            encoding='UTF-8',
            header=TRUE,
            input=ARC_CATEGORIES_FILE_PATH_STR,
            sep='\t',
            stringsAsFactors=TRUE)
    # TODO: value of stringsAsFactors?

    return(base::list(ARC_CATEGORY_DT))
}

input_calculate_arc_category_prior_probability <- function(ARC_CATEGORY_DT=NULL)
    base::log(1.0 / base::nrow(ARC_CATEGORY_DT))

#' @export
input_read_arc_categories_tables <- function(ARC_CATEGORY_FILE_NAME_REX_STR='^arcs_categories_(\\w+).tsv$', DATA_DIR_PATH_STR=NULL) {
    check_args(fun=input_read_arc_categories_tables)

    arc_categories_tables_files_paths_cvec <-
        base::list.files(
            full.names=TRUE,
            no..=TRUE,
            all.files=FALSE,
            recursive=FALSE,
            path=DATA_DIR_PATH_STR,
            pattern=ARC_CATEGORY_FILE_NAME_REX_STR)

    # TODO: naming, and DT naming in class?
    arc_categories_dt_lst <-
        base::vapply(
            arc_categories_tables_files_paths_cvec,
            FUN=input_read_arc_categories_table,
            FUN.VALUE=TEMPLATES_1_LST,
            USE.NAMES=FALSE)

    ARC_CATEGORY_NAMES_CVEC <-
        stringi::stri_match_first_regex(
            base::basename(
                arc_categories_tables_files_paths_cvec),
                pattern=ARC_CATEGORY_FILE_NAME_REX_STR)[,2L]

    base::names(arc_categories_dt_lst) <-
        ARC_CATEGORY_NAMES_CVEC

    ARC_CATEGORY_PRIOR_PROBABILITY_DVEC <-
        base::vapply(
            arc_categories_dt_lst,
            FUN=input_calculate_arc_category_prior_probability,
            FUN.VALUE=TEMPLATES_1_DVEC,
            USE.NAMES=TRUE)

    ARC_CATEGORIES <-
        methods::new(
            'ArcCategories',
            DT=arc_categories_dt_lst,
            PRIOR_PROBABILITY_DVEC=ARC_CATEGORY_PRIOR_PROBABILITY_DVEC)

    return(ARC_CATEGORIES)
}