#' To read the analysis' configuration.
#'
#' @name feat-input
NULL

# TODO: use single named list for both below
ANNOTATION_DT_COLUMN_CLASSES_LST <-
    base::c(
        base::character(),
        base::character(),
        base::integer(),
        base::character(),
        base::character(),
        base::character())
ANNOTATION_DT_COLUMN_NAMES_CVEC <-
    base::c(
        'SHA256_value',
        'document_name',
        'sentence_ID',
        'segment_type',
        'conclusion_type',
        'comments')

ARC_CATEGORIES_DT_COLUMN_CLASSES_LST <-
    base::c(
        base::character(),
        base::character())
ARC_CATEGORY_FILE_NAME_REX_STR <-
    '^arcs_categories_(\\w+).tsv$'

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
input_write_annotation_table_template <- function(DOCUMENTS_DIR_PATH_STR=NULL, OUTPUT_DIR_PATH_STR=NULL, ANNOTATION_TABLE_FILE_PATH_STR=base::file.path(output_dir_path_str, 'annotation_table.tsv')) {
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

    annotation_dt <-
        data.table::data.table(
            SHA256_value=SHA256_VALUES_CVEC,
            document_name=DOCUMENT_DIR_NAMES_CVEC,
            sentence_ID=SENTENCE_IDS_IVEC,
            segment_type=NA_character_,
            conclusion_type=NA_character_,
            comments=NA_character_)
    if (!base::identical(
            base::colnames(annotation_dt),
            ANNOTATION_DT_COLUMN_NAMES_CVEC)) {
       base::stop('The column names used annotation table template are inconsistent with the predefined column names. ')
    }

    data.table::setorder(
        annotation_dt,
        document_name,
        sentence_ID)

    utils::write.table(
        annotation_dt,
        file=ANNOTATION_TABLE_FILE_PATH_STR,
        sep='\t',
        row.names=FALSE,
        col.names=TRUE,
        fileEncoding='UTF-8')

    return()
}

input_verify_annotation_table_consistency <- function(SENTENCE_FILES_PATHS_CVEC=NULL, ANNOTATION_DT=NULL, SEGMENT_TYPES_CVEC=NULL) {
    check_args(fun=input_verify_annotation_table_consistency)

    if (!base::identical(
            base::levels(ANNOTATION_DT[['segment_type']]),
            SEGMENT_TYPES_CVEC))
        base::stop(
            base::sprintf(
                'The annotation table contains segment_type levels {%s}, but you specified a different SEGMENT_TYPES_CVEC {%s}. ',
                base::paste0(
                    base::levels(
                        ANNOTATION_DT[['segment_type']]),
                    collapse=', '),
                base::paste0(
                    SEGMENT_TYPES_CVEC, collapse=', ')))

    ACTUAL_SHA256_VALUES_CVEC <-
        input_calculate_hash_values(FILES_PATHS_CVEC=SENTENCE_FILES_PATHS_CVEC)

    # TODO: use setorder() for efficiency.
    if (!base::identical(
            base::as.factor(ACTUAL_SHA256_VALUES_CVEC),
            ANNOTATION_DT[['SHA256_value']]))
        base::stop(
            base::sprintf(
                'The %d Alpino XML files listed in the annotation table are not of the same number and content as the %d XML files found in the documents directory. ',
                base::nrow(ANNOTATION_DT),
                base::length(ACTUAL_SHA256_VALUES_CVEC)))

    return()
}

#' @export
input_read_annotation_table <- function(ANNOTATION_TABLE_FILE_PATH_STR=NULL, DOCUMENTS_DIR_PATH_STR=NULL, SEGMENT_TYPES_CVEC=NULL) {
    check_args(fun=input_read_annotation_table)

    base::message(
        base::sprintf(
            "Reading annotation table at '%s' ... ",
            ANNOTATION_TABLE_FILE_PATH_STR))
    RELEVANTCOL_NAMES_BVEC <-
        ANNOTATION_DT_COLUMN_NAMES_CVEC != 'comments'

    annotation_dt <-
        data.table::fread(
            ANNOTATION_TABLE_FILE_PATH_STR,
            header=TRUE,
            sep='\t',
            encoding='UTF-8',
            col.names=ANNOTATION_DT_COLUMN_NAMES_CVEC[RELEVANTCOL_NAMES_BVEC],
            select=ANNOTATION_DT_COLUMN_NAMES_CVEC[RELEVANTCOL_NAMES_BVEC],
            colClasses=ANNOTATION_DT_COLUMN_CLASSES_LST,
            stringsAsFactors=TRUE)

    SENTENCE_FILES_PATHS_CVEC <-
        base::file.path(
            DOCUMENTS_DIR_PATH_STR,
            annotation_dt[['document_name']],
            base::sprintf(
                '%d.xml',
                annotation_dt[['sentence_ID']]))

    data.table::setkey(
        annotation_dt,
        document_name,
        sentence_ID)

    input_verify_annotation_table_consistency(
        SENTENCE_FILES_PATHS_CVEC=SENTENCE_FILES_PATHS_CVEC,
        ANNOTATION_DT=annotation_dt,
        SEGMENT_TYPES_CVEC=SEGMENT_TYPES_CVEC)

    return(annotation_dt)
}

input_read_arc_categories_table <- function(ARC_CATEGORIES_FILE_PATH_STR=NULL) {
    check_args(fun=input_read_arc_categories_table)

    base::message(
        base::sprintf(
            "Reading arc categories table at '%s'... ",
            ARC_CATEGORIES_FILE_PATH_STR))

    ARC_CATEGORY_DT <-
        data.table::fread(
            ARC_CATEGORIES_FILE_PATH_STR,
            header=TRUE,
            sep='\t',
            encoding='UTF-8',
            colClasses=ARC_CATEGORIES_DT_COLUMN_CLASSES_LST,
            stringsAsFactors=TRUE)

    return(base::list(ARC_CATEGORY_DT))
}

input_calculate_arc_category_prior_probability <- function(ARC_CATEGORY_DT=NULL)
    base::log(1.0 / base::nrow(ARC_CATEGORY_DT))

#' @export
input_read_arc_categories_tables <- function(DATA_DIR_PATH_STR=NULL) {
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
            base::basename(arc_categories_tables_files_paths_cvec),
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