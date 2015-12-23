## To read annotations table.
#' @keywords internal file

metadata_calculate_SHA256_hash_values <- function(
    FILES_PATHS_CVEC)
    base::vapply(
        FILES_PATHS_CVEC,
        FUN=digest::digest,
        algo='sha256',
        file=TRUE,
        FUN.VALUE=base::character(length=1L),
        USE.NAMES=FALSE)

#' To write an annotations table template
#'
#' This template should be manually annotated for feature extraction and supervised learning purposes.
#'
#' @inheritParams metadata_verify_annotation_table_consistency
#' @inheritParams metadata_read_annotations_table
#' @export
metadata_write_annotations_table_template <- function(
    ANNOTATIONS_TABLE_FILE_PATH_STR,
    DOCUMENTS_DIR_PATH_STR) {

    futile.logger::flog.info(
        "Writing annotation table template to '%s' ... ",
        ANNOTATIONS_TABLE_FILE_PATH_STR)

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
        metadata_calculate_SHA256_hash_values(
            FILES_PATHS_CVEC=SENTENCE_FILES_PATHS_CVEC)

    annotation_dt <-
        data.table::data.table(
            SHA256_value=SHA256_VALUES_CVEC,
            document_name=DOCUMENT_DIR_NAMES_CVEC,
            sentence_ID=SENTENCE_IDS_IVEC)
    # TODO: Generate file paths.
    data.table::setorderv(
        annotation_dt,
        'document_name',
        'sentence_ID')

    utils::write.table(
        col.names=TRUE,
        file=ANNOTATIONS_TABLE_FILE_PATH_STR,
        fileEncoding='UTF-8',
        row.names=FALSE,
        sep='\t',
        x=annotation_dt)

    return()
}

#' To verify an annotations table template
#'
#' Performs a consistency check on the annotations table by comparing the SHA256 hash values in column \emph{SHA256_value} the annotations table with actual SHA256 hash alue of the respective XML document files under \code{DOCUMENTS_DIR_PATH_STR}.
#'
#' @include Annotations-class.R
#' @param ANNOTATIONS_DT The annotations \code{\link[data.table]{data.table}}. See: \link{Annotations}.
#' @param DOCUMENTS_DIR_PATH_STR The root documents directory path.
#' @return \emph{Signals an error if not verified.}
#' @export
metadata_verify_annotation_table_consistency <- function(
    ANNOTATIONS_DT,
    DOCUMENTS_DIR_PATH_STR) {

    SENTENCE_FILES_PATHS_CVEC <-
        base::file.path(
            DOCUMENTS_DIR_PATH_STR,
            ANNOTATIONS_DT[['document_name']],
            base::sprintf(
                '%d.xml',
                ANNOTATIONS_DT[['sentence_ID']]))

    ACTUAL_SHA256_VALUES_CVEC <-
        base::sort(
            metadata_calculate_SHA256_hash_values(
                FILES_PATHS_CVEC=SENTENCE_FILES_PATHS_CVEC))
    # TODO: (efficiency) Replace the hash check something better and more efficient.
    # TODO: (efficiency) Use setorder().
    if (!base::identical(
            ACTUAL_SHA256_VALUES_CVEC,
            # TODO: (efficieny) performance of `[[` operator with data.table?
            base::sort(ANNOTATIONS_DT[['SHA256_value']])))

        base::stop(
            futile.logger::flog.fatal(
                'The %d Alpino XML files listed in the annotation table are not of the same number and content as the %d XML files found in the documents directory. ',
                base::nrow(ANNOTATIONS_DT),
                base::length(ACTUAL_SHA256_VALUES_CVEC)))

    return()
}

#' To read an annotations table template
#'
#' This template should be manually annotated for feature extraction and supervised learning purposes.
#'
#' @include Annotations-class.R
#' @param ANNOTATIONS_TABLE_FILE_PATH_STR The annotations table file path.
#' @param KEPT_ANNOTATIONS_DT_COLUMN_NAMES_CVEC The names of the columns of the annotations table to preserve in the return value.
#' @param ... Extra arguments passed on to \code{\link[data.table]{fread}}.
#' @return \code{ANNOTATIONS} See: \link{Annotations}.
#' @export
metadata_read_annotations_table <- function(
    ANNOTATIONS_TABLE_FILE_PATH_STR,
    KEPT_ANNOTATIONS_DT_COLUMN_NAMES_CVEC,
    ...) {

    futile.logger::flog.info(
        "Reading annotation table at '%s' ... ",
        ANNOTATIONS_TABLE_FILE_PATH_STR)

    ANNOTATIONS_DT <-
        data.table::fread(
            encoding='UTF-8',
            header=TRUE,
            input=ANNOTATIONS_TABLE_FILE_PATH_STR,
            select=KEPT_ANNOTATIONS_DT_COLUMN_NAMES_CVEC,
            sep='\t',
            ## TODO: bug in data.table's S3 method for stats::omit.na: does not work if NA is contained within factor.
            stringsAsFactors=FALSE)

    return(Annotations(DT=ANNOTATIONS_DT))
}