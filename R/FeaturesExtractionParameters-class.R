#' @include FilesystemPath-class.R
NULL

#' Parameters of the feature extraction job
#'
#' @slot ANNOTATIONS_TABLE_FILE_PATH The file path to the annotations table. See: \code{\link{Annotations}}, \code{\link{metadata_write_annotations_table_template}}, \code{\link{metadata_verify_annotation_table_consistency}}, \code{\link{metadata_read_annotations_table}}.
#' @slot DOCUMENTS_DIR_PATH The root directory path containing the XML documents representing observations. To this the \code{relative_file_path} column in annotation tables must be relative.
#' @slot JOB_NAME_STR A name for the particular feature extraction job.
#' @slot OUTPUT_DIR_PATH The root directory path for any filesystem output.
#' @export FeaturesExtractionParameters
#' @exportClass FeaturesExtractionParameters
FeaturesExtractionParameters <-
    methods::setClass(
        Class='FeaturesExtractionParameters',
        slots=base::list(
            ANNOTATIONS_TABLE_FILE_PATH='FilesystemPath',
            DOCUMENTS_DIR_PATH='FilesystemPath',
            JOB_NAME_STR='character',
            OUTPUT_DIR_PATH='FilesystemPath'),
        prototype=base::list(
            ANNOTATIONS_TABLE_FILE_PATH=FilesystemPath(base::tempdir()),
            DOCUMENTS_DIR_PATH=FilesystemPath(base::tempdir()),
            JOB_NAME_STR=NULL,
            OUTPUT_DIR_PATH=FilesystemPath(base::tempdir())),
        sealed=TRUE)