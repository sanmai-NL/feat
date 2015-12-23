# ' @exportClass Annotations
# TODO: validate
# TODO: R 3.2.1 bug does not allow exportClass
#' An annotations table.
#'
#' This table (in TSV format) must at minimum contain SHA256 hash values in column \emph{SHA256_value} and file paths relative to a single documents root directory that point to XML documents in column \emph{relative_file_paths}.
#'
#' @slot DT A \code{\link[data.table]{data.table}} containing annotations, with rows as observations and columns as variables.
#' @importClassesFrom data.table data.table
#' @export Annotations
#' @exportClass Annotations
Annotations <-
    methods::setClass(
        Class='Annotations',
        slots=base::list(
            DT='data.table'),
        prototype=base::list(DT=NULL),
        sealed=TRUE)
