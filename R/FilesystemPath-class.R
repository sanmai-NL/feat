# FilesystemPath <-
#' A string representing a filesystem path
#'
#' @export FilesystemPath
#' @exportClass FilesystemPath
FilesystemPath <-
    methods::setClass(
        Class='FilesystemPath',
        contains='character',
        validity=function(object) base::length(
            base::normalizePath(
                object,
                mustWork=TRUE)) == 1L,
        sealed=TRUE)