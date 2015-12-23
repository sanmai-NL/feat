#' @include FilesystemPath-class.R
#' @keywords internal
CountsAndLM <-
    methods::setClass(
        Class='CountsAndLM',
        slots=base::list(
            LANGUAGE_MODEL_FILE_PATH='FilesystemPath',
            COUNTS_FILE_PATH='FilesystemPath'),
        prototype=base::list(
            LANGUAGE_MODEL_FILE_PATH=NULL,
            COUNTS_FILE_PATH=NULL),
        sealed=TRUE)