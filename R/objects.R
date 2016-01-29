## To process objects from which features are to be extracted

#' To process an object
#'
#' @keywords file
#' @param OBJECT_ID_I The identifier of an object, as in the \emph{object_ID} column of \code{DT}.
#' @inheritParams objects_process_collection
#' @inheritParams objects_read_or_process_with_feature_representation
#' @inheritParams extract_features
#' @return \code{OBJECT_FEATURES_DVEC} Feature scores, with named elements.
#' @export
objects_process_object <- function(
    OBJECT_ID_I,
    DT,
    FEATURES_EXTRACTION_PARAMETERS,
    FEATURE_REPRESENTATION,
    OBJECT_OUTPUT_DIR_PATH_STR) {

    futile.logger::flog.debug(
        'Processing object with ID %d ... ',
        OBJECT_ID_I)

    RELATIVE_FILE_PATHS_CVEC <-
        DT[object_ID == OBJECT_ID_I,
           relative_file_path]

    OBJECT_FEATURES_DVEC <-
        extract_features(
            object=FEATURE_REPRESENTATION,
            FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS,
            OBJECT_OUTPUT_DIR_PATH_STR=OBJECT_OUTPUT_DIR_PATH_STR,
            OBJECT_ID_I=OBJECT_ID_I,
            RELATIVE_FILE_PATHS_CVEC=RELATIVE_FILE_PATHS_CVEC)

    return(OBJECT_FEATURES_DVEC)
}

#' To process objects according to a feature representation (performs filesystem output)
#'
#
#' @keywords file
#' @inheritParams objects_process_collection
#' @inheritParams objects_read_or_process_with_feature_representation
#' @param DESIGN_MATRIX_FILE_PATH_STR The path at which to write the design matrix RDS file.
#' @return \code{DESIGN_MATRIX} See \code{\link{objects_read_or_process_with_feature_representation}}.
#' @export
objects_process_with_feature_representation <- function(
    FEATURE_REPRESENTATION,
    DESIGN_MATRIX_FILE_PATH_STR,
    DT,
    FEATURES_EXTRACTION_PARAMETERS) {

    futile.logger::flog.debug(
        "Processing with feature representation '%s' ... ",
        FEATURE_REPRESENTATION@NAME_STR)

    UNIQUE_OBJECTS_IDS_IVEC <-
        base::unique(DT[['object_ID']])

    OBJECT_OUTPUT_DIR_PATHS_CVEC <-
        base::file.path(
            FEATURES_EXTRACTION_PARAMETERS@OUTPUT_DIR_PATH,
            FEATURE_REPRESENTATION@NAME_STR,
            UNIQUE_OBJECTS_IDS_IVEC)

    for (OBJECT_OUTPUT_DIR_PATH_STR in OBJECT_OUTPUT_DIR_PATHS_CVEC) {
        base::dir.create(OBJECT_OUTPUT_DIR_PATH_STR, showWarnings=FALSE, recursive=TRUE)
    }

    EXISTENT_DIR_PATHS_BVEC <-
        base::dir.exists(OBJECT_OUTPUT_DIR_PATHS_CVEC)

    if (!base::all(EXISTENT_DIR_PATHS_BVEC)) {
        base::stop(
            futile.logger::flog.fatal(
                "Failed to create all object output directories, for instance: '%s'. ",
                OBJECT_OUTPUT_DIR_PATHS_CVEC[!EXISTENT_DIR_PATHS_BVEC]))
    }

    OBJECT_FEATURES_DVEC_LST <-
        base::lapply(
            base::seq_along(UNIQUE_OBJECTS_IDS_IVEC),
            function(OBJECT_ID_I, ...)
                objects_process_object(
                    OBJECT_ID_I=OBJECT_ID_I,
                    OBJECT_OUTPUT_DIR_PATH_STR=OBJECT_OUTPUT_DIR_PATHS_CVEC[OBJECT_ID_I],
                    ...),
            DT=DT,
            FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS,
            FEATURE_REPRESENTATION=FEATURE_REPRESENTATION)
    OBJECT_FEATURES_DVECS_LENGTHS_IVEC <-
        base::lengths(OBJECT_FEATURES_DVEC_LST)
    OBJECTS_IDS_IVEC <-
        base::rep.int(
            base::seq_along(OBJECT_FEATURES_DVECS_LENGTHS_IVEC),
            times=OBJECT_FEATURES_DVECS_LENGTHS_IVEC)
    OBJECTS_FEATURES_DVEC <-
        base::unlist(
            OBJECT_FEATURES_DVEC_LST,
            recursive=FALSE,
            use.names=TRUE)
    OBJECTS_FEATURES_NAMES_CVEC <-
        base::names(OBJECTS_FEATURES_DVEC)
    OBJECTS_UNIQUE_FEATURES_NAMES_CVEC <-
        stringi::stri_unique(
            OBJECTS_FEATURES_NAMES_CVEC)
    OBJECTS_FEATURES_NAMES_IVEC <-
        base::match(
            OBJECTS_FEATURES_NAMES_CVEC,
            OBJECTS_UNIQUE_FEATURES_NAMES_CVEC)

    DESIGN_DMAT <-
        Matrix::sparseMatrix(
            i=OBJECTS_IDS_IVEC,
            j=OBJECTS_FEATURES_NAMES_IVEC,
            x=OBJECTS_FEATURES_DVEC,
            dims=base::c(
                base::length(OBJECT_FEATURES_DVECS_LENGTHS_IVEC),
                base::length(OBJECTS_UNIQUE_FEATURES_NAMES_CVEC)))
    base::colnames(DESIGN_DMAT) <-
        OBJECTS_UNIQUE_FEATURES_NAMES_CVEC

    if (FEATURE_REPRESENTATION@DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D < 1.0) {
        K_I <-
            base::as.integer(
                base::floor(
                    (base::min(
                        base::ncol(DESIGN_DMAT),
                        base::nrow(DESIGN_DMAT)) - 1L) *
                FEATURE_REPRESENTATION@DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D))
        SVD <-
            rARPACK::svds(
                DESIGN_DMAT,
                k=K_I,
                nv=K_I,
                nu=0L)
        LOWER_RANK_APPROXIMATION_DMAT <-
            DESIGN_DMAT %*% SVD[['v']]
    }

    DESIGN_MATRIX <-
        DesignMatrix(
            DMAT=DESIGN_DMAT,
            SVD=SVD,
            LOWER_RANK_APPROXIMATION_DMAT=LOWER_RANK_APPROXIMATION_DMAT)

    futile.logger::flog.info(
        "Saving design matrix for '%s' to '%s' ... ",
        FEATURE_REPRESENTATION@NAME_STR,
        DESIGN_MATRIX_FILE_PATH_STR)
    base::saveRDS(
        DESIGN_MATRIX,
        file=DESIGN_MATRIX_FILE_PATH_STR)

    return(DESIGN_MATRIX)
}

#' To process objects according to a feature representation
#'
#' \emph{This function performs filesystem input. }
#'
#' @keywords file
#' @inheritParams objects_process_collection
#' @param FEATURE_REPRESENTATION See: \code{\link{FeatureRepresentation}}.
#' @return \code{DESIGN_MATRIX} See: \code{\link{DesignMatrix}}.
#' @export
objects_read_or_process_with_feature_representation <- function(
    FEATURE_REPRESENTATION,
    DT,
    FEATURES_EXTRACTION_PARAMETERS) {

    DESIGN_MATRIX_FILE_PATH_STR <-
        base::file.path(
            FEATURES_EXTRACTION_PARAMETERS@OUTPUT_DIR_PATH,
            FEATURE_REPRESENTATION@NAME_STR,
            'DESIGN_MATRIX.rds')
    DESIGN_MATRIX <-
        base::tryCatch({
                DESIGN_MATRIX <-
                    base::readRDS(
                        file=DESIGN_MATRIX_FILE_PATH_STR)
                futile.logger::flog.debug(
                    'Reusing design matrix file. ')
                DESIGN_MATRIX
            },
            error=function(CONDITION) objects_process_with_feature_representation(
                FEATURE_REPRESENTATION=FEATURE_REPRESENTATION,
                DESIGN_MATRIX_FILE_PATH_STR=DESIGN_MATRIX_FILE_PATH_STR,
                DT=DT,
                FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS),
            warning=function(CONDITION) objects_process_with_feature_representation(
                FEATURE_REPRESENTATION=FEATURE_REPRESENTATION,
                DESIGN_MATRIX_FILE_PATH_STR=DESIGN_MATRIX_FILE_PATH_STR,
                DT=DT,
                FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS))

    futile.logger::flog.info(
        'Extracted %d ‘%s’ features, approximated in a lower-rank matrix with %d columns, with rows for %d observations. ',
        base::ncol(DESIGN_MATRIX@DMAT),
        FEATURE_REPRESENTATION@NAME_STR,
        base::ncol(DESIGN_MATRIX@LOWER_RANK_APPROXIMATION_DMAT),
        base::nrow(DESIGN_MATRIX@DMAT))

    return(DESIGN_MATRIX)
}

#' To process all objects in a collection with one or more feature representations
#'
#' @keywords file
#' @param DT The annotations table as a \code{\link[data.table]{data.table}}. See: \code{\link{Annotations}}.
#' @param FEATURES_EXTRACTION_PARAMETERS See: \code{\link{FeaturesExtractionParameters}}.
#' @param FEATURE_REPRESENTATIONS_LST See: \code{\link{SrfFeatureRepresentation}}, \code{\link{StrctFeatureRepresentation}}.
#' @return \code{DESIGN_MATRIX_LST} A list of \code{\link{DesignMatrix}} objects.
#' @export
objects_process_collection <- function(
    DT,
    FEATURES_EXTRACTION_PARAMETERS,
    FEATURE_REPRESENTATIONS_LST) {

    data.table::setkeyv(DT, 'object_ID')

    futile.logger::flog.info(
        "Processing documents collection in '%s' into '%s' ... ",
        FEATURES_EXTRACTION_PARAMETERS@DOCUMENTS_DIR_PATH,
        FEATURES_EXTRACTION_PARAMETERS@OUTPUT_DIR_PATH)

    DESIGN_MATRIX_LST <-
        base::lapply(
            FEATURE_REPRESENTATIONS_LST,
            objects_read_or_process_with_feature_representation,
            DT=DT,
            FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS)

    return(DESIGN_MATRIX_LST)
}