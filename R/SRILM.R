## To use SRILM command-line utilities.
#' @keywords internal file
NULL

COUNTS_FILE_COLCLASSES_CVEC <-
    base::c('character', 'character')
PERPLEXITY_LINES_REX_STR <-
    'ppl1= [-]?(\\d+)[\\.]?(\\d+)$'

SRILM_count_and_write_ARPA_language_model <- function(
    COUNTS_FILE_PATH_STR=
        stringi::stri_join(
            TEXT_FILE_PATH_STR,
            '.counts'),
    LANGUAGE_MODEL_FILE_PATH_STR=
        stringi::stri_join(
            COUNTS_FILE_PATH_STR,
            '.arpa'),
    SRILM_NGRAMCOUNT_PARAMETERS_CVEC,
    TEXT_FILE_PATH_STR) {

    if (!(base::file.exists(LANGUAGE_MODEL_FILE_PATH_STR) &&
        base::file.exists(COUNTS_FILE_PATH_STR))) {
        ## WARNING: At least SRILM 1.7.1 does not return a non-zero exit status on at
        ## least some IO errors. That's the reason for this additional TEXT_FILE_PATH_STR
        ## file opening check.
        base::tryCatch({
                TEXT_FILE <-
                    base::file(TEXT_FILE_PATH_STR)
                base::open(TEXT_FILE)
            },
            error=function(CONDITION)
                base::stop(
                    futile.logger::flog.error(
                        "Text file at '%s' could not be opened. ",
                        TEXT_FILE_PATH_STR)),
            finally={ base::close(TEXT_FILE) })

        EXIT_STATUS_I <-
            base::system2(
                'ngram-count',
                args=base::c(
                    SRILM_NGRAMCOUNT_PARAMETERS_CVEC,
                    '-lm', LANGUAGE_MODEL_FILE_PATH_STR,
                    '-text', TEXT_FILE_PATH_STR,
                    '-write', COUNTS_FILE_PATH_STR),
                stderr=FALSE)
        if (EXIT_STATUS_I != 0L) {
            base::stop(
                futile.logger::flog.fatal(
                    "Error running 'ngram-count'. Its exit status: %d. ",
                    EXIT_STATUS_I))
        }
    } else {
        futile.logger::flog.debug(
            'Reusing counts and language model files. ')
    }

    COUNTS_AND_LM <-
        CountsAndLM(
            LANGUAGE_MODEL_FILE_PATH=
                FilesystemPath(LANGUAGE_MODEL_FILE_PATH_STR),
            COUNTS_FILE_PATH=
                FilesystemPath(COUNTS_FILE_PATH_STR))

    return(COUNTS_AND_LM)
}

SRILM_write_ARPA_language_model <- function(
    COUNTS_FILE_PATH_STR,
    LANGUAGE_MODEL_FILE_PATH_STR=
        stringi::stri_join(
            COUNTS_FILE_PATH_STR,
            '.arpa'),
    SRILM_NGRAMCOUNT_PARAMETERS_CVEC) {

    if (!(base::file.exists(LANGUAGE_MODEL_FILE_PATH_STR) &&
        base::file.exists(COUNTS_FILE_PATH_STR))) {
        EXIT_STATUS_I <-
            base::system2(
                'ngram-count',
                base::c(
                    SRILM_NGRAMCOUNT_PARAMETERS_CVEC,
                    '-lm', LANGUAGE_MODEL_FILE_PATH_STR,
                    '-read', COUNTS_FILE_PATH_STR),
                stderr=FALSE)
        if (EXIT_STATUS_I != 0L) {
            base::stop(
                futile.logger::flog.fatal(
                    "Error running 'ngram-count'. Its exit status: %d.",
                    EXIT_STATUS_I))
        }
    } else {
        futile.logger::flog.debug(
            'Reusing counts and language model files. ')
    }
    COUNTS_AND_LM <-
        CountsAndLM(
            LANGUAGE_MODEL_FILE_PATH=
                FilesystemPath(LANGUAGE_MODEL_FILE_PATH_STR),
            COUNTS_FILE_PATH=
                FilesystemPath(COUNTS_FILE_PATH_STR))

    return(COUNTS_AND_LM)
}

SRILM_extract_features_vector <- function(
    FEATURES_DVEC_FILE_PATH_STR,
    COUNTS_AND_LM,
    SEQUENCES_FILE_PATH_STR,
    SRILM_NGRAM_PARAMETERS_CVEC) {

    if (!base::file.exists(SEQUENCES_FILE_PATH_STR)) {
        ## Strip last field of counts file to get sequence strings.
        # TODO: suppressing warnings because of bug: https://github.com/Rdatatable/data.table/issues/1124
        # TODO: handle features containing \t. SRILM uses \t as well, so acceptable as separator?
        SEQUENCES_CVEC <-
            base::suppressWarnings(
                data.table::fread(
                    colClasses=COUNTS_FILE_COLCLASSES_CVEC,
                    encoding='UTF-8',
                    header=FALSE,
                    input=COUNTS_AND_LM@COUNTS_FILE_PATH,
                    select=1L,
                    skip=0L,
                    sep='\t',
                    stringsAsFactors=FALSE,
                    quote=''))
        base::cat(
            SEQUENCES_CVEC[[1L]],
            file=SEQUENCES_FILE_PATH_STR,
            sep='\n')
    } else {
        futile.logger::flog.debug(
            'Reusing sequences file. ')
    }

    futile.logger::flog.debug(
        "Using 'ngram' on the sequences file '%s' and LM file '%s' ... ",
        SEQUENCES_FILE_PATH_STR,
        COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH)

    ## Calculate score on each sequence.
    NGRAM_OUTPUT_LINES_CVEC <-
        base::tryCatch(
            # TODO: Allow logging to file.
            base::system2(
                'ngram',
                base::c(
                    SRILM_NGRAM_PARAMETERS_CVEC,
                    '-debug', '4',
                    '-lm', COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH,
                    '-ppl', SEQUENCES_FILE_PATH_STR),
                stdout=TRUE,
                stderr=FALSE),
            error=base::stop,
            warning=base::stop)
    # TODO: ngram does not return an error exit status, apparently. Fix upstream.
    # TODO: Replace with library calls once available.
    ## There seems to be a defect in futile.logger: it does not close all connections it opens.
    base::closeAllConnections()

    ## Get score after 'ppl1= ' in ngram output. Remove last element, which is the total score.
    PERPLEXITY_SCORES_CVEC <-
        stringi::stri_extract_last_words(
            stringi::stri_subset_fixed(
                NGRAM_OUTPUT_LINES_CVEC,
                pattern='ppl1= '))

    ## Very small scores are 'undefined'. They will be coerced to NA.
    ## Take reciprocal of perplexity so that 0 scores indicate feature absence.
    # TODO: Make scoring function configurable.
    features_dvec <-
        (1.0 / base::as.numeric(PERPLEXITY_SCORES_CVEC))[-base::length(PERPLEXITY_SCORES_CVEC)]

    SEQUENCES_CVEC <-
        base::readLines(
            SEQUENCES_FILE_PATH_STR,
            encoding='UTF-8')
    base::names(features_dvec) <- SEQUENCES_CVEC
    features_dvec <- features_dvec[!base::is.na(features_dvec)]

    if (!base::file.exists(FEATURES_DVEC_FILE_PATH_STR)) {
        base::saveRDS(
            features_dvec,
            file=FEATURES_DVEC_FILE_PATH_STR)
    }

    return(features_dvec)
}

SRILM_read_or_extract_features_vector <- function(
    COUNTS_AND_LM,
    FEATURES_DVEC_FILE_PATH_STR=stringi::stri_join(
        SEQUENCES_FILE_PATH_STR,
        '.features_dvec.rds'),
    SEQUENCES_FILE_PATH_STR=
        stringi::stri_join(
            COUNTS_AND_LM@COUNTS_FILE_PATH,
            '.seq'),
    SRILM_NGRAM_PARAMETERS_CVEC) {

    FEATURES_DVEC <-
        base::tryCatch({
            FEATURES_DVEC <-
                base::readRDS(
                    file=FEATURES_DVEC_FILE_PATH_STR)
            futile.logger::flog.debug(
                'Reusing features vector file. ')
            FEATURES_DVEC
        },
        error=function(CONDITION) SRILM_extract_features_vector(
            COUNTS_AND_LM=COUNTS_AND_LM,
            FEATURES_DVEC_FILE_PATH_STR=FEATURES_DVEC_FILE_PATH_STR,
            SEQUENCES_FILE_PATH_STR=SEQUENCES_FILE_PATH_STR,
            SRILM_NGRAM_PARAMETERS_CVEC=SRILM_NGRAM_PARAMETERS_CVEC),
        warning=function(CONDITION) SRILM_extract_features_vector(
            COUNTS_AND_LM=COUNTS_AND_LM,
            FEATURES_DVEC_FILE_PATH_STR=FEATURES_DVEC_FILE_PATH_STR,
            SEQUENCES_FILE_PATH_STR=SEQUENCES_FILE_PATH_STR,
            SRILM_NGRAM_PARAMETERS_CVEC=SRILM_NGRAM_PARAMETERS_CVEC))

    return(FEATURES_DVEC)
}