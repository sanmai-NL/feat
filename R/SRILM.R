## To use SRILM command-line utilities.
#' @keywords internal file
NULL

COUNTS_FILE_COLCLASSES_CVEC <-
    base::c('character', 'integer')
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

        NGRAM_COUNT_ARGUMENTS_CVEC <-
            base::c(
                SRILM_NGRAMCOUNT_PARAMETERS_CVEC,
                '-lm', base::shQuote(LANGUAGE_MODEL_FILE_PATH_STR),
                '-text', base::shQuote(TEXT_FILE_PATH_STR),
                '-write', base::shQuote(COUNTS_FILE_PATH_STR))

        EXIT_STATUS_I <-
            base::system2(
                'ngram-count',
                args=NGRAM_COUNT_ARGUMENTS_CVEC,
                stderr=FALSE)
        if (EXIT_STATUS_I != 0L) {
            base::stop(
                futile.logger::flog.fatal(
                    "Error running 'ngram-count %s'. Its exit status: %d. ",
                    stringi::stri_join(
                        NGRAM_COUNT_ARGUMENTS_CVEC,
                        sep='',
                        collapse=' '),
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

    if (base::file.exists(COUNTS_FILE_PATH_STR)) {
        if (!(base::file.exists(LANGUAGE_MODEL_FILE_PATH_STR))) {
            if (base::file.info(COUNTS_FILE_PATH_STR)[['size']] > 0L) {
                NGRAM_COUNT_ARGUMENTS_CVEC <-
                    base::c(
                        SRILM_NGRAMCOUNT_PARAMETERS_CVEC,
                        '-lm', base::shQuote(LANGUAGE_MODEL_FILE_PATH_STR),
                        '-read', base::shQuote(COUNTS_FILE_PATH_STR))
                EXIT_STATUS_I <-
                    base::system2(
                        'ngram-count',
                        args=NGRAM_COUNT_ARGUMENTS_CVEC,
                        stderr=FALSE)
                if (EXIT_STATUS_I != 0L) {
                    base::stop(
                        futile.logger::flog.fatal(
                            "Error running 'ngram-count %s'. Its exit status: %d. ",
                            stringi::stri_join(
                                NGRAM_COUNT_ARGUMENTS_CVEC,
                                sep='',
                                collapse=' '),
                            EXIT_STATUS_I))
                }
            } else {
                futile.logger::flog.debug(
                    'Skipping empty counts file. ')
                return(NULL)
            }
        } else {
            futile.logger::flog.debug(
                'Reusing counts and language model files. ')
        }
    } else {
        base::stop(
            futile.logger::flog.fatal(
                "The counts file at '%s' specified does not exist",
                COUNTS_FILE_PATH_STR))
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
    SCORING_STR,
    SEQUENCES_FILE_PATH_STR,
    SRILM_NGRAM_PARAMETERS_CVEC) {

    if (!base::file.exists(SEQUENCES_FILE_PATH_STR)) {
        ## Strip last field of counts file to get sequence strings.
        # TODO: suppressing warnings because of bug: https://github.com/Rdatatable/data.table/issues/1124
        # TODO: handle features containing \t. SRILM uses \t as well, so acceptable as separator?
        SEQUENCES_CVEC <-
            base::tryCatch({
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
                    quote=''))[[1L]] },
            error=function(CONDITION) {
                    if (base::file.info(
                        COUNTS_AND_LM@COUNTS_FILE_PATH)[['size']] == 0L) {
                        futile.logger::flog.warn(
                            "The counts file '%s' turned out empty. Assuming it is still to be written, so pausing for 10 seconds. ")
                        base::Sys.sleep(10L)
                        ## TODO: use restarts.
                        base::Recall(
                            FEATURES_DVEC_FILE_PATH_STR=FEATURES_DVEC_FILE_PATH_STR,
                            COUNTS_AND_LM=COUNTS_AND_LM,
                            SEQUENCES_FILE_PATH_STR=SEQUENCES_FILE_PATH_STR,
                            SRILM_NGRAM_PARAMETERS_CVEC=SRILM_NGRAM_PARAMETERS_CVEC)
                    }
                })
            ## Suppress warnings. data.table generates spurious warnings on short files. See: https://github.com/Rdatatable/data.table/issues/1124, but fill=TRUE is not the solution.)
        base::stopifnot(base::length(SEQUENCES_CVEC) >= 1L)
        base::cat(
            SEQUENCES_CVEC,
            file=SEQUENCES_FILE_PATH_STR,
            sep='\n')
    } else {
        futile.logger::flog.debug(
            'Reusing sequences file. ')
        SEQUENCES_CVEC <-
            base::readLines(
                SEQUENCES_FILE_PATH_STR,
                encoding='UTF-8')
    }

    futile.logger::flog.debug(
        "Using 'ngram' on the sequences file '%s' and LM file '%s' ... ",
        SEQUENCES_FILE_PATH_STR,
        COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH)
    # TODO: ngram does not return an error exit status, apparently. Fix upstream.
    # TODO: Replace with library calls once available.
    ## There seems to be a defect in futile.logger: it does not close all connections it opens.
    base::closeAllConnections()

    if (SCORING_STR == 'Pr') {
        NGRAM_ARGUMENTS_CVEC <-
            base::c(
                SRILM_NGRAM_PARAMETERS_CVEC,
                '-debug', '4',
                '-lm', base::shQuote(COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH),
                '-ppl', base::shQuote(SEQUENCES_FILE_PATH_STR))

        futile.logger::flog.debug(
            "Running 'ngram %s' ...",
            stringi::stri_join(
                NGRAM_ARGUMENTS_CVEC,
                sep='',
                collapse=' '))

        ## Calculate score on each sequence.
        NGRAM_OUTPUT_LINES_CVEC <-
            base::tryCatch({
                # TODO: Allow logging to file.
                base::system2(
                    'ngram',
                    NGRAM_ARGUMENTS_CVEC,
                    stdout=TRUE,
                    stderr=FALSE) },
                error=base::stop,
                warning=base::stop)

        ## Get score after 'ppl1= ' in ngram output.
        LOGPROB_MATCH_CMAT_LST <-
            stringi::stri_match_all_regex(
                NGRAM_OUTPUT_LINES_CVEC,
                pattern='logprob= ([\\-\\deE\\.]+) ',
                omit_no_match=TRUE)

        LOGPROB_MATCH_CMAT_LST <-
            LOGPROB_MATCH_CMAT_LST[lengths(LOGPROB_MATCH_CMAT_LST) == 2L]

        LOGPROB_MATCH_CMAT <-
            stringi::stri_list2matrix(
                LOGPROB_MATCH_CMAT_LST,
                byrow=TRUE)

        # TODO: Make scoring function configurable. Invert log probability to probability.

        # TODO: is high-precision math helpful here?
        # E.g. Rmpfr::mpfr(10, 64)^-3.20177e+06
        # Issues: 1. extra dependencies; 2. Other tools may not support the type of data/representation as string.
        features_dvec <-
            10.0 ^ base::as.numeric(
                ## Do not select last row, which contains the total score.
            LOGPROB_MATCH_CMAT[base::seq_along(SEQUENCES_CVEC), 2L])

        ## Very small scores are 'undefined'. They will be coerced to NA.
        if (base::anyNA(features_dvec) ||
            base::any(
                base::is.nan(features_dvec),
                base::is.infinite(features_dvec))) {
            base::stop(
                futile.logger::flog.fatal(
                    'Could not calculate at least one feature score correctly. '))
        ## Accept current calculation as 'automatic feature score tresholding in case of
        ## a high number of different features per object'. Using a logprob instead has
        ## important effects on the scale and spread of feature scores that could be considered first.
        #else if (base::any(features_dvec == 0.0)) {
        #     futile.logger::flog.warn(
        #         'Zero feature scores induced. Suggestion: the log probability of some features
        ## is so large in magnitude (low) that the fractional part of the probability will be treated as zero. ')
        # }
        }
    } else if (SCORING_STR == 'boolean') {
        ## Use 1 integers instead of TRUE booleans because downstream code generally expects numeric matrices (e.g. SVD functions).
        features_dvec <-
            base::rep.int(
                x=1L,
                times=base::length(SEQUENCES_CVEC))
    } else {
        base::stop(futile.logger::flog.fatal('Scoring function unknown. '))
    }

    base::names(features_dvec) <- SEQUENCES_CVEC

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
    SCORING_STR,
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
            SCORING_STR=SCORING_STR,
            SRILM_NGRAM_PARAMETERS_CVEC=SRILM_NGRAM_PARAMETERS_CVEC),
        warning=function(CONDITION) SRILM_extract_features_vector(
            COUNTS_AND_LM=COUNTS_AND_LM,
            FEATURES_DVEC_FILE_PATH_STR=FEATURES_DVEC_FILE_PATH_STR,
            SEQUENCES_FILE_PATH_STR=SEQUENCES_FILE_PATH_STR,
            SCORING_STR=SCORING_STR,
            SRILM_NGRAM_PARAMETERS_CVEC=SRILM_NGRAM_PARAMETERS_CVEC))

    return(FEATURES_DVEC)
}