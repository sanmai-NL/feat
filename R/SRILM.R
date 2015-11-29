#' To use SRILM command-line utilities.
#'
#' @name feat-SRILM
NULL

PERPLEXITY_LINES_REX_STR <-
    '^\\d+ zeroprobs, logprob= .+ ppl= .+ ppl1= (.+)$'
SEQUENCES_DT_HEADER_CVEC <-
    base::c('sequence')

methods::setClass(
    'CountsAndLM',
    methods::representation(
        LANGUAGE_MODEL_FILE_PATH_STR='character',
        COUNTS_FILE_PATH_STR='character'),
    sealed=TRUE)
methods::setClass(
    'SequencesAndScores',
    methods::representation(
        SEQUENCES_FILE_PATH_STR='character',
        SEQUENCES_SCORES_DVEC_FILE_PATH_STR='character'),
    sealed=TRUE)

SRILM_count_and_write_ARPA_language_model <- function(TEXT_FILE_PATH_STR=NULL, COUNTS_FILE_PATH_STR=base::paste0(TEXT_FILE_PATH_STR, '.counts'), LANGUAGE_MODEL_FILE_PATH_STR=base::paste0(COUNTS_FILE_PATH_STR, '.arpa'), PARAMETERS=NULL) {
    check_args(fun=SRILM_count_and_write_ARPA_language_model)

    COUNTS_AND_LM <-
        methods::new(
            'CountsAndLM',
            LANGUAGE_MODEL_FILE_PATH_STR=LANGUAGE_MODEL_FILE_PATH_STR,
            COUNTS_FILE_PATH_STR=COUNTS_FILE_PATH_STR)

    if (!base::file.exists(COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH_STR) ||
        !base::file.exists(COUNTS_AND_LM@COUNTS_FILE_PATH_STR)) {
        ## WARNING: SRILM 1.7.1 has a bug that it does not return a non-zero exit status on at least some errors. That's the reason for the additional TEXT_FILE_PATH_STR check.
        base::stopifnot(base::file.exists(TEXT_FILE_PATH_STR))
        EXIT_STATUS <-
            base::system2(
                'ngram-count',
                args=base::c(
                    PARAMETERS,
                    '-lm', COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH_STR,
                    '-text', TEXT_FILE_PATH_STR,
                    '-write', COUNTS_AND_LM@COUNTS_FILE_PATH_STR))
        if (EXIT_STATUS != 0L)
            base::stop(
                base::sprintf(
                    "Error running 'ngram-count'. Exit status: %d. ",
                    EXIT_STATUS))
    }

    return(COUNTS_AND_LM)
}

SRILM_write_ARPA_language_model <- function(COUNTS_FILE_PATH_STR=NULL, LANGUAGE_MODEL_FILE_PATH_STR=base::paste0(COUNTS_FILE_PATH_STR, '.arpa'), PARAMETERS=NULL) {
    check_args(fun=SRILM_write_ARPA_language_model)

    COUNTS_AND_LM <-
        methods::new(
            'CountsAndLM',
            LANGUAGE_MODEL_FILE_PATH_STR=LANGUAGE_MODEL_FILE_PATH_STR,
            COUNTS_FILE_PATH_STR=COUNTS_FILE_PATH_STR)

    if (!base::file.exists(COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH_STR) ||
        !base::file.exists(COUNTS_AND_LM@COUNTS_FILE_PATH_STR)) {
        EXIT_STATUS <-
            base::system2(
                'ngram-count',
                base::c(
                    PARAMETERS,
                    '-lm', COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH_STR,
                    '-read', COUNTS_AND_LM@COUNTS_FILE_PATH_STR))
        if (EXIT_STATUS != 0L)
            base::stop(
                base::sprintf(
                    "Error running 'ngram-count'. Exit status: %d.",
                    EXIT_STATUS))
    }

    return(COUNTS_AND_LM)
}

SRILM_write_feature_scores <- function(COUNTS_AND_LM=NULL, SEQUENCES_FILE_PATH_STR=base::paste0(COUNTS_AND_LM@COUNTS_FILE_PATH_STR, '.seq'), SEQUENCES_SCORES_DVEC_FILE_PATH_STR=base::paste0(SEQUENCES_FILE_PATH_STR, '.scores.rds'), SEED_I=NULL) {
    check_args(fun=SRILM_write_feature_scores)

    if (!base::file.exists(SEQUENCES_SCORES_DVEC_FILE_PATH_STR) ||
        !base::file.exists(SEQUENCES_FILE_PATH_STR)) {
        base::message(
            base::sprintf(
                "Writing feature scores to \n'%s'\n using 'ngram' on the counts file \n'%s'\n transformed to sequences file \n'%s'\n and language model file \n'%s'\n ...",
                SEQUENCES_SCORES_DVEC_FILE_PATH_STR,
                COUNTS_AND_LM@COUNTS_FILE_PATH_STR,
                SEQUENCES_FILE_PATH_STR,
                COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH_STR))

        ## Strip last field of counts file to get sequence strings.
        EXIT_STATUS <-
            base::system2(
                'awk',
                base::c(
                    "-F$'\t'", "'sub(FS $NF,x)'",
                    COUNTS_AND_LM@COUNTS_FILE_PATH_STR),
            stdout=SEQUENCES_FILE_PATH_STR)
        if (EXIT_STATUS != 0L)
            base::stop(
                base::sprintf(
                    "Error running 'awk'. Exit status: %d",
                    EXIT_STATUS))

        ## Calculate score on each sequence.
        NGRAM_OUTPUT_LINES_CVEC <-
            base::tryCatch(
                # TODO: add logging
                base::system2(
                    'ngram',
                    base::c(
                        # TODO: parameterize
                        '-no-sos', '-no-eos',
                        '-debug', '1',
                        '-seed', SEED_I,
                        '-expand-exact', '1',
                        '-lm', COUNTS_AND_LM@LANGUAGE_MODEL_FILE_PATH_STR,
                        '-ppl', SEQUENCES_FILE_PATH_STR),
                    stdout=TRUE),
                error=base::stop,
                warning=base::stop)

        ## Take reciprocal of perplexity so that 0 scores indicate feature absence.
        perplexities_rex_dvec <-
            base::t(
                1.0 / base::as.numeric(
                    base::c(
                        stats::na.omit(
                            stringi::stri_match_first_regex(
                                NGRAM_OUTPUT_LINES_CVEC,
                                pattern=PERPLEXITY_LINES_REX_STR))[, 2L])))
        sequences_scores_dvec <-
            perplexities_rex_dvec[-base::length(perplexities_rex_dvec)]

        SEQUENCES_CVEC <-
            base::readLines(
                SEQUENCES_FILE_PATH_STR,
                encoding='UTF-8')
        base::names(sequences_scores_dvec) <- SEQUENCES_CVEC

        base::saveRDS(
            sequences_scores_dvec,
            file=SEQUENCES_SCORES_DVEC_FILE_PATH_STR)
    }

    SEQUENCES_AND_SCORES <-
        methods::new(
            'SequencesAndScores',
            SEQUENCES_FILE_PATH_STR=SEQUENCES_FILE_PATH_STR,
            SEQUENCES_SCORES_DVEC_FILE_PATH_STR=SEQUENCES_SCORES_DVEC_FILE_PATH_STR)

    return(SEQUENCES_AND_SCORES)
}