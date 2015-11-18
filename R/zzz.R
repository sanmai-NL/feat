# TODO: move into separate package
# TODO: make check_args work without specifying function
check_args <- function(fun=NULL, .pos=parent.frame(), .fun_formals=base::formals(fun), .ls.str=utils::ls.str(.pos)) {
    if (base::any(
            base::vapply(
                base::mget(
                	base::names(.fun_formals),
                	envir=base::as.environment(.pos)),
                FUN=base::is.null,
                FUN.VALUE=base::logical(length=1L)))) {
        base::print(.ls.str)
        base::stop('A required function argument was not supplied or specified (i.e., has a value of NULL). ')
    }
}