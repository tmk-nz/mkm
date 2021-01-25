#' Parse
#'
#' @param x An MKM object, similar to that produced by \code{\link{read_mkm}}
#' @param definition A definition object
#' @param .default The defult parsing function to use, if none is specified
#'
#' @return
#' @export
parse_mkm <- function(x, definition = NULL, .default = "to_df"){
    # Setup / check parse list
    pl <- lapply(definition, .get_parse_fn)

    out <- list()
    nmes <- names(x)

    for(a in seq_along(nmes)){

        nme <- nmes[a]
        fn <- pl[[nme]]

        if(rlang::is_null(fn)){
            if(nme == "meta"){
                fn <- to_meta
            } else {
                fn <- .get_parse_fn(.default) # Default
            }
        }

        res <- try(fn(x[[nme]]), silent = TRUE)

        if(!inherits(res, "try-error")){
            out[[nme]] <- res
        } else {
            nmspce <- rlang::env_label(rlang::get_env(fn))
            # It is hard (impossible) to get sensible names for functions at
            # this point.
            # Note to future self: Don't try unless you have lots of spare
            # time.

            msg <- paste0("Error parsing section `%s`\n",
                          "    %s")

            msg <- sprintf(msg, nme, attr(res, "condition")$message)
            stop(msg, call. = FALSE)
        }

    }
    return(out)
}

#' Coerce MKM objects to base R types
#'
#' to_* functions take raw input (which is a list with 'text' and 'format' components) and produce a base R type (list, dataframe or matrix)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @aliases to_lst to_tlst to_meta to_df to_tdf to_mat to_tmat
to_lst <- function(x){
    x <- .raw_dat(x)
    .to_list(x)
}

#' @export
to_tlst <- function(x){
    x <- .raw_dat(x)
    .to_list(.t(x))
}

#' @export
to_meta <- function(x){
    x <- to_tlst(x)
    set_keys(x)
}

#' @export
to_df <- function(x){
    x <- .raw_dat(x)
    .to_data_frame(x)
}

#' @export
to_tdf <- function(x){
    x <- .raw_dat(x)
    .to_data_frame(.t(x))
}

#' @export
to_mat <- function(x){
    x <- .raw_dat(x)
    .to_matrix(x)
}

#' @export
to_tmat <- function(x){
    x <- .raw_dat(x)
    .to_matrix(.t(x))
}

.get_parse_fn <- function(x){
    if(rlang::is_function(x)) return(x)

    if(rlang::is_scalar_character(x)) {
        # TODO: Consider refactoring to `match.fun`. Safer? Can't limit namespace though...
        fn <- try(get(x, mode = "function"), silent = TRUE)
        if(!inherits(fn, "try-error")) return(fn)

        fn <- try(get(paste0("to_", x), mode = "function", pos = "package:mkm"), silent = TRUE)
        if(!inherits(fn, "try-error")) return(fn)
    }

    stop("Parse functions must be a function or a scalar character that can be resolved to one.")
}


.raw_dat <- function(x){
    err <- "Can only parse a list with an mkm-like structure."

    if(!rlang::is_list(x)) stop(err)

    if(all(names(x) == c("shead", "cname", "rname", "vals"))) return(x)

    if(all(names(x) == c("dat", "format"))) return(.raw_dat(x$dat))

    stop(err)
}

.t <- function(x){
    # TODO Checking needed here
    out <- x
    out$cname <- x$rname
    out$rname <- x$cname
    out$vals <- t(x$vals)
    return(out)
}

## TODO: Consider mkm specific functionsm perc, pa, sf, etc
.to_list <- function(x){
    out <- list()

    for(a in seq_len(ncol(x$vals))){
        vals <- x$vals[,a]
        vals <- vals[!is.na(vals)] # Drop empty cells
        vals <- utils::type.convert(vals, as.is=TRUE) # Guess
        if(length(vals) > 0){
            out[[a]] <- vals
        } else {
            out[[a]] <- NA
        }
    }

    names(out) <- vctrs::vec_as_names(x$cname, repair = "minimal")

    return(out)
}

.to_data_frame <- function(x){

    out <- list()

    for(a in seq_len(ncol(x$vals))){
        vals <- x$vals[,a]
        out[[a]] <- utils::type.convert(vals, as.is=TRUE) # Guess
    }

    names(out) <- vctrs::vec_as_names(x$cname, repair = "minimal")

    if(!all(is.na(x$rname))){
        id <- list(.id = utils::type.convert(x$rname, as.is=TRUE))
        out <- c(id, out)
    }

    out <- as.data.frame(out, optional = TRUE, fix.empty.names = FALSE,
                         stringsAsFactors = FALSE)

    return(out)
}

.to_matrix <- function(x){
    out <- x$vals
    if(!all(is.na(x$cname))){
        colnames(out) <- x$cname
    }
    if(!all(is.na(x$rname))){
        rownames(out) <- x$rname
    }
    return(out)
}
