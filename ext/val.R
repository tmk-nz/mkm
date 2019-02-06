
vv <- function(..., .prefix="R"){
    L <- as.list(substitute(list(...))[-1])
    nm <- extract_names(L, prefix = .prefix)
    cr <- Sys.time()
    R <- vector(length(L), mode="list")
    for ( i in seq_along(L) ){
        R[[i]] <- list(
            expr = L[[i]]
            , name = nm[i]
            , origin="command-line"
            , created = cr
        )
    }
    R
}

npos <- function(n) max(1,ceiling(log10(n+1)))

# get names from a list, replacing empty names values with numbers
extract_names <- function(L,prefix="V"){
    npos <- npos(length(L))
    fmt <- paste0("%s%0",npos,"d")
    generic <- sprintf(fmt,prefix,seq_along(L))
    given <- names(L)
    if (is.null(given)) return(generic)
    igen <- given %in% c("", NA)
    given[igen] <- generic[igen]
    make.names(given, unique=T)
}
