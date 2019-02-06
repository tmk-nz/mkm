gate = function(value, ...){
    val <- fingerprint(value)

    pos <- list(...)
    # vec <- vapply(pos, is.vector, FUN.VALUE = logical(1L))
    # null <- vapply(pos, is.null, FUN.VALUE = logical(1L))
    # if(!all(vec | null)) stop("All prototypes must be a vector (see `?is.vector`) or NULL")

    pos <- lapply(pos, fingerprint)

    # We don't match class exactly, but rather look for inheritance...
    # So here replace class with inheritance.
    # inh <- mapply(inherits, what = lapply(pos, '[[', 'class'), MoreArgs = list(x = value))
    pos <- lapply(pos, function(x, v) { x$inherits <- inherits(v, x$class); x$class <- NULL; return(x) }, value)
    val$class <- NULL
    val$inherits <- TRUE # Obviously the value inherets from itself!

    # Set length zero items to the length of value (so they match)
    indx <- vapply(pos, function(x) x$length == 0, FUN.VALUE = logical(1L))
    pos[indx] <- lapply(pos[indx], function(x, v) { x['length'] <- list(v); return(x) }, val$length)

    # Set items with all zero dims to the dim of the value (so they match)
    indx <- vapply(pos, function(x) all(x$dim == 0), FUN.VALUE = logical(1L))
    pos[indx] <- lapply(pos[indx], function(x, v) { x['dim'] <- list(v); return(x) }, val$dim)

    any(vapply(pos, identical, logical(1L), val))
}

fingerprint = function(value){
    list(type = typeof(value), class = class(value), length = length(value), dim = dim(value))
}
