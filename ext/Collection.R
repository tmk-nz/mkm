# The MKM Collection

Collection <- R6Class(
    "Collection",

    public = list(

        initialize = function(){
            private$.dat <- list()
        },

        add = function(obj){
            if(!inherits(x = obj, what = c("Object", "Collection"))){
                msg <- "Must be an <Object> or a <Collection>."
                stop(msg, call. = FALSE)
            }

            if(self$has(obj$name)){
                msg <- "`name` must be unique"
                stop(msg, call. = FALSE)
            }

            private$.dat[[self$length()+1]] <- obj
            invisible(self)
        },

        get = function(name){
            ndx <- self$matchNames(name, stop_on_missing = TRUE)
            private$.dat[[ndx]]
        },

        remove = function(name){
            ndx <- self$matchNames(name, stop_on_missing = TRUE)
            private$.dat[[ndx]] <- NULL
            invisible(self)
        },

        matchNames = function(x, stop_on_missing = FALSE) {
            if(!is.character(x)){
                msg <- sprintf("`$matchNames` requires a character, not %s.", mode(value))
                stop(msg, call. = FALSE)
            }
            ndx <- base::match(x, self$names)
            if(any(is.na(ndx)) && stop_on_missing){
                miss <- x[is.na(ndx)]
                if(length(miss) > 1){
                    msg <- sprintf("The following names were not found: '%s'",
                                   paste(miss, collapse = "', "))
                    stop(msg, call. = FALSE)
                } else {
                    msg <- sprintf("'%s' not found", miss)
                    stop(msg, call. = FALSE)
                }
            }
            return(ndx)
        },

        hasNames = function(x, stop_on_missing = FALSE){
            ndx <- self$matchNames(x, stop_on_missing)
            !is.na(ndx)
        },

        validate = function(){
            for(a in seq_along(private$.dat)){
                private$.dat[[a]]$validate()
            }
            invisible(self)
        },

        isValid = function(na.rm = FALSE){
            all(vapply(private$.dat, isValid, FUN.VALUE = logical(1), na.rm = na.rm))
        },

        validationSummary = function(){
            lapply(private$.dat, validationSummary)
        },

        print = function(...){
            cat("A <Collection> object... With a crappy print method defined.")
        },

        length = function(){
            base::length(self$dat)
        }
    ),

    active = list(
        names = function(value) {
            if (missing(value)) {
                vapply(private$.dat, "[[", character(1L), "name")
            } else {
                stop("`$names` is read only", call. = FALSE)
            }
        }
    ),

    private = list(
        .dat = NULL
    )
)
