Section <- R6Class(
    "Section",

    public = list(
        initialize = function(search, parser, validator = NULL, name = NULL){
            self$search <- search
            self$parser <- parser

            if(is.null(validator)){
                validator <- validate::validator()
            }
            self$validator <- validator

            if(is.null(name)){
                self$name <- make_key(search)
            } else {
                self$name <- make_key(name)
            }
        },

        print = function(...){
            cat("A <Section> object... With a crappy print method defined.")
        },

        parse = function(dat){
            private$.dat <- self$parser(dat)
            invisible(self)
        },

        validate = function(validator = NULL){
            if(!missing(validator)){
                self$validator <- validator
            }
            private$.validation <- validate::confront(dat = self$dat, x = self$validator)
            invisible(self)
        },

        validationSummary = function(){
            if(is.null(self$validation)){
                self$validate()
            }
            summary(self$validation)
        },

        isValid = function(na.rm = FALSE){
            if(is.null(self$validation)){
                self$validate()
            }
            all(self$validation, na.rm = na.rm)
        }
    ),

    active = list(
        search = function(value) {
            if (missing(value)) {
                return(private$.search)
            } else {
                # Check search (section / search text)
                if(!is.character(value)){
                    stop("`search` must be a character string", call. = FALSE)
                }
                private$.search <- value
            }
        },

        name = function(value) {
            if (missing(value)) {
                return(private$.name)
            } else {
                if(!is.character(value)){
                    stop("`name` must be a character string", call. = FALSE)
                }
                private$.name <- value
            }
        },

        dat = function(value) {
            if (missing(value)) {
                return(private$.dat)
            } else {
                stop("`$dat` is read only. Use `$parse()` to parse data.", call. = FALSE)
            }
        },

        parser = function(value) {
            if (missing(value)) {
                return(private$.parser)
            } else {
                if(!inherits(value, c("character", "function"))){
                    msg <- sprintf("`$parser` must be a character or a function, not %s.", mode(value))
                    stop(msg, call. = FALSE)
                }
                private$.parser <- match.fun(value)
                private$.parser_name <- get_original_name(private$.parser)
                private$.parser_location <- find(private$.parser_name, mode = "function")[1]
            }
        },

        validator = function(value) {
            if (missing(value)) {
                return(private$.validator)
            } else {
                if(!inherits(value, "validator")) {
                    msg <- "`validator` must be a validator object (see `?validate::validator`)"
                    stop(msg, call. = FALSE)
                }
                private$.validator <- value
            }
        },

        validation = function(value) {
            if (missing(value)) {
                return(private$.validation)
            } else {
                stop("`$validation` is read only. Use `$validate()` to confront data.", call. = FALSE)
            }
        }
    ),

    private = list(
        .search = NULL,
        .name = NULL,
        .dat = NULL,
        .parser = NULL,
        .parser_name = NULL,
        .parser_location = NULL,
        .validator = NULL,
        .validation = NULL
    )
)



get_original_name <- function(fun) {
    nmes <- ls(envir = environment(fun))
    for (nme in nmes) {
        if (identical(fun, get(nme, envir = environment(fun)))) {
            return(nme)
        }
    }
}

