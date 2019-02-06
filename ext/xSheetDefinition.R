SheetDefinition <- R6Class(
    "SheetDefinition",

    public = list(
        sections = NULL,

        initialize = function(){
            self$sections <- list()
        },

        print = function(...){
            if(self$length() == 0){
                cat("<SheetDefinition> object describing 0 sections.", sep="")
            } else if(self$length() == 1){
                cat("<SheetDefinition> object describing 1 section:\n", sep="")
            } else {
                cat(sprintf("<SheetDefinition> object describing %d sections:\n", self$length()), sep="")
            }

            for (a in seq_along(self$sections)) {
                s <- self$sections[[a]]
                padw <- max(stringr::str_length(self$names))
                cat(sprintf("  %s: '%s' \n",
                            stringr::str_pad(s$name, padw),
                            s$search))
            }
        },

        length = function(){
            base::length(self$sections)
        },

        add = function(search, parser = NULL, validator = NULL, name = NULL){

            sec <- Section$new(search = search, parser = parser, validator = validator, name = name)

            if(sec$name %in% self$names){
                stop("`name` must be unique")
            }

            self$sections[[self$length()+1]] <- sec
            invisible(self)
        },

        get = function(name){
            ndx <- match(name, self$names)
            if(is.na(ndx)){
                stop(sprintf("Section with name '%s' not found.", name))
            }
            self$sections[[ndx]]
        },

        remove = function(name){
            ndx <- match(name, self$names)
            if(is.na(ndx)){
                stop(sprintf("Section with name '%s' not found.", name))
            }
            self$sections[[ndx]] <- NULL
            invisible(self)
        }
    ),

    active = list(
        search = function(value) {
            if (missing(value)) {
                vapply(self$sections, "[[", character(1L), "search")
            } else {
                stop("`$search` is read only", call. = FALSE)
            }
        },

        names = function(value) {
            if (missing(value)) {
                vapply(self$sections, "[[", character(1L), "name")
            } else {
                stop("`$names` is read only", call. = FALSE)
            }
        }
    )
)
