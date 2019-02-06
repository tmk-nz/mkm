# The MKM Data Entry Sheet

Sheet <- R6Class(
    "Sheet",

    public = list(

        initialize = function(file = NULL, sheet = NULL, template = NULL){
            if(!missing(sheet)){
                self$sheet <- sheet
            }
            if(!missing(file)){
                self$file <- file
                self$read()
            }
            if(!missing(template)){
                self$define(template)
            }
        },

        read = function(file = NULL, sheet = NULL){
            if(!missing(file)){
                self$file <- file
            }
            if(!missing(sheet)){
                self$sheet <- sheet
            }
            if(is.null(self$file)){
                stop("`file` cannot be NULL")
            }
            if(is.null(self$sheet)){
                stop("`sheet` cannot be NULL")
            }
            if(!file.exists(self$file)){
                stop(sprintf("file (%s) does not exist", self$file))
            }
            dat <- read_mkm(file = self$file, sheet = self$sheet)
            private$.text <- dat$text
            private$.format <- dat$format
            invisible(self)
        },

        define = function(template){
            if(!inherits(template, "Sheet")){
                msg <- "`template` must be a <Sheet> object (use `Sheet$new()`)"
                stop(msg)
            }

            if(is.null(self$text)){
                msg <- "File not read yet. Attempting automatically with `$read()`"
                warning(msg)
                self$read()
            }

            # Find sections...
            # find_sections() will error out if any sections as missing or matched multiple times.
            private$.definition <- find_sections(self$text, template)
            invisible(self)
        },

        length = function(){
            base::length(self$sections)
        },

        add = function(search, parser, validator = NULL, name = NULL){
            sec <- Section$new(search = search, parser = parser, validator = validator, name = name)

            if(sec$name %in% self$names){
                stop("`name` must be unique")
            }

            private$.sections[[self$length()+1]] <- sec
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
        },

        parse = function(){
            if(is.null(self$definition)){
                msg <- paste("You must define the structure of the file first (use `$define(...)`)")
                stop(msg)
            }

            # Extract/parse sections
            nmes <- self$names
            # TODO: Nicer error catching... Catch all errors and report
            for(a in seq_along(nmes)){
                ndx <- seq(from = self$sections[a, "start"], to = self$sections[a, "end"])
                dat <- list()
                dat[['text']] <- mat_select_rows(mat = self$text, ndx = ndx)
                dat[['format']] <- rapply(object = self$format, f = mat_select_rows, how = "list", ndx = ndx)
                self$get(nmes[a])$parse(dat)
            }
        },

        validate = function(){
            if(is.null(self$parsed)){
                msg <- paste("You must parse the file first (use `$parse()`)")
                stop(msg)
            }
            # Validate sections
            nmes <- self$name
            for(a in seq_along(nmes)){
                self$get(nmes[a])$validate()
            }
        },

        isValid = function(na.rm = FALSE){
            all(vapply(self$sections, isValid, FUN.VALUE = logical(1), na.rm = na.rm))
        },

        validationSummary = function(){
            lapply(self$sections, validationSummary)
        },

        to_list = function(){

        },

        print = function(...){

        }
    ),

    active = list(
        file = function(value){
            if (missing(value)) {
                return(private$.file)
            }
            if(!is.character(value)){
                stop("`file` must be a character")
            }
            private$.file <- value
        },

        sheet = function(value){
            if (missing(value)) {
                return(private$.sheet)
            }
            if(!is.numeric(value)){
                stop("`sheet` must be a numeric")
            }
            private$.sheet <- value
        },

        dat = function(value){
            if (missing(value)) {
                private$.dat
            } else {
                if(!is.list(value)){
                    msg <- sprintf("`$dat` must be a list, not %s.", mode(value))
                    stop(msg, call. = FALSE)
                }
                if(is.null(names(value))){
                    msg <- "`$dat` must have names"
                    stop(msg, call. = FALSE)
                }
                if()

            }
        },

        definition = function(value){
            if (missing(value)) {
                private$.definition
            } else {
                stop("`$definition` is read only", call. = FALSE)
            }
        },

        sections = function(value){
            if (missing(value)) {
                private$.sections
            } else {
                stop("`$sections` is read only", call. = FALSE)
            }
        },

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
    ),

    private = list(
        .file = NULL,
        .sheet = 1,
        .definition = NULL,
        .sections = list(),
        .text = NULL,
        .format = NULL
    )
)
