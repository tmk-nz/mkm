Item <- R6Class(
    "Item",
    inherit = Proto,

    public = list(
        initialize = function(name = NULL, content = NULL,
                              validator = validate::validator(),
                              indicator = validate::indicator(),
                              path = NULL, sheet = NULL, section_text = NULL,
                              parser = function(x) return(NULL)){
            super$initialize(name = name, path = path, sheet = sheet, section_text = section_text)
            # Instantiate active fields...
            private$.props[c("content", "validator", "indicator", "parser")] <- list(NULL)
            self$content <- content
            self$validator <- validator
            self$indicator <- indicator
            self$parser <- parser
        },

        print = function(...){
            if(is.null(self$content)){
                msg <- sprintf("An <Item> named '%s', with no content.",
                               self$name)
                cat(msg)
            } else {
                msg <- sprintf("An <Item> named '%s', containing an item of class `%s`:\n",
                               self$name,
                               class(self$content)[1])
                cat(msg)
                str(self$content)
            }
        },

        validate = function(validator = NULL){
            if(!missing(validator)){
                self$validator <- validator
            }
            private$.validation <- validate::confront(dat = self$content, x = self$validator)
            invisible(self)
        },

        indicate = function(indicator = NULL){
            if(!missing(indicator)){
                self$indicator <- indicator
            }
            private$.indication <- validate::confront(dat = self$content, x = self$indicator)
            invisible(self)
        },

        parse = function(raw = NULL){
            # If raw is missing, then we should get our own...
            if(missing(raw)){
                if(is.null(self$raw)){
                    msg <- "File not read yet. Attempting automatically with `$read()`"
                    warning(msg, immediate. = TRUE)
                    self$read()
                }
                # Use section text (if not null) to find the section (there can only be one)
                if(is.null(self$section_text)){
                    strt <- 1L
                } else {
                    strt <- self$match_section_text(self$raw$text[, 1])
                    if(is.na(strt)) stop(comment(strt))
                }
                # Filter content with the result of match_section_text()
                ndx <- seq(from = strt, to = nrow(self$raw$text))
                self$raw <- rapply(object = self$raw, f = mat_select_rows,
                                   how = "list", ndx = ndx)
            } else {
                # If raw is provided then we assume it is already in a format suitable for the parser.
                # In particular, we assume that it has already been 'chunked', if coming form a collection.
                self$raw <- raw
            }
            self$content <- self$parser(self$raw)
            if(is.null(self$content)){
                msg <- sprintf("Parser for `%s` returned NULL", self$name)
                warning(msg, immediate. = TRUE, call. = FALSE)
            }
            invisible(self)
        },

        get_validation = function(summarise = FALSE){
            self$validate()
            if(summarise){
                return(summary(private$.validation))
            } else {
                return(private$.validation)
            }
        },

        get_indication = function(summarise = FALSE){
            self$indicate()
            if(summarise){
                return(summary(private$.indication))
            } else {
                return(private$.indication)
            }
        },

        is_valid = function(allow_na = FALSE,
                            allow_warnings = FALSE,
                            allow_errors = FALSE,
                            quiet = FALSE){
            v <- self$get_validation(summarise = FALSE)
            # If allow_na is TRUE, this will return TRUE if there are NAs and NA otherwise:
            res <- all(v, na.rm = allow_na)
            wrn <- validate::warnings(v)
            err <- validate::errors(v)
            na_msg <- wrn_msg <- err_msg <- NULL
            if(is.na(res)){
                na_msg <- sprintf("The validation for `%s` contains one or more missing (NA) values",
                                  self$name)
                res <- FALSE
            }
            if(!allow_warnings && length(wrn)){
                wrn_msg <- sprintf("The validation for `%s` has %1.0f warning%s:",
                                   self$name,
                                   length(wrn),
                                   ifelse(length(wrn) == 1, "", "s"))
                wf <- paste0(paste0("\n  ", names(wrn), ": ", wrn), collapse = '')
                wrn_msg <- paste0(wrn_msg, wf)
                res <- FALSE
            }
            if(!allow_errors && length(err)){
                err_msg <- sprintf("The validation for `%s` has %1.0f error%s:",
                                   self$name,
                                   length(err),
                                   ifelse(length(err) == 1, "", "s"))
                ef <- paste0(paste0("\n  ", names(err), ": ", err), collapse = '')
                err_msg <- paste0(err_msg, ef)
                res <- FALSE
            }
            comment(res) <- c("na_msg" = na_msg, "wrn_msg" = wrn_msg, "err_msg" = err_msg)
            if(!quiet){
                for(a in seq_along(cmmt <- comment(res))){
                    message(cmmt[a])
                }
            }
            return(res)
        },

        to_list = function(allow_invalid = FALSE, ...) {
            if(!allow_invalid && !self$is_valid(...)){
                msg <- "Content did not pass validation. Override with `allow_invalid = TRUE`"
                stop(msg)
            }
            out <- list(self$content)
            names(out) <- self$name
            return(out)
        },

        template = function() {
            out <- self$clone(deep = TRUE)
            out$path <- NULL
            out$sheet <- NULL
            out$content <- NULL
            out$raw <- NULL
            # Ooof! This feels rough... Shouldn't be *strictly* needed...
            out$.__enclos_env__$private$.validation <- NULL
            out$.__enclos_env__$private$.indication <- NULL
            return(out)
        }
    ),

    active = list(
        content = function(value) {
            if (missing(value)) {
                return(private$.props$content)
            }
            # Again, the ['content'] <- list(value) notation is important if value == NULL
            private$.props['content'] <- list(value)
        },

        validator = function(value) {
            if (missing(value)) {
                return(private$.props$validator)
            }
            if(!gate(value, validate::validator())){
                msg <- "`validator` must be a validator object (see `?validate::validator`)"
                stop(msg, call. = FALSE)
            }
            private$.props$validator <- value
        },

        indicator = function(value) {
            if (missing(value)) {
                return(private$.props$indicator)
            }
            if(!gate(value, validate::indicator())){
                msg <- "`indicator` must be a indicator object (see `?validate::indicator`)"
                stop(msg, call. = FALSE)
            }
            private$.props$indicator <- value
        },

        parser = function(value) {
            if (missing(value)) {
                return(private$.props$parser)
            }
            if(!gate(value, character(1L), function(){})){
                msg <- "`parser` must be a character vector of length 1 or a function"
                stop(msg, call. = FALSE)
            }
            private$.props$parser <- match.fun(value)
        }
    ),

    private = list(
        .validation = NULL,
        .indication = NULL,
        deep_clone = function(name, value) {
            # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
            # each field, with the name and value.
            if (name == ".props") {
                # `.props` might contain validate:: items with reference semantics
                # That this is possible is a bit of a fuckup.
                # Might change this, but for now lets pass the tests...
                if("validator" %in% names(value)){
                    value$validator <- value$validator$copy()
                }
                if("indicator" %in% names(value)){
                    value$indicator <- value$indicator$copy()
                }
                return(value)
            } else if (name %in% c(".validation", ".indication")) {
                if(is.null(value)){
                    value
                } else {
                    value$copy()
                }
            } else {
                # For all other fields, just return the value
                value
            }
        }
    )
)
