Collection <- R6Class(
    "Collection",
    inherit = Proto,

    public = list(
        initialize = function(name = NULL, content = NULL,
                              path = NULL, sheet = NULL, section_text = NULL){
            super$initialize(name = name, path = path, sheet = sheet, section_text = section_text)
            # Instantiate active fields...
            private$.props[c("content")] <- list(NULL)
            self$content <- content
        },

        print = function(...){
            if(self$length == 0){
                msg <- sprintf("A <Collection> named '%s', with no content.",
                               self$name)
                cat(msg)
            } else {
                msg <- sprintf("A <Collection> named '%s', containing:\n",
                               self$name)
                cat(msg)
                for(a in seq_len(self$length)){
                    self$content[[a]]$print(...)
                    cat("\n")
                }
            }
        },

        parse = function(raw = NULL){
            if(missing(raw)){
                # If raw is missing, then we should get our own...
                if(is.null(self$raw)){
                    msg <- "File not read yet. Attempting automatically with `$read()`"
                    warning(msg)
                    self$read()
                }
            } else {
                # If raw is provided then we assume it is OK to be chunked.
                self$raw <- raw
            }
            # Use section texts to find the sections (none can be missing)
            st <- list()
            for (a in seq_len(self$length)) {
                st[[self$content[[a]]$name]] <- self$content[[a]]$match_section_text(self$raw$text[,1])
            }
            if(any(indx <- vapply(st, is.na, FUN.VALUE = logical(1)))){
                nas <- st[names(indx)[indx]]
                m1 <- sprintf("`section_text` for %1.0f item%s not found:",
                              length(nas),
                              ifelse(length(nas) == 1, '', 's'))
                mn <- paste0(sprintf("\n  %s: %s", names(nas), lapply(nas, comment)), collapse = '')
                msg <- paste0(m1, mn)
                stop(msg)
            }
            # Chunk the raw content and pass to the objects for parsing...
            runlist <- data.frame(names = names(st), st = unlist(st),
                                  stringsAsFactors = F, row.names = NULL)
            runlist <- runlist[order(runlist$st),]
            runlist$en <- c(runlist$st[seq_along(runlist$names)[-1]]-1, nrow(self$raw$text))
            for (a in seq_along(runlist$names)) {
                ndx <- seq(from = runlist$st[a], to = runlist$en[a])
                rawdat <- rapply(object = self$raw, f = mat_select_rows,
                                   how = "list", ndx = ndx)
                self$content[[runlist$names[a]]]$parse(rawdat)
                # TODO: Some nice checking / handling here:
                # Handle warnings about returning NULL parser results
                # Handle errors signalled from other collections (e.g. missing `section_text`)
            }
            invisible(self)
        },

        validate = function(){
            for(a in seq_len(self$length)){
                self$content[[a]]$validate()
            }
        },

        is_valid = function(allow_na = FALSE,
                            allow_warnings = FALSE,
                            allow_errors = FALSE,
                            quiet = FALSE){
            self$validate()
            val <- logical(length = self$length)
            cmmt <- NULL
            for(a in seq_len(self$length)){
                tmp <- self$content[[a]]$is_valid(allow_na = allow_na,
                                                  allow_warnings = allow_warnings,
                                                  allow_errors = allow_errors,
                                                  quiet = TRUE)
                val[a] <- tmp
                cmmt <- c(cmmt, comment(tmp))
            }
            res <- all(val)
            if(!quiet && length(cmmt)){
                msg <- sprintf("In Collection `%s`:\n", self$name)
                cmmt <- paste0("  ", cmmt)
                cmmt <- stringr::str_replace_all(cmmt, "\n", "\n  ")
                cmmt <- paste0(cmmt, collapse = "\n")
                msg <- paste0(msg, cmmt)
                message(msg)
                comment(res) <- msg
            }
            return(res)

        },

        to_list = function(allow_invalid = FALSE, ...) {
            if(!allow_invalid && !self$is_valid(...)){
                msg <- "Content did not pass validation.\n  Override with `allow_invalid = TRUE`"
                stop(msg)
            }
            out <- list()
            for (a in seq_along(self$names)){
                nm <- self$names[a]
                out[nm] <- self$content[[nm]]$to_list(allow_invalid = TRUE)
            }
            out <- list(out)
            names(out) <- self$name
            return(out)
        },

        define_meta = function(name){
            if(!gate(name, character(1L))){
                msg <- sprintf("`name` must be a character vector of length 1")
                stop(msg, call. = FALSE)
            }
            if(!name %in% self$names){
                msg <- sprintf("`name` must reference an item in the collection.\n  i.e. one of: %s",
                               paste(self$names, collapse = ', '))
                stop(msg, call. = FALSE)
            }
            if(!is_named_list(self$content[[name]]$content)){
                msg <- sprintf("`name` must reference an item containing a named list")
                stop(msg, call. = FALSE)
            }
            private$.meta <- self$content[[name]]
        },

        undefine_meta = function(){
            private$.meta <- NULL
        },

        template = function() {
            out <- self$clone(deep = TRUE)
            for(a in seq_along(out$content)){
                out$content[[a]] <- out$content[[a]]$template()
            }
            out$path <- NULL
            out$sheet <- NULL
            out$raw <- NULL
            out$undefine_meta()
            return(out)
        }

    ),

    active = list(
        content = function(value) {
            if(missing(value)) {
                names(private$.props$content) <- private$.extract_names(private$.props$content)
                return(private$.props$content)
            } else {
                if(is.null(value)){
                    return(private$.props['content'] <- list(value))
                }
                # If not NULL, it must be a list
                if(!gate(value, list())){
                    msg <- "`$content` must be a list."
                    stop(msg, call. = FALSE)
                }
                # Must contain only Items or Collections
                content_inherits <- vapply(value, inherits, FUN.VALUE = logical(1L),
                                           c("Item", "Collection"))
                if(!all(content_inherits)){
                    msg <- "`$content` must be a list containing only objects of class 'Item' or 'Collection'."
                    stop(msg, call. = FALSE)
                }
                # Must have unique names
                content_names <- private$.extract_names(value)
                if(length(content_names) != length(unique(content_names))){
                    # Revert names
                    for(a in seq_along(private$.props[['content']])){
                        private$.props[['content']][[a]]$name <- names(private$.props[['content']])[a]
                    }
                    msg <- "All objects in `$content` must have unique names."
                    stop(msg, call. = FALSE)
                }
                # Optional syntax, becuase this will never be NULL
                names(value) <- content_names
                private$.props['content'] <- list(value)
            }
        },

        meta = function(value){
            if(missing(value)) {
                return(private$.meta$content)
            } else {
                msg <- paste0("`$meta` is (currently) read only.")
                stop(msg, call. = FALSE)
            }
        },

        names = function(value) {
            if(missing(value)){
                return(private$.extract_names(self$content))
            } else {
                if(!gate(value, character(self$length))){
                    msg <- sprintf("`$names` must be a character vector of length %d.", self$length)
                    stop(msg, call. = FALSE)
                }
                if(length(value) != length(unique(value))){
                    msg <- "`$names` must be unique."
                    stop(msg, call. = FALSE)
                }
                for(a in seq_along(self$content)){
                    self$content[[a]]$set_prop('name', value[[a]])
                }
            }
        },

        length = function(value) {
            if(missing(value)){
                return(length(self$content))
            } else {
                msg <- "`$length` is read only."
                stop(msg, call. = FALSE)
            }
        }
    ),

    private = list(
        .extract_names = function(input){
            if(length(input)){
                return(as.vector(vapply(input, '[[', FUN.VALUE = character(1L), 'name')))
            } else {
                return(NULL)
            }

        },

        .meta = NULL,

        deep_clone = function(name, value) {
            # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
            # each field, with the name and value.
            if (name == ".props") {
                # `.props` might contain a content item...
                # If this has a positive length, then it contains items that should be deep cloned too...
                # (see note in Item that this is possible is a bit of a fuckup)
                # For now lets pass the tests...
                if(length(value$content)){
                    for(a in seq_along(value$content)){
                        value$content[[a]] <- value$content[[a]]$clone(deep = TRUE)
                    }
                }
                return(value)
            } else {
                # For all other fields, just return the value
                value
            }
        }
    )
)
