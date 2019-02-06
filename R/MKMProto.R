Proto <- R6Class(
    "Proto",
    public = list(
        initialize = function(name = NULL, path = NULL, sheet = NULL, section_text = NULL){
            # Must use single bracket list notation here to instantiate active fields...
            # Otherwise they don't get set if input is NULL
            private$.props[c("name", "path", "sheet", "section_text")] <- list(NULL)
            self$name <- name
            self$path <- path
            self$sheet <- sheet
            self$section_text <- section_text
        },

        read = function(path = NULL, sheet = NULL){
            if(!missing(path)){
                self$path <- path
            }
            if(!missing(sheet)){
                self$sheet <- sheet
            }
            if(is.null(self$path)){
                stop("When reading from file, `path` cannot be NULL")
            }
            if(!file.exists(self$path)){
                stop(sprintf("File not found: %s", self$path))
            }
            self$raw <- read_mkm(path = self$path, sheet = self$sheet)
            invisible(self)
        },

        get_prop = function(key) {
            key <- private$.parse_key(key)
            return(private$.props[[key]])
        },

        set_prop = function(key, value) {
            key <- private$.parse_key(key)
            if(key %in% private$.active_props()){
                self[[key]] <- value
            } else {
                key <- make_key(key) ## TODO: Relax this assumption? Rely on back ticks?
                private$.props[[key]] <- value
            }
            invisible(self)
        },

        remove_prop = function(key) {
            self$set_prop(key, NULL)
            invisible(self)
        },

        match_section_text = function(haystack = NULL){
            if(!gate(haystack, character())){
                msg <- "`haystack` must be a character vector"
                stop(msg, call. = FALSE)
            }
            if(is.null(self$section_text)){
                msg <- "`section_text` is undefined (NULL) so it is impossible to match"
                out = NA_integer_
                comment(x = out) <- msg
                return(out)
            }
            mtch <- haystack %in% self$section_text
            if(sum(mtch) == 0){
                msg <- sprintf("`section_text` not found ('%s' matched 0 times)", self$section_text)
                out <- NA_integer_
                comment(x = out) <- msg
                return(out)
            }
            if(sum(mtch) > 1){
                msg <- sprintf("`section_text` matched more than once ('%s' matched %1.0f times)",
                               self$section_text, sum(mtch))
                out <- NA_integer_
                comment(x = out) <- msg
                return(out)
            }
            return(which(mtch))
        }
    ),

    active = list(
        properties = function(value) {
            if(missing(value)) {
                return(private$.props)
            } else {
                msg <- paste0("`$properties` is read only.\n",
                              "Use `$set_prop(...)` or active bindings to set properties.")
                stop(msg, call. = FALSE)
            }
        },

        name = function(value) {
            if (missing(value)) {
                return(private$.props$name)
            }
            if(!gate(value, character(1L))) {
                msg <- "`name` must be a character vector of length 1"
                stop(msg, call. = FALSE)
            }
            value <- make_key(value) # NB: Coerced by make_key
            ## TODO: Consider just back ticking non-syntactic names when needed (printing?)...
            ##       This is what tibble:: does...

            # This notation is not really needed, really, becuase 'value' will never be NULL,
            # but whatever...
            private$.props['name'] <- list(value)
        },

        path = function(value) {
            if (missing(value)) {
                return(private$.props$path)
            }
            if(!gate(value, character(1L), NULL)){
                msg <- "`path` must be a character vector of length 1 or NULL"
                stop(msg, call. = FALSE)
            }
            # Again, the ['path'] <- list(value) notation is important if value == NULL
            private$.props['path'] <- list(value)
        },

        sheet = function(value) {
            if (missing(value)) {
                return(private$.props$sheet)
            }
            if(!gate(value, character(1L), numeric(1L), integer(1L), NULL)) {
                msg <- "`sheet` must be a character or integer vector of length 1 or NULL"
                stop(msg, call. = FALSE)
            }
            if(is.numeric(value)){
                value <- as.integer(value) # NB: Coerced to integer, if numeric
            }
            private$.props['sheet'] <- list(value)
        },

        section_text = function(value) {
            if (missing(value)) {
                return(private$.props$section_text)
            }
            if(!gate(value, character(1L), NULL)){
                msg <- "`section_text` must be a character vector of length 1 or NULL"
                stop(msg, call. = FALSE)
            }
            private$.props['section_text'] <- list(value) # Allowing for NULL
        },

        raw = function(value) {
            if(missing(value)) {
                return(private$.raw)
            }
            # NB: At the moment we assume that a `raw`:
            #    - Is a list with named items 'text' and 'format' (although the later may be NULL)
            #    - All of the components are matrixes
            #    - The 'text' item is a character matrix, specifically
            #    - The dimensions of all objects are the same
            if(is.null(value)){
                return(private$.raw <- value)
            }
            # proto_obj <- list("text" = matrix("A")[0,0], "format" = list())
            if(!is.list(value)) {
                msg <- "`raw` must be a list or NULL"
                stop(msg, call. = FALSE)
            }
            if(!all(c("text", "format") %in% names(value))) {
                msg <- "`raw` must contain items named 'text' and 'format'"
                stop(msg, call. = FALSE)
            }
            if(!gate(value$text, matrix("A")[0,0])) {
                msg <- "`raw$text` must be a character matrix"
                stop(msg, call. = FALSE)
            }
            if(!gate(value$format, list(), NULL)) {
                msg <- "`raw$format` must be a list or NULL"
                stop(msg, call. = FALSE)
            }
            if(gate(value$format, list())) {
                is_all_mat <- rapply(value$format, is.matrix, how="unlist")
                if(is.null(is_all_mat) || !all(is_all_mat)) {
                    msg <- "`raw$format` must be a (nested) list of matrices"
                    stop(msg, call. = FALSE)
                }
                trgt_dim <- dim(value$text)
                all_dims_equal <- all(rapply(value$format, function(x, cmpre = trgt_dim) { identical(dim(x), cmpre) }))
                if(!all_dims_equal) {
                    msg <- "All matrices in `raw$format` must have the same dimensions as `raw$text`"
                    stop(msg, call. = FALSE)
                }
            }
            private$.raw <- value
        }
    ),

    private = list(
        .active_names = function() {
            indx <- vapply(names(self), bindingIsActive, FUN.VALUE = logical(1), env = self)
            names(indx)[indx]
        },

        .active_props = function() {
            indx <- names(private$.props) %in% private$.active_names()
            names(private$.props)[indx]
        },

        .parse_key = function(key){
            if(!gate(key, character(1L), numeric(1L), integer(1L))) {
                msg <- "`key` must be a character or integer vector of length 1"
                stop(msg, call. = FALSE)
            }
            if(is.numeric(key)) {
                key <- as.integer(key)
                key <- names(private$.props)[key]
                if(is.na(key)) {
                    msg <- "Numeric value for `key` must index an existing property"
                    stop(msg, call. = FALSE)
                }
            }
            return(key)
        },

        .props = list(),

        .raw = NULL
    )
)

# is_named_list <- function(x){
#     if(!is.list(x)) return(FALSE)
#     if(length(names(x)) != length(x)) return(FALSE)
#     return(TRUE)
# }

is_syntactic <- function(x){
    ret <- make.names(x) == x
    ret[is.na(x)] <- FALSE
    ret
}

tick_non_syntactic <- function(x) {
    needs_ticks <- !is_syntactic(x)
    x[needs_ticks] <- tick(x[needs_ticks])
    x
}

tick <- function(x) {
    x[is.na(x)] <- "NA"
    encodeString(x, quote = "`")
}
