ItemProto <- R6Class(
    "ItemProto",
    inherit = MKMProto,

    public = list(
        initialize = function(name = NULL){
            super$initialize(name = name)
            private$.content[[1]] <- Atom$new(name = "meta", content = list())
            private$.content[[2]] <- Atom$new(name = "data")
        },

        print = function(...){
            msg <- sprintf("An %s named '%s'.", class(self)[1], self$name)
            cat(msg)
        },

        setMeta = function(x){
            if(is.character(x)){
                # try to find existing item by name
            } else if(is.numeric(x)) {
                # try to find exisiting by position
            } else if(is_meta_atom(value)) {
                # overwrite exisiting meta
            } else if(is_named_list(value)) {
                # overwrite exisiting meta contents
            }
        }

    ),

    active = list(

        meta = function(value) {
            if (missing(value)) {
                return(private$.metaAtom$content)
            } else if(is_meta_atom(value)) {
                self$metaAtom <- value
            } else if(is_named_list(value)) {
                private$.metaAtom$content <- value
            } else {
                stop("`meta` must be a named list, or an <Atom> containing one.", call. = FALSE)
            }
        },

        metaAtom = function(value) {
            if (missing(value)) {
                return(private$.metaAtom)
            } else if(is_meta_atom(value)) {
                private$.metaAtom <- value
            } else {
                stop("`metaAtom` must be an <Atom> containing a named list.", call. = FALSE)
            }
        },

        data = function(value) {
            if (missing(value)) {
                return(private$.dataAtom$content)
            } else if(is_atom(value)){
                self$dataAtom <- value
            } else {
                private$.content[['data']]$content <- value
            }
        },

        dataAtom = function(value) {
            if (missing(value)) {
                return(private$.dataAtom)
            } else if(is_atom(value)){
                self$dataAtom <- value
            } else {
                stop("`dataAtom` must inherit from class <Atom>.", call. = FALSE)
            }
        }
    ),

    private = list(
        .metaItem = NULL,
        .maxData = 1,
        .content = list()
        )
)

is_atom = function(x){
    inherits(x, what = "Atom")
}

is_meta_atom = function(x){
    if(!is_atom(x)) return(FALSE)
    if(!is_named_list(x$content)) return(FALSE)
    return(TRUE)
}

is_named_list <- function(x){
    if(!is.list(x)) return(FALSE)
    if(length(names(x)) != length(x)) return(FALSE)
    return(TRUE)
}
