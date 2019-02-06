Item <- R6Class(
    "Item",
    inherit = MKMProto,

    public = list(
        initialize = function(name = NULL){
            super$initialize(name = name)
            private$.metaObject <- Atom$new(name = "meta", content = list())
            private$.dataObject <- Atom$new(name = "data")
        },

        print = function(...){
            msg <- sprintf("An <Item> named '%s'.", self$name)
            cat(msg)
        }
    ),

    active = list(

        data = function(value) {
            if (missing(value)) {
                return(private$.dataObject$content)
            } else if(is_atom(value)){
                self$dataObject <- value
            } else {
                private$.dataObject$content <- value
            }
        },

        dataObject = function(value) {
            if (missing(value)) {
                return(private$.dataObject)
            } else if(is_atom(value)){
                self$dataObject <- value
            } else {
                stop("`dataObject` must inherit from class <Atom>.", call. = FALSE)
            }
        },

        meta = function(value) {
            if (missing(value)) {
                return(private$.metaObject$content)
            } else if(is_meta_atom(value)) {
                self$metaObject <- value
            } else if(is_named_list(value)) {
                private$.metaObject$content <- value
            } else {
                stop("`meta` must be a named list, or an <Atom> containing one.", call. = FALSE)
            }
        },

        metaObject = function(value) {
            if (missing(value)) {
                return(private$.metaObject)
            } else if(is_meta_atom(value)) {
                private$.metaObject <- value
            } else {
                stop("`metaObject` must be an <Atom> containing a named list.", call. = FALSE)
            }
        }
    ),

    private = list(
        .metaObject = NULL,
        .dataObject = NULL
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
