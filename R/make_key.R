make_key <- function(string, make_unique = TRUE){
    key <- as.character(string)
    key[is.na(key)] <- "NA"
    key <- stringr::str_trim(key) # Trim whitespace

    # Replace all illegal, but non digit, characters at the beginning with 'x'
    stregex <- "^[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789]+"
    key <- stringr::str_replace(key, stregex, "x")

    # Prepend 'x' if the start character is a digit
    indx <- stringr::str_detect(key, "^[0123456789]")
    if(any(indx)){
        key[indx] <- paste0('x', key[indx])
    }

    key <- make.names(key) # Replace all other non-syntactic characters
    key <- stringr::str_replace_all(key, "\\.+", "_") # Replace dots with underscores
    key <- stringr::str_replace_all(key, "_{2,}", "_") # Replace multiple underscores with a single
    key <- stringr::str_replace_all(key, "_$", "") # Remove underscores at the end of the key

    # Prepend 'x' if the key is a reserved word
    indx <- is_reserved(key)
    if(any(indx)){
        key[indx] <- paste0('x', key[indx])
    }

    # key <- stringr::str_trim(key) # Trim whitespace (again, just to be sure...)
    # key <- stringr::str_to_lower(key) # Make lowercase

    if(make_unique){
        key <- make.unique(key, sep="_")
    }
    return(key)
}

reserved <- function() {
    # https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Reserved-words
    c("if", "else", "repeat", "while", "function", "for", "in", "next", "break",
      "TRUE", "FALSE", "NULL", "Inf", "NaN",
      "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
}

is_reserved <- function(x) {
    x %in% reserved()
}


