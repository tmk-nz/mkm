# MKM Parsing + Coercion functions
# parse_* functions take raw input (which is a list with 'text' and 'format' components) and produce a base R type (list, dataframe or matrix)

parse_rlist <- function(x){
    x <- raw_text(x)
    x <- x[-1, , drop = FALSE] # Row 1 is always removed for these objects
    to_list(x)
}

parse_clist <- function(x){
    x <- raw_text(x)
    x <- x[ , -1, drop = FALSE] # Column 1 is always removed for these objects
    to_list(t(x))
}

parse_df <- function(x){
    x <- raw_text(x)
    to_data_frame(x)
}

parse_tdf <- function(x){
    x <- raw_text(x)
    to_data_frame(t(x))
}

parse_mat <- function(x){
    x <- raw_text(x)
    to_matrix(x)
}

parse_tmat <- function(x){
    x <- raw_text(x)
    to_matrix(t(x))
}

## Consider mkm specific functionsm perc, pa, sf, etc

# Support functions... Not Exported
raw_text <- function(x, remove_section_text = TRUE, remove_empty_cols = TRUE, remove_empty_rows = TRUE){
    # The 'master text parser'.
    # Is beligerant about input types. Coerces dataframes and lists if possible.
    # By default, it strips totally NA columns and rows
    # By default it sets the 'section text' to NA
    # Returns a character matrix with no dimnames.
    if(is.data.frame(x)){
        x <- x %>% dplyr::mutate_all(as.character) %>% as.matrix()
    }

    if(is.list(x)) {
        has_text <- "text" %in% names(x)
        if(!has_text){
            stop("If input is a list, it must have an item named 'text'")
        }
        x <- raw_text(x$text,
                      remove_section_text = remove_section_text,
                      remove_empty_cols = remove_empty_cols,
                      remove_empty_rows = remove_empty_rows)
        return(x)
    }

    if(!is.matrix(x) || !is.character(x) || length(dim(x)) != 2 ) {
        stop("Input must be a character matrix with two dimensions")
    }

    # Detect and remove totally empty (NA only) columns
    if(remove_empty_cols){
        indx <- apply(x, 2, all_is_na)
        x <- mat_select_cols(x, !indx)
    }

    # Detect and remove totally empty (NA only) rows
    if(remove_empty_rows){
        indx <- apply(x, 1, all_is_na)
        x <- mat_select_rows(x, !indx)
    }

    # After all of this, cell x[1, 1], if it exists could be a section header / text.
    # Optionally set to NA.
    if(ncol(x) && nrow(x) && remove_section_text){
        x[1, 1] <- NA
    }

    dimnames(x) <- NULL # row and col names have no meaning, if somehow they still exist.
    return(x)
}

to_list <- function(x){
    # Coerces to a list.
    # Assumes the first column is names (keys)
    # Drops empty cells by defualt
    if(!is.matrix(x) || !is.character(x) || length(dim(x)) != 2 ) {
        stop("Input must be a character matrix with two dimensions")
    }

    keys <- make_key(x[, 1]) # The first column are the keys
    ## TODO: Relax this assumption? Rely on back ticks?

    x <- x[ , -1, drop = FALSE]
    xout <- list()
    for(a in seq_along(keys)){
        vals <- x[a,]
        vals <- vals[!is.na(vals)] # Drop empty cells
        vals <- stringr::str_trim(vals) # Trim. Shouldn't really be needed...
        vals <- utils::type.convert(vals, as.is=TRUE) # Guess
        if(length(vals) > 0){
            xout[[keys[a]]] <- vals
        } else {
            xout[[keys[a]]] <- NA
        }
    }
    return(xout)
}

to_data_frame <- function(x){
    # Coerces to a dataframe.
    # Assumes the first row is (col)names.
    # The first column is promoted to data (if not all NA)
    if(!is.matrix(x) || !is.character(x) || length(dim(x)) != 2 ) {
        stop("Input must be a character matrix with two dimensions")
    }
    id_col <- x[-1,1]
    x <- to_matrix(x) # This gets appropriate dimnames
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    if(!all(is.na(id_col))){
        x <- tibble::add_column(.data = x, .id = id_col, .before = 1L)
    }
    x <- lapply(x, utils::type.convert, as.is = TRUE)
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    rownames(x) <- NULL
    return(x)

}

to_matrix <- function(x){
    # Coerces to a (potentially non-character) matrix
    # Assumes the first row is colnames.
    # Assumes the first column is rownames.
    if(!is.matrix(x) || !is.character(x) || length(dim(x)) != 2 ) {
        stop("Input must be a character matrix with two dimensions")
    }

    if(all_is_na(x[1, -1])){
        cnmes <- NULL
    } else {
        cnmes <- make_key(x[1, -1]) ## TODO: Relax this assumption? Rely on back ticks?
    }

    if(all_is_na(x[-1, 1])){
        rnmes <- NULL
    } else {
        rnmes <- make_key(x[-1, 1]) ## TODO: Relax this assumption? Rely on back ticks?
    }

    x <- utils::type.convert(x[-1, -1, drop = FALSE], as.is = TRUE)
    colnames(x) <- cnmes
    rownames(x) <- rnmes
    return(x)
}

mat_select <- function(mat, ndx){
    # stopifnot(length(mat) == length(ndx))
    mat <- mat[ndx]
    dim(mat) <- dim(ndx)
    return(mat)
}

mat_select_rows <- function(mat, ndx, drop = FALSE){
    # stopifnot(nrow(mat) == length(ndx))
    mat[ndx, , drop = drop]
}

mat_select_cols <- function(mat, ndx, drop = FALSE){
    # stopifnot(ncol(mat) == length(ndx))
    mat[ ,ndx, drop = drop]
}
