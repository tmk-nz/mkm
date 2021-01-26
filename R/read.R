#'Read an MKM-structured spreadsheet
#'
#'@param path A string. The location of an \code{xlsx}, \code{xls} or \code{csv}
#'  files to read.
#'@param section_text A (optionally named) character vector or scalar. See Details
#'@param sheet The sheet to read. Either a string (the name of a sheet), or an
#'  integer (the position of the sheet). Ignored for \code{csv} files.
#'
#'@details This function reads either an \code{xlsx}, \code{xls} or \code{csv}
#'  file, searches the first (non-empty) column for the text contained in
#'  \code{section_text} and returns an object with various 'sections' that can then
#'  be further parsed into sensible R data structures (e.g. via
#'  \code{\link{parse_mkm}}).
#'
#'  Sections are denoted by one or more plain text strings (\code{search_text}). If
#'  these are supplied as a named character vector, these names are used for the
#'  sections in the returned list. Where one or more names are missing or blank
#'  the search text is used and all names are made syntactically valid and unique (via
#'  \code{\link{make_keys}}).
#'
#'
#'@return A named list with \code{length(sections)} items representing the
#'  sections extracted from the file. Each item is a list with two sections:
#'
#'  \itemize{
#'
#'  \item{\code{dat}: Data extracted from the section}
#'
#'  \item{\code{format}: Formatting of the data in the section. Always
#'  \code{NULL} for \code{.xls} and \code{.csv} files} because formatting cannot
#'  be read from these file types.
#'
#'  }
#'
#'
#'@examples
#'
#'@export

read_mkm <- function(path, section_text, sheet = 1){
    # Check path exists? TODO

    # Read Data
    ext <- tools::file_ext(path)
    if(ext == "xlsx"){
        raw_dat <- .read_xlsx(path = path, sheet = sheet)
    } else if(ext == "xls"){
        raw_dat <- .read_xls(path = path, sheet = sheet)
    } else if(ext == "csv"){
        raw_dat <- .read_csv(path = path)
    } else {
        stop("Only 'xlsx', 'xls' and 'csv' files are supported.")
    }

    # Find section text in the first column
    sections <- .match_sections(x = raw_dat, section_text = section_text)

    # Find and breakup sections
    raw_dat <- .sectionise(x = raw_dat, sections = sections)

    # Clean each each section
    raw_dat <- .clean(x = raw_dat)

    # Pre-parse
    raw_dat <- .pre_parse(x = raw_dat)

    # Return a named list
    return(raw_dat)
}

# .read_* functions - take a path (and optionally a sheet)
#                   - return a list with `text` and `format` objects
.read_xlsx <- function(path, sheet){
    raw_dat <- tidyxl::xlsx_cells(path = path, sheets = sheet)
    frmts <- tidyxl::xlsx_formats(path = path)

    # Create numerical indexes.
    raw_dat <- raw_dat %>% dplyr::rowwise() %>%
        dplyr::mutate(all = unique_not_na(c(logical, numeric, date, character)))
    # TODO: Refactor e.g. to .tidyxl_to_mat(...)
    txt <- raw_dat %>% dplyr::select("row", "col", "all") %>%
        tidyr::pivot_wider(names_from = "col", values_from = "all") %>%
        dplyr::select(-row) %>% as.matrix()

    ndx_lf <- raw_dat %>% dplyr::select("row", "col", "local_format_id") %>%
        tidyr::pivot_wider(names_from = "col", values_from = "local_format_id") %>%
        dplyr::select(-row) %>% as.matrix()

    ndx_sf <- raw_dat %>% dplyr::select("row", "col", "style_format") %>%
        tidyr::pivot_wider(names_from = "col", values_from = "style_format") %>%
        dplyr::select(-row) %>% as.matrix()

    # Apparently there is no recursive map in the tidyverse. So for now we keep rapply().
    # As at 2020-04-07:
    # Look at ?purrr::modify_depth? Possible (with depth = -1) but potenitally unstable?
    local <- rapply(object = frmts$local, f = .mat_select, how = "replace", ndx = ndx_lf)
    style <- rapply(object = frmts$style, f = .mat_select, how = "replace", ndx = ndx_sf)
    format <- list("local" = local, "style" = style)
    return(list("dat" = txt, "format" = format))
}

.read_xls <- function(path, sheet){
    raw_dat <- suppressMessages(
        readxl::read_excel(path = path, sheet = sheet,
                           col_types = 'text', col_names = FALSE))
    txt <- raw_dat %>% as.matrix()
    dimnames(txt) <- NULL
    return(list("dat" = txt, "format" = NULL))
}

.read_csv <- function(path){
    raw_dat <- utils::read.csv(file = path, header = FALSE, colClasses = "character",
                           stringsAsFactors = FALSE, na.strings = "")
    txt <- raw_dat %>% as.matrix()
    dimnames(txt) <- NULL
    return(list("dat" = txt, "format" = NULL))
}

# .match_sections takes a vector of section text (needles) and the raw data (a
# haystack) to search and it returns a named integer vector of position matches
.match_sections <- function(x, section_text){

    # Check Sections
    msg <- "`section_text` must be a unique character vector with at least 1 item"
    if(length(section_text) < 1) stop(msg)
    if(!rlang::is_character(section_text)) stop(msg)
    if(length(section_text) != length(unique(section_text))) stop(msg)

    # Coerce section_text into a named character vector
    section_text <- set_keys(section_text)

    # Check the haystack is a character vector
    haystack = x$dat[, 1]
    msg <- "Internal error: `raw_data` must be a character vector"
    if(!rlang::is_character(haystack)) stop(msg)

    # Go
    out <- list() # TODO - just use vector not a list

    for(a in seq_along(section_text)){
        nme <- names(section_text)[a]
        # Could section_text[a] be NULL? TODO: Handle later.

        mtch <- haystack %in% section_text[a]

        if(sum(mtch) == 0){
            msg <- sprintf("Section text ('%s') not found",
                           section_text[a])
            v <- NA_integer_
            comment(x = v) <- msg
            out[[nme]] <- v
        } else if(sum(mtch) > 1){
            msg <- sprintf("Section text ('%s') matched %1.0f times",
                           section_text[a], sum(mtch))
            v <- NA_integer_
            comment(x = v) <- msg
            out[[nme]] <- v
        } else {
            out[[nme]] <- which(mtch)
        }
    }

    # Check for errors
    ndx <- purrr::map_int(out, `[[`, 1)
    na_indx <- is.na(ndx)
    if(any(na_indx)){
        msg <- ifelse(sum(na_indx) > 1,
                      sprintf("There were %d problems when detecting sections:",
                              sum(na_indx)),
                      "There was a problem when detecting sections:")
        errs <- out[na_indx]
        for(a in seq_along(errs)){
            msg <- sprintf("%s\n\t%s: %s", msg, names(errs)[a], comment(errs[[a]]))
        }
        stop(msg)
    }

    # Order and return
    ndx[order(ndx)]
}

# Sectionise takes the the entire raw object and section matches and returns a
# list of length(sections)
.sectionise <- function(x, sections){
    full_seq <- c(sections, nrow(x$dat))

    out <- list()

    for(a in seq_along(sections)){
        rows <- seq(from = full_seq[a], to = full_seq[a+1]-1)
        nme <- names(sections)[a]
        out[[nme]] <- rapply(object = x, f = .mat_select_rows,
                             how = "replace", ndx = rows)
    }

    return(out)
}

# .clean takes the the entire raw object and applies .clean to each section.
# Just a simple refactor for readability, above.
.clean <- function(x){
    x <- lapply(x, .clean_sec)
    return(x)
}

# .clean_sec handles each section individually. It detects empty rows and columns in
# the text data and propagates those changes to the format object (if present).
.clean_sec <- function(x){
    # if(is.null(x)) return(NULL)
    # Detect totally empty (NA only) **data** columns
    cols <- !apply(x$dat, 2, function(x) all(is.na(x)))
    x <- rapply(object = x, f = .mat_select_cols,
                  how = "replace", ndx = cols)

    # Detect totally empty (NA only) **data** rows
    rows <- !apply(x$dat, 1, function(x) all(is.na(x)))
    x <- rapply(object = x, f = .mat_select_rows,
                  how = "replace", ndx = rows)

    return(x)
}

# .pre_parse takes the entire raw object (of matrixes) and 'pre parses' everything into a list with items: "stext", "cname", "rname", "vals"
.pre_parse <- function(x){
    rapply(x, .pre_parse_sec, how = "replace")
}

# .pre_parse takes an individual matrix and breaks it into it's various components
.pre_parse_sec <- function(x){
    shead <- x[ 1,  1]
    cname <- x[ 1, -1]
    rname <- x[-1,  1]
    vals  <- utils::type.convert(x[-1, -1, drop = FALSE], as.is = TRUE)
    list('shead' = shead, 'cname' = cname, 'rname' = rname, 'vals' = vals)
}

.mat_select <- function(mat, ndx){
    mat <- mat[ndx]
    dim(mat) <- dim(ndx)
    return(mat)
}

.mat_select_rows <- function(mat, ndx, drop = FALSE){
    mat[ndx, , drop = drop]
}

.mat_select_cols <- function(mat, ndx, drop = FALSE){
    mat[ , ndx, drop = drop]
}
