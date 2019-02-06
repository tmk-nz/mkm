unique_not_na <- function(...){
    x <- unlist(list(...), recursive = TRUE, use.names = FALSE)
    if(all_is_na(x)){
        return(NA)
    }
    x <- unique(x[!is.na(x)]) # Unique, non-NA
    return(x)
}

all_is_na <- function(...){
    # Could move to assertive package?
    x <- unlist(list(...), recursive = TRUE, use.names = FALSE)
    logi <- all(is.na(x))
    return(logi)
}

is_named_list <- function(x){
    if(!inherits(x, 'list')) return(FALSE)
    if(length(names(x)) != length(x)) return(FALSE)
    return(TRUE)
}

# print.object_with_msg = function (x, ...)
# {
#     if(is.null(attr(x, which = "msg", exact = TRUE))){
#         stop("`x` is malformed. It must be a vector with a 'msg' attribute.")
#     }
#     msg <- attr(x, which = "msg", exact = TRUE)
#     attr(x, which = "msg", exact = TRUE) <- NULL
#     print(x, ...)
#     cat("Message:", msg, "\n")
# }

read_mkm <- function(path, sheet = NULL){
    if(is.null(sheet)){
        sheet <- 1
    }
    ext <- tools::file_ext(path)
    if(!ext %in% c("xlsx", "xls", "csv")) {
        stop("Only 'xlsx', 'xls' and 'csv' files are supported.")
    }
    txt <- NULL
    format <- NULL
    if(ext == "xlsx"){
        raw <- tidyxl::xlsx_cells(path = path, sheets = sheet)
        # The internet says I should move to a tidyverse approach...
        # RIP base::apply and reshape2::acast...
        # I don't like it, but in this case I think it is cleaner.
        raw$all <- raw %>% dplyr::select(logical, numeric, date, character) %>%
            dplyr::mutate_all(as.character) %>%
            purrr::pmap_chr(mkm:::unique_not_na)

        txt <- raw %>% dplyr::select(row, col, all) %>%
            tidyr::spread(key = col, value = all) %>%
            dplyr::select(-row) %>% as.matrix()
        ndx_lf <- raw %>% dplyr::select(row, col, local_format_id) %>%
            tidyr::spread(key = col, value = local_format_id) %>%
            dplyr::select(-row) %>% as.matrix()
        ndx_sf <- raw %>% dplyr::select(row, col, style_format) %>%
            tidyr::spread(key = col, value = style_format) %>%
            dplyr::select(-row) %>% as.matrix()

        # Now that we are deailing with character matrixes, I feel better about using base:: functions.

        indx <- apply(txt, 2, all_is_na) # Detect totally empty (NA only) **data** columns
        txt <- mat_select_cols(txt, !indx) # Remove empty columns
        ndx_lf <- mat_select_cols(ndx_lf, !indx) # Apply the same indx on the local format
        ndx_sf <- mat_select_cols(ndx_sf, !indx) # Apply the same indx on the style format

        indx <- apply(txt, 1, all_is_na) # Detect totally empty (NA only) **data** rows
        txt <- mat_select_rows(txt, !indx) # ...
        ndx_lf <- mat_select_rows(ndx_lf, !indx) # ...
        ndx_sf <- mat_select_rows(ndx_sf, !indx) # ...

        dimnames(ndx_lf) <- dimnames(ndx_sf) <- dimnames(txt) <- NULL

        frmts <- tidyxl::xlsx_formats(path = path)
        # Apparently there is no recursive map in the tidyverse. So for now we keep this:
        local <- rapply(object = frmts$local, f = mat_select, how = "replace", ndx = ndx_lf)
        style <- rapply(object = frmts$style, f = mat_select, how = "replace", ndx = ndx_sf)
        format <- list("local_style" = local, "global_style" = style)
    } else if(ext == "xls"){
        raw <- readxl::read_excel(path = path, sheet = sheet, col_types = 'list', col_names = FALSE)
        txt <- raw %>% dplyr::mutate_all(as.character) %>% as.matrix()
        dimnames(txt) <- NULL
        txt <- raw_text(txt, remove_section_text = FALSE)
    } else if(ext == "csv"){
        raw <- read.csv(file = path, header = FALSE, colClasses = "character",
                        stringsAsFactors = FALSE, na.strings = "")
        txt <- raw %>% as.matrix()
        dimnames(txt) <- NULL
        txt <- raw_text(txt, remove_section_text = FALSE)
    }
    return(list("text" = txt, "format" = format))
}

# find_sections <- function(raw, structure){
#     # Search for the sections in the first column
#     sections <- data.frame("name" = names(structure),
#                            "start" = NA, "end" = NA,
#                            "matches" = 0,
#                            "missing" = FALSE,
#                            "multi_match" = FALSE,
#                            stringsAsFactors = FALSE)
#
#     # haystack <- tolower(stringr::str_replace_all(raw[, 1], "\\s", ""))
#     haystack <- raw[, 1]
#     # needles <- tolower(stringr::str_replace_all(file_structure$search, "\\s", ""))
#     needles <- file_structure$search
#
#     sections$start <- match(needles, haystack) # Find the start positions
#     sections$matches <- rowSums(!adist(needles,haystack), na.rm = TRUE) # Count matches
#     sections$missing <- sections$matches == 0 # Flag missing sections
#     sections$multi_match <- sections$matches > 1 # Flag multiple matches
#     sections <- sections[order(sections$start), ] # Sort by start values
#
#     # Sort out end values. Ignore sections that aren't matched.
#     sections[!sections$missing, 'end'] <- c(sections[!sections$missing, 'start'][-1]-1, nrow(raw))
#
#     msg <- character()
#     if(any(sections$missing)){
#         msg <- sprintf("The following section%s not found: \n%s",
#                        ifelse(sum(sections$missing) > 1, 's were', ' was'),
#                        paste(sections[sections$missing, 'name'], collapse = ", "))
#     }
#
#     if(any(sections$multi_match)){
#         if(length(msg)) msg <- paste0(msg, "\n\n")
#         msg <- sprintf("The following section%s matched more than once: \n%s",
#                        ifelse(sum(sections$multi_match) > 1, 's were', ' was'),
#                        paste(sections[sections$multi_match, 'name'], collapse = ", "))
#     }
#
#     if(length(msg)){
#         stop(msg)
#     }
#
#     return(sections)
# }
