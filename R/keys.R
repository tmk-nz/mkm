#' Keys
#'
#' Keys are names that are syntactically valid and unique.
#'
#' @param x Input (coerced to a character vector) that will be converted to a
#'   string. See Detials.
#'
#' @details Naming things is hard. By their nature, any naming scheme or
#'   philosophy is opinionated.
#'
#'   The philosophy here is focused specifically on producing syntactically
#'   valid and unique names. The word 'keys' has been adopted to distinguish
#'   this from the various degrees of syntactic validity and uniqueness that are
#'   \emph{allowed} within R (even if they are not \emph{recomended}).
#'
#'   There are a number of very good naming schemes within the R ecosystem.
#'   Apart from the base-R function \code{\link{make.names}}, high-profile
#'   examples include the \code{\link[vctrs::vec_as_names]{tidyverse}} and the
#'   \code{\link[janitor::make_clean_names]{janitor package}}). These functions
#'   adopt elements from all three.
#'
#'   The overarching philosophy here is that if a name is syntactically valid
#'   and unique, then no conversion is performed, however 'ugly' that name might
#'   be. This is consistent with \code{make.names} and the \code{tidyverse} but
#'   contrasts the \code{janitor} package, which will alter a valid name.
#'
#'   If conversion is required, the following logic is applied:
#'
#'   \itemize{
#'
#'   \item All input is coerced to a character (\code{as.character()}) and
#'   missing values (NA) are converted to empty strings ("").
#'
#'   \item Some 'special' strings are replaced with syntactically valid
#'   alternatives (see table below).
#'
#'   \item Non-syntactic characters that appear \strong{before} the first
#'   letter, number or underscore are replaced by ("filled with") dots.
#'
#'   \item Non-syntactic characters or separators (or sequences of these) that
#'   appear \strong{after} this point are replaced with ("collapsed to") a
#'   single dot OR an underscore.
#'
#'   \itemize{
#'
#'   \item Which separator character to use is determined by the input. The
#'   underscore ("_") character is the default, unless the input string contains
#'   dots, in which case, a dot is used.
#'
#'   }
#'
#'   \item Unless the string is a \link[base::reserved]{reserved word}, trailing
#'   non-syntactic characters or separators are removed.
#'
#'   \item Dots are prepended, as needed, so that the start of the string
#'   syntactically valid.
#'
#'   \item If the string is a reserved word, or one is created by prior
#'   modifications then it is prepended by a single dot.
#'
#'   \item Finally, if values are not unique, they are suffixed with \code{...j} where
#'   \code{j} is an integer representing the position.
#'
#'   }
#'
#'   The \code{tidyverse} 'Ugly, with a purpose' philosophy.
#'
#'   \tabular{lll}{
#'
#'   \strong{Item}  \tab  \strong{Replacement}  \tab  \strong{Example}\cr
#'
#'   "\%"           \tab  "percent"             \tab "\% cover" => "percent_cover"\cr
#'
#'   "#"            \tab  "number"              \tab "Plants (#) => "plants_number"\cr
#'
#'   "$"            \tab  "dollar"              \tab "$ Spent" => "dollar_Spent"\cr
#'
#'   "[x]^2"        \tab  "[x] squared"         \tab "1m^2" => "..1m_squared"\cr
#'
#'   "[x]^-1"       \tab   "per [x]"            \tab "m s^-1" => "m_per_s"\cr
#'
#'   "[x]^-2"       \tab   "per [x] squared"    \tab "m^-2" => "per_m_squared"
#'
#'   }
#'
#'
#'   Where [x] is any alphanumeric character string
#'
#' @aliases make_keys set_keys
#'
#' @return For \code{make_key}, a character vector of \code{length(x)}. For \code{set_keys}, \code{x} but named.

#' @export
make_keys <- function(x){
    # Convert all input to a character
    x <- as.character(x)

    if(length(x) == 0) return(x)

    # Ensure that missing values (NA) are equal to empty strings ("")
    x[is.na(x)] <- ""

    # Apply rules to non-syntactic values only
    if(any(.is_not_syntactic(x))){
      x[.is_not_syntactic(x)] <- .apply_rules(x[.is_not_syntactic(x)])
    }

    # Now - make unique with "...n" if not unique or if  == ""
    pos <- which(x == "" | vctrs::vec_duplicate_detect(x))
    x[pos] <- paste0(x[pos], "...", pos)

    return(x)
}

#' @export
set_keys <- function(x){
    # Ensure nmes is a character string of length(x)
    nmes <- rlang::names2(x)

    # If x is actually character string promote the item wherever name is empty
    if(rlang::is_character(x)){
        indx <- nmes == ""
        nmes[indx] <- x[indx]
    }

    nmes <- make_keys(nmes)

    # Set names and return
    rlang::set_names(x = x, nm = nmes)
}

## Not exported below...

.apply_rules <- function(x){
  input <- x # For later (might move that logic up, if needed)

  # Replace some 'special' strings with syntactically valid alternatives.
  # These rules don't overlap. So they can be applied in series
  x <- stringr::str_replace_all(x, "%", "percent")
  x <- stringr::str_replace_all(x, "#", "number")
  x <- stringr::str_replace_all(x, "\\$", "dollar")
  x <- stringr::str_replace_all(x, "\\^2", " squared")
  x <- stringr::str_replace_all(x, "([:alnum:]+)\\^-2", "per \\1 squared")
  x <- stringr::str_replace_all(x, "([:alnum:]+)\\^-1", "per \\1")

  # Non-syntactic characters that appear before the first letter, number or
  # underscore are replaced ("filled") with dots and trailing non-syntactic
  # characters are removed.

  # Best to do this in bits
  parts <- stringr::str_match(x, "^([^[:alnum:]_]*)(.*)")
  parts[, 2] <- stringr::str_replace_all(parts[, 2], ".", ".")
  parts[, 3] <- stringr::str_replace_all(parts[, 3], "[^[:alnum:]._]*$", "")
  x <- paste0(parts[, 2], parts[, 3])

  # Remaining non-syntactic characters or seperators (or sequences of these)
  # are replaced with a single dot OR an underscore.
  # Which seperator character to use is determined by the input.
  has_dot <- stringr::str_detect(input, "\\.")
  has_uscore <- stringr::str_detect(input, "_")
  repl <- ifelse(has_dot, ".", "_")

  # Best done in bits (to preserve leading dots and underscores)
  parts <- stringr::str_match(x, "^([._]*)(.*)")
  parts[, 3] <- stringr::str_replace_all(parts[, 3], "[^[:alnum:]]+", repl)

  # Also, remove trailing seperators, if it is not a reserved word
  # Best done in bits too...
  indx <- .is_not_reserved(x)
  parts[indx, 3] <- stringr::str_replace(parts[indx, 3], "[._]+$", "")

  # Rejoin bits
  x <- paste0(parts[, 2], parts[, 3])

  # Make the *start* syntactic.
  x <- .make_syntactic_start(x)

  # At this point we might have conceivably created reserved words
  # e.g. ". 2" => "..2" and ".  " => "..."
  # Or otherwise non-syntactic names?
  # e.g. ??? Hmmm... No.
  x <- .fix_reserved_words(x)

  return(x)
}

.make_syntactic_start <- function(x){
  # Append dots so that the start is valid (literally).
  # "... starts with a letter or the dot not followed by a number."
  # Other non-syntactic issues are dealt with later.

  # Add a single dot to ""
  # "" => "." A valid name
  empty <- x == ""
  x[empty] <- paste0(".", x[empty])
  # TODO: The following regex could pick this up too, if
  # it was smarter. This might work "^[^[:alpha:].]|$"
  # But for now this is KISS.

  # Add a dot if it starts with anything other that a dot or [:alpha:]
  # "_" => "._" A valid name
  # "1" => ".1" Not valid (yet)
  # " " => ". " Not valid (yet)
  no_valid_start <- stringr::str_detect(x, "^[^[:alpha:].]")
  x[no_valid_start] <- paste0(".", x[no_valid_start])

  # Add a dot if it starts with a dot and a digit.
  # ".1" => "..1" Have created a reserved word (but that is OK)
  # ". " => ".. " Not valid (yet)
  # ".. " => ".. " Nothing to do here (yet)
  dot_digi <- stringr::str_detect(x, "^[.][[:digit:]]")
  x[dot_digi] <- paste0(".", x[dot_digi])

  return(x)
}

.fix_reserved_words <- function(x){
  # Add dots to reserved words...
  # In general: reserved => .reserved
  # "Inf" => ".Inf"
  # "..." => "...."
  # "..1" => "...1"
  x[.is_reserved(x)] <- paste0(".", x[.is_reserved(x)])

  return(x)
}

.reserved <- function(){
    c("if", "else", "repeat", "while", "function", "for", "in", "next", "break",
      "TRUE", "FALSE", "NULL", "Inf", "NaN",
      "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", "...")
}

.is_reserved <- function(x){
    # It is reserved if it is in reserved() or matches ..1, ..2, etc.
    # See ?reserved
    x %in% .reserved() | .is_dot_dot_number(x)
}

.is_not_reserved <- function(x){
  !.is_reserved(x)
}

.is_dot_dot_number <- function(x){
  stringr::str_detect(x, "^\\.\\.[1-9][0-9]*$")
}

.is_syntactic <- function(x){
  # Is the name syntactic?

  # From the docs:
  # A syntactically valid name consists of letters, numbers and the dot or
  # underline characters and starts with a letter or the dot not followed by a
  # number. Names such as ".2way" are not valid, and neither are the reserved
  # words.

  # Things that are syntactic are:
  # - Equal to the output of make.names, AND
  # - Not "reserved", including "..1", "..2" etc and "...",
  #   which make.names() (strangely) doesn't touch.

  make.names(x) == x & !.is_reserved(x)

}

.is_not_syntactic <- function(x){
  # Things that are NOT syntactic:
  # - Anything modified by make.names, OR
  # - Anything "reserved", including "..1", "..2" etc and "...",
  #   which make.names() (strangely) doesn't modify

  !.is_syntactic(x)

}
