unique_not_na <- function(x){
    if(all(is.na(x))) return(as.vector(NA, mode = typeof(x)))
    unique(stats::na.omit(x)) # Unique, non-NA
}
