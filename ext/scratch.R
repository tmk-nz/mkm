item <- Item$new(name = "test")

item$name <- 1
item$name <- "GG"
item$name
item$meta$test <- "GEPP"

item$set_prop("meta", list())
item$properties$meta$test <- "TT"
item$get_prop('meta')$test <- "HH"
item$set_prop("meta", list("test" = "This is how we do it!"))
item$properties$meta

item$data <- data.frame(A = 1:3)
item$validate()
item$get_validation() # This is an error in validate - send pull request
item$is_valid()
item$validator <- validate::validator("A" %in% names(.), A<4)
item$is_valid()
item$get_validation(summarise = TRUE)
item$validator <- validate::validator(c("A", "B") %in% names(.), A<2)
item$is_valid()
item$get_validation(summarise = TRUE)
plot(item$get_validation())






metaval <- validate::validator(all(c("key", "another_key", "date", "another_key_1") %in% names(.)))
# SheetSection$debug("initialize")
# SheetSection$debug("parser")
sec <- Section$new(search = "takiwaR Metadata",
                   parser = parse_rlist,
                   validator = metaval,
                   name = "meta")
sec$parser <- NULL
sec$parser <- 1


fd <- Sheet$new()
fd$add(search = "takiwaR Metadata", parser = parse_rlist, validator = metaval, name = "meta")
fd$add(search = "Substrate (% Cover)", parser = parse_tmat, name = "substrate")
fd$add(search = "Primary Producers (Counts)", parser = parse_tmat, name = 'prim_prod_c')
fd$add(search = "Iris size frequency", parser = parse_mat, name = "iris_sf")


f <- Sheet$new(file = "./tests/testthat/test_files/test_mkm_read.xlsx", template = fd)
f$text
f$format$local_style$fill$patternFill$fgColor$rgb
f$format$local_style$font$color$rgb
f$parse()

f$names
f$length()
f$search
f$sections
f$getSection('meta')
f$getSection('balls')

f$define(definition = fd)
f$parse()
f$parsed
f$validate()
f$validationSummary()
validate::errors(f$validation$meta)

df <- data.frame(strings = c("takiwaR Metadata", "key", "Another Key", NA_character_),
                 integers = c(1L, 2L, 300L, NA_integer_),
                 doubles = c(1, Inf, NaN, NA_real_),
                 logicals = c(TRUE, FALSE, NA, logical(1)),
                 factors = factor(c("F1", "F2", "F3", "F3")),
                 stringsAsFactors = F)
lsdf <- list("text" = df)

m2 <- mkm:::raw_text(lsdf)

parse_clist(m2)




mat <- matrix(2, nrow = 2, ncol=3)
v <- validate::validator(is.numeric(.))
confront(mat, v)
