context("Parsing Functions")

test_that("`raw_text` accepts only 2D matrixes", {
    v <- c("1", "2", "3")
    a <- array(v, dim=c(1,1,3))
    expect_error(raw_text(v), "must be a character matrix with two dimensions")
    expect_error(raw_text(a), "must be a character matrix with two dimensions")
})

test_that("`raw_text` accepts only character matrixes", {
    m <- matrix(1:6, ncol = 2)
    expect_error(raw_text(m), "must be a character matrix with two dimensions")
})

test_that("`raw_text` can drop NA cols and rows", {
    m <- matrix(c(rep(c("s", "s", NA), 2), rep(NA, 3)), ncol = 3)
    r <- raw_text(m)
    expect_equal(ncol(r), 2)
    expect_equal(nrow(r), 2)
    r <- raw_text(m, remove_empty_rows = FALSE)
    expect_equal(ncol(r), 2)
    expect_equal(nrow(r), 3)
    r <- raw_text(m, remove_empty_cols = FALSE)
    expect_equal(ncol(r), 3)
    expect_equal(nrow(r), 2)
    r <- raw_text(m, remove_empty_rows = FALSE, remove_empty_cols = FALSE)
    expect_equal(ncol(r), 3)
    expect_equal(nrow(r), 3)
})

test_that("`raw_text` respects dimensions not explicity touched", {
    m <- matrix(rep(NA_character_, 9), ncol = 3, byrow = TRUE)
    r <- raw_text(m, remove_empty_cols = FALSE)
    expect_equal(ncol(r), 3)
    expect_equal(nrow(r), 0)
    r <- raw_text(m, remove_empty_rows = FALSE)
    expect_equal(ncol(r), 0)
    expect_equal(nrow(r), 3)
})

test_that("`raw_text` can remove the 'section text'", {
    m <- matrix("s", ncol = 3, nrow=3)
    r <- raw_text(m)
    expect_equal(r[1, 1], NA_character_)
    r <- raw_text(m, remove_section_text = FALSE)
    expect_equal(r[1, 1], "s")
})

test_that("`raw_text` returns an a character matrix", {
    m <- matrix("s", ncol = 3, nrow=3)
    r <- raw_text(m)
    expect_true(is.matrix(r))
    expect_type(object = r, type = "character")
})

test_that("`raw_text` returns an empty matrix if all are NA", {
    m <- matrix(NA_character_, ncol = 3, nrow=3)
    r <- raw_text(m)
    expect_true(is.matrix(r))
    expect_type(object = r, type = "character")
    expect_equal(ncol(r), 0)
    expect_equal(nrow(r), 0)
})


df <- data.frame(strings = c("takiwaR Metadata", "key", "Another Key", NA_character_),
                 integers = c(1L, 2L, 300L, NA_integer_),
                 doubles = c(1, Inf, NaN, NA_real_),
                 logicals = c(TRUE, FALSE, NA, logical(1)),
                 factors = factor(c("F1", "F2", "F3", "F3")),
                 stringsAsFactors = F)
lsdf <- list("text" = df)

mat <- matrix(c("A", "B", "C"), ncol = 3, nrow = 3)
lsmat <- list("text" = mat)

test_that("`raw_text` can handle a dataframe", {
    m <- raw_text(df)
    expect_true(is.matrix(m))
    expect_true(is.character(m))
    expect_equal(nrow(m), 4)
    expect_equal(ncol(m), 5)
    expect_null(dimnames(m)) # Ignore dimnames
    expect_equal(m[1,1], NA_character_)
    expect_equal(m[1,2], "1")
    expect_equal(m[1,3], "1")
    expect_equal(m[1,4], "TRUE")
    expect_equal(m[1,5], "F1")
    expect_equal(m[2,3], "Inf")
    expect_equal(m[2,4], "FALSE")
    expect_equal(m[3,3], "NaN")
    m2 <- raw_text(df, remove_section_text = FALSE)
    expect_equal(m2[1,1], "takiwaR Metadata")
})

test_that("`raw_text` can handle a list", {
    expect_error(raw_text(list()), "must have an item named 'text'")
    m <- raw_text(lsdf)
    expect_equal(m, raw_text(df))
    m2 <- raw_text(lsdf, remove_section_text = FALSE)
    expect_equal(m2, raw_text(df, remove_section_text = FALSE))
})

test_that("`parse_rlist` works as designed", {
    d <- parse_rlist(lsdf)
    expect_type(d, type = "list")
    expect_length(d, 3)
    expect_named(d, make_key(c("key", "Another Key", NA)))
    expect_equal(d[[1]], c("2","Inf", "FALSE", "F2"))
    d <- parse_rlist(lsmat)
    expect_type(d, type = "list")
    expect_length(d, 2)
    expect_named(d, make_key(c("B", "C")))
    expect_equal(d[[1]], c("B","B"))
    expect_equal(d[[2]], c("C","C"))

})

test_that("`parse_clist` works as designed", {
    d <- parse_clist(lsdf)
    expect_type(d, type = "list")
    expect_length(d, 4)
    expect_named(d, make_key(c(1, 1, TRUE, "F1")))
    expect_equal(d[[1]], c(2,300))
    expect_equal(d[[2]], c(Inf, NaN))
    expect_equal(d[[3]], c(FALSE, FALSE))
    expect_equal(d[[4]], c("F2", "F3", "F3"))
    d <- parse_clist(lsmat)
    expect_type(d, type = "list")
    expect_length(d, 2)
    expect_named(d, make_key(c("A", "A")))
    expect_equal(d[[1]], c("B", "C"))
    expect_equal(d[[2]], c("B", "C"))
})

test_that("`parse_df` works as designed", {
    d <- parse_df(lsdf)
    expect_s3_class(d, class = "data.frame")
    expect_named(d, c(".id", make_key(c(1, 1, TRUE, "F1"))))
    expect_equal(d[[1]], c("key", "Another Key", NA))
    expect_equal(d[[2]], c(2,300, NA))
    expect_equal(d[[3]], c(Inf, NaN, NA))
    expect_equal(d[[4]], c(FALSE, NA, FALSE))
    expect_equal(d[[5]], c("F2", "F3", "F3"))
    d <- parse_df(lsmat)
    expect_s3_class(d, class = "data.frame")
    expect_named(d, c(".id", make_key(c("A", "A"))))
    expect_equal(rownames(d), c("1", "2"))
    expect_equal(d[[1]], c("B", "C"))
    expect_equal(d[[2]], c("B", "C"))
    expect_equal(d[[3]], c("B", "C"))
})


test_that("`to_matrix` works when there is only a single row in the data part", {
    m <- matrix(c("Data", "C1", "C2", "C3", "R1", "1", "2", "3"), ncol = 4, nrow=2, byrow = TRUE)
    expect_silent(mparse <- mkm:::to_matrix(m))
    expect_equal(as.numeric(mparse), as.numeric(m[-1, -1, drop=FALSE]))
})






