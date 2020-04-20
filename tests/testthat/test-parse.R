context("Parsing Functions")

s <- c(meta = "takiwaR Metadata", subs = "Substrate (% Cover)",
       ppc = "Primary Producers (Counts)", isf = "Iris size frequency",
       empty = "empty section")
x <- read_mkm("./test_files/test_mkm_read.csv", section_text = s)

pl <- c(subs = to_df, ppc = "tmat", isf = "mat")

test_that("`parse_mkm` works as designed", {
    r <- parse_mkm(x = x, definition = pl)

    expect_type(r$meta, type = "list")
    expect_named(r$meta, c("key", "another_key...2", "date", "another_key...4"))

    expect_s3_class(object = r$subs, class = "data.frame")
    expect_named(object = r$subs, expected = c(".id", "1", "2"))

    expect_true(is.matrix(r$ppc))
    expect_equal(colnames(r$ppc), c("spp_d", "spp_e", "another species"))

    expect_true(is.matrix(r$isf))
    expect_null(colnames(r$isf))

    expect_s3_class(object = r$empty, class = "data.frame")
    expect_equal(nrow(r$empty), 0)
    expect_equal(ncol(r$empty), 0)

    expect_error(parse_mkm(x = x, definition = c(meta = "balls")),
                 "that can be resolved")
    expect_error(parse_mkm(x = list(nope = 1), definition = pl),
                 "only parse a list with an mkm-like structure")

})

test_that("`to_lst` works as designed", {
    d <- to_lst(x$meta)
    expect_type(d, type = "list")
    expect_length(d, 2)
    expect_named(d, c("", ""))

    expect_equal(d[[1]], c("value", "1", "2018-10-27"))
    expect_equal(d[[2]], "27/10/18")
})

test_that("`to_tlst` works as designed", {
    d <- to_tlst(x$meta)
    expect_type(d, type = "list")
    expect_length(d, 4)
    expect_named(d, c("key", "another key", "date", "another key"))

    expect_equal(d[[1]], "value")
    expect_equal(d[[2]], 1)
    expect_equal(d[[3]], c("2018-10-27", "27/10/18"))
    expect_equal(d[[4]], NA)
})

test_that("`to_meta` works as designed", {
    d <- to_meta(x$meta)
    expect_type(d, type = "list")
    expect_length(d, 4)
    expect_named(d, c("key", "another_key...2", "date", "another_key...4"))

    expect_equal(d[[1]], "value")
    expect_equal(d[[2]], 1)
    expect_equal(d[[3]], c("2018-10-27", "27/10/18"))
    expect_equal(d[[4]], NA)
})

test_that("`to_df` works as designed", {
    d <- to_df(x$ppc)
    expect_s3_class(d, class = "data.frame")
    expect_named(d, c(".id", "1", "2"))

    expect_equal(d$.id, c("spp_d", "spp_e", "another species"))
    expect_equal(d[[2]], c(NA, NA, NA))
    expect_equal(d[[3]], c(NA, 5, NA))
})

test_that("`to_tdf` works as designed", {
    d <- to_tdf(x$ppc)
    expect_s3_class(d, class = "data.frame")
    expect_named(d, c(".id", c("spp_d", "spp_e", "another species")))

    expect_equal(d$.id, c(1,2))
    expect_equal(d[[2]], c(NA, NA))
    expect_equal(d[[3]], c(NA, 5))
    expect_equal(d[[4]], c(NA, NA))
})

test_that("`to_mat` works as designed", {
    d <- to_mat(x$isf)
    expect_true(is.matrix(d))
    expect_equal(rownames(d), c("1", "2"))
    expect_equal(colnames(d), NULL)
    expect_equal(dim(d), c(2,4))
})

test_that("`to_tmat` works as designed", {
    d <- to_tmat(x$isf)
    expect_true(is.matrix(d))
    expect_equal(rownames(d), NULL)
    expect_equal(colnames(d), c("1", "2"))
    expect_equal(dim(d), c(4,2))
})

