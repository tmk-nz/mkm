context("Test `read_mkm`")

s <- c(meta = "takiwaR Metadata", subs = "Substrate (% Cover)",
       ppc = "Primary Producers (Counts)", isf = "Iris size frequency",
       empty = "empty section")
mat <- matrix(c("takiwaR Metadata", "key", "another key",
                "Substrate (% Cover)", "spp_a", "#NAME?",
                "Primary Producers (Counts)", "Iris size frequency", "1", "2", "empty section"),
              ncol = 1)
v <- list(dat = mat)

test_that("Only xlsx, xls and csv are supported", {
    expect_error(read_mkm("./test_files/test_mkm_read.tab", section_text = s),
                 "Only 'xlsx', 'xls' and 'csv' files are supported.")
})

test_that("`section_text` must be valid", {
    expect_error(read_mkm("./test_files/test_mkm_read.csv",
                          section_text = NULL),
                 "must be a unique character vector with at least 1 item")
    expect_error(read_mkm("./test_files/test_mkm_read.csv",
                          section_text = character(0L)),
                 "must be .+ at least 1 item")
    expect_error(read_mkm("./test_files/test_mkm_read.csv",
                          section_text = 1),
                 "must be a .+ character vector")
    expect_error(read_mkm("./test_files/test_mkm_read.csv",
                          section_text = c("test", "test")),
                 "must be .+ unique")
    expect_error(read_mkm("./test_files/test_mkm_read.csv",
                          section_text = "nope"),
                 "There was a problem.+\\('nope'\\) not found")
    expect_error(read_mkm("./test_files/test_mkm_read.csv",
                          section_text = c("nope", n2 = "again")),
                 "There were 2 problems")
})

test_that("`.match_sections` works as expected", {
    # Correctly identifies a single match and returns a named integer vector
    vo <- .match_sections(v, c(subs = "Substrate (% Cover)"))
    expect_type(vo, 'integer')
    expect_named(vo)
    expect_equal(vo, c(subs = 4))

    # Returns an ordered match, even when out of sync...
    vo <- .match_sections(v, c(subs = "Substrate (% Cover)", meta = "takiwaR Metadata"))
    expect_equal(vo, c(meta = 1, subs = 4))

    v$text <- v$text[sample(nrow(v$text)), , drop = FALSE]
    vo <- .match_sections(v, c("takiwaR Metadata", "Substrate (% Cover)", "key"))
    expect_true(all(diff(vo)>0))
})

test_that("Can read a csv", {
    expect_silent(x <- read_mkm("./test_files/test_mkm_read.csv", section_text = s))
    # Should produce a named list with the same length and names as the sections
    expect_type(x, 'list')
    expect_length(x, length(s))
    expect_named(x, names(s))

    # Should produce no format information
    expect_null(x$subs$format)

    # The `dat` object should be a list with objects:
    #   - shead, cname, rname, vals
    expect_type(x$subs$dat, "list")
    expect_named(x$subs$dat, c("shead", "cname", "rname", "vals"))

})

test_that("Can read an xls", {
    expect_silent(x <- read_mkm("./test_files/test_mkm_read.xls", section_text = s))
    expect_type(x, 'list')
    expect_length(x, length(s))
    expect_named(x, names(s))
    expect_null(x$subs$format)
    expect_type(x$subs$dat, "list")
    expect_named(x$subs$dat, c("shead", "cname", "rname", "vals"))
})

test_that("Can read an xlsx", {
    expect_silent(x <- read_mkm("./test_files/test_mkm_read.xlsx", section_text = s))
    expect_type(x, 'list')
    expect_length(x, length(s))
    expect_named(x, names(s))
    expect_type(x$subs$dat, "list")
    expect_named(x$subs$dat, c("shead", "cname", "rname", "vals"))

    # At the point of the 'clean' step, dims() should be equal between dat and format.
    raw_dat <- .read_xlsx(path = "./test_files/test_mkm_read.xlsx", sheet = 1)
    sections <- .match_sections(x = raw_dat, section_text = s)
    raw_dat <- .sectionise(x = raw_dat, sections = sections)
    raw_dat <- .clean(x = raw_dat)
    expect_equal(dim(raw_dat$subs$dat),
                 dim(raw_dat$subs$format$local$numFmt))
    expect_equal(dim(raw_dat$subs$dat),
                 dim(raw_dat$subs$format$style$numFmt))
    # Double checking all dims are equal
    expect_true(all(diff(rapply(raw_dat$meta, ncol, how = "unlist")) == 0))
    expect_true(all(diff(rapply(raw_dat$subs, ncol, how = "unlist")) == 0))
    expect_true(all(diff(rapply(raw_dat$ppc, ncol, how = "unlist")) == 0))
    expect_true(all(diff(rapply(raw_dat$isf, ncol, how = "unlist")) == 0))
    expect_true(all(diff(rapply(raw_dat$empty, ncol, how = "unlist")) == 0))
    # All should be matrixes
    expect_true(all(rapply(raw_dat, is.matrix, how = "unlist")))
})

test_that("`read_mkm` returns an empty matrix (not a vector)", {
    x <- read_mkm("./test_files/test_mkm_read.xlsx", section_text = s)
    r <- x$empty$dat$vals
    expect_true(is.matrix(r))
    expect_equal(ncol(r), 0)
    expect_equal(nrow(r), 0)
})




