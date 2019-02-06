context("Protos")

test_that("Protos are of class 'R6'", {
    obj <- mkm:::Proto$new(name = "test")
    expect_s3_class(object = obj, class = c("R6"))
})

test_that("Protos are initalised with all required fields, though some might be NULL", {
    expect_error(object = mkm:::Proto$new(),
                 regexp = "must be a character vector of length 1")
    obj <- mkm:::Proto$new(name = "test")
    req <- c("name", "path", "sheet", "section_text")
    expect_length(object = obj$properties, n = length(req))
    expect_true(all(req %in% names(obj$properties)))
    expect_equal(obj$name, "test")
    expect_null(object = obj$path)
    expect_null(object = obj$sheet)
    expect_null(obj$section_text)
})

test_that("Properties can be recalled via `get_prop`", {
    obj <- mkm:::Proto$new(name = "test")
    expect_equal(obj$get_prop("name"), "test")
    expect_equal(obj$get_prop(1), "test")
    expect_error(obj$get_prop(list()), "must be a character or integer vector of length 1")
    expect_error(obj$get_prop(c("name", "name2")), "must be a character or integer vector of length 1")
})

test_that("Properties can be set via `set_prop`", {
    obj <- mkm:::Proto$new(name = "test")
    # String indexing
    obj$set_prop("name", "val1")
    expect_equal(obj$get_prop("name"), "val1")
    # Numeric indexing
    obj$set_prop(1, "val2")
    expect_equal(obj$get_prop(1), "val2")
    # Indexing errors
    expect_error(obj$set_prop(list(), NULL), "must be a character or integer vector of length 1")
    expect_error(obj$set_prop(c("name", "name2"), NULL), "must be a character or integer vector of length 1")
    # Another arbitary value
    obj$set_prop("another", 1)
    expect_true("another" %in% names(obj$properties))
    expect_equal(obj$get_prop("another"), 1)
})

test_that("Properties can't be set directly", {
    obj <- mkm:::Proto$new(name = "test")
    expect_error(obj$properties$name <- "val",
                 regexp = "is read only")
    expect_error(obj$properties$another <- "val",
                 regexp = "is read only")
})

test_that("Properties with active bindings can set NULL (assuming this is allowed), but not completely removed with `remove_prop`", {
    obj <- mkm:::Proto$new(name = "test")
    obj$set_prop("path", "a/path/")
    obj$remove_prop("path")
    expect_true("path" %in% names(obj$properties))
    expect_null(obj$get_prop("path"))
})

test_that("Properties without active bindings are completely removed with `remove_prop`", {
    obj <- mkm:::Proto$new(name = "test")
    obj$set_prop("another", 1)
    obj$remove_prop("another")
    expect_false("another" %in% names(obj$properties))
    expect_null(obj$get_prop("another"))
})

test_that("Only exisiting properties are touched with numeric indexing", {
    obj <- mkm:::Proto$new(name = "test")
    expect_error(obj$get_prop(50), "must index an existing property")
    expect_error(obj$set_prop(50, NULL), "must index an existing property")
    expect_error(obj$remove_prop(50), "must index an existing property")
})

test_that("`name` can be set and recalled by various means", {
    obj <- mkm:::Proto$new(name = "test")
    obj$name <- "new" # Via active binding
    expect_equal(obj$name, "new")
    expect_equal(obj[['name']], "new")
    expect_equal(obj$get_prop("name"), "new")
    expect_equal(obj$properties$name, "new")

    obj$set_prop('name', "new2") # Via set_prop
    expect_equal(obj$name, "new2")

    expect_error(obj$name <- 1, "must be a character vector of length 1")
    expect_error(obj$set_prop('name', 1), "must be a character vector of length 1")

    expect_error(obj$name <- list(), "must be a character vector of length 1")
    expect_error(obj$name <- matrix("A"), "must be a character vector of length 1")
    # Must not be NULL (by any means)
    expect_error(obj$name <- NULL, "must be a character vector of length 1")
    expect_error(obj[['name']] <- NULL, "must be a character vector of length 1")
    expect_error(obj$remove_prop('name'), "must be a character vector of length 1")

    expect_error(obj$name <- c('val1', 'val2'), "must be a character vector of length 1")
    expect_error(obj$set_prop('name', c('val1', 'val2')), "must be a character vector of length 1")
})

test_that("`path` can be set and recalled by various means", {
    obj <- mkm:::Proto$new(name = "test")

    pth1 <- "./tests/testthat/test-Proto.R"
    pth2 <- "./tests/testthat.R"

    obj$path <- pth1 # Via active binding
    expect_equal(obj$path, pth1)
    expect_equal(obj[['path']], pth1)
    expect_equal(obj$get_prop("path"), pth1)
    expect_equal(obj$properties$path, pth1)

    obj$set_prop('path', pth2) # Via set_prop
    expect_equal(obj$path, pth2)

    obj$path <- pth1 # Can be set to NULL, but not removed
    obj$path <- NULL
    expect_true("path" %in% names(obj$properties))
    expect_null(obj$path)

    obj$path <- pth1 # Can be set to NULL, but not removed
    obj$remove_prop('path')
    expect_null(obj$path)
    expect_true("path" %in% names(obj$properties))

    obj$path <- pth1 # Can be set to NULL, but not removed
    obj[["path"]] <- NULL
    expect_null(obj$path)
    expect_true("path" %in% names(obj$properties))

    expect_error(obj$path <- 1, "must be a character vector of length 1 or NULL")
    expect_error(obj$path <- list(), "must be a character vector of length 1 or NULL")

    expect_error(obj$set_prop('path', 1), "must be a character vector of length 1 or NULL")
    expect_error(obj$set_prop('path', matrix("A")), "must be a character vector of length 1 or NULL")

    expect_error(obj$path <- c('val1', 'val2'), "must be a character vector of length 1 or NULL")
    expect_error(obj$set_prop('path', c('val1', 'val2')), "must be a character vector of length 1 or NULL")
})

test_that("`sheet` can be set and recalled by various means", {
    obj <- mkm:::Proto$new(name = "test")

    sht1 <- 1
    sht2 <- "default"

    obj$sheet <- sht1 # Via active binding
    expect_equal(obj$sheet, sht1)
    expect_equal(obj[['sheet']], sht1)
    expect_equal(obj$get_prop("sheet"), sht1)
    expect_equal(obj$properties$sheet, sht1)

    obj$set_prop('sheet', sht2) # Via set_prop
    expect_equal(obj$sheet, sht2)

    obj$sheet <- sht1 # Can be set to NULL, but not removed
    obj$sheet <- NULL
    expect_true("sheet" %in% names(obj$properties))
    expect_null(obj$sheet)

    obj$sheet <- sht1 # Can be set to NULL, but not removed
    obj$remove_prop('sheet')
    expect_null(obj$sheet)
    expect_true("sheet" %in% names(obj$properties))

    obj$sheet <- sht1 # Can be set to NULL, but not removed
    obj[["sheet"]] <- NULL
    expect_null(obj$sheet)
    expect_true("sheet" %in% names(obj$properties))

    expect_error(obj$sheet <- matrix(1), "must be a character or integer vector of length 1 or NULL")
    expect_error(obj$sheet <- matrix("A"), "must be a character or integer vector of length 1 or NULL")
    expect_error(obj$sheet <- list(), "must be a character or integer vector of length 1 or NULL")

    expect_error(obj$set_prop('sheet', list()), "must be a character or integer vector of length 1 or NULL")

    expect_error(obj$sheet <- c('val1', 'val2'), "must be a character or integer vector of length 1 or NULL")
    expect_error(obj$set_prop('sheet', c('val1', 'val2')), "must be a character or integer vector of length 1 or NULL")
})

test_that("`section_text` active binding accepts only character vectors", {
    obj <- Proto$new(name = "test", section_text = "some text")
    expect_equal(obj$section_text, "some text")
    obj$section_text <- "some more text"
    expect_equal(obj$section_text, "some more text")

    expect_error(obj$section_text <- 1)
    expect_error(obj$section_text <- list())
    expect_error(obj$section_text <- data.frame())
})

test_that("`raw` must be a list with a very specific format (or NULL)", {
    obj <- mkm:::Proto$new(name = "test")
    expect_error(obj$raw <- matrix(1), "must be a list")

    errrx <- "must contain items named '?text'? and '?format'?"
    expect_error(obj$raw <- list(), errrx)
    expect_error(obj$raw <- list(1,2), errrx)
    expect_error(obj$raw <- list(1,2,3), errrx)
    expect_error(obj$raw <- list("text" = 1), errrx)
    expect_error(obj$raw <- list("format" = 1), errrx)

    expect_error(obj$raw <- list("text" = 1, "format" = list()), "must be a character matrix")
    expect_error(obj$raw <- list("text" = matrix("A"), "format" = 1), "must be a list")

    errrx <- "must be a .+? list of matrices"
    expect_error(obj$raw <- list("text" = matrix("A"), "format" = list()), errrx)
    expect_error(obj$raw <- list("text" = matrix("A"), "format" = list("A")), errrx)

    errrx <- "must have the same dimensions"
    expect_error(obj$raw <- list("text" = matrix("A"), "format" = list(matrix(c("A","B")))), errrx)

    proto_obj <- list("text" = matrix("A"), "format" = list(matrix("A"), matrix("A"), matrix(1)))
    expect_silent(obj$raw <- proto_obj)
    expect_equal(obj$raw$text, proto_obj$text)
    expect_equal(obj$raw$format, proto_obj$format)

    proto_obj <- list("text" = matrix("A"), "format" = NULL) # This is also allowed
    expect_silent(obj$raw <- proto_obj)

    expect_silent(obj$raw <- NULL) # Raw can also be NULL
    expect_null(obj$raw)
})

test_that("Protos can read files", {
    real_pth <- "./test_files/mkm_basic_file.xlsx"
    miss_pth <- "./?%$#@!"

    # Raw is empty by default.
    obj <- mkm:::Proto$new(name = "test")
    expect_null(obj$raw)
    expect_error(obj$read(), "`?path`? cannot be NULL")

    # Read with path and sheet supplied (and save info to use again)
    obj$read(path = real_pth, sheet = 1)
    expect_equal(obj$path, real_pth)
    expect_equal(obj$sheet, 1)
    expect_type(obj$raw, "list")
    obj$read()
    expect_type(obj$raw, "list")

    # Read with only path supplied
    obj2 <- mkm:::Proto$new(name = "test")
    obj2$read(path = real_pth)
    expect_equal(obj$raw,
                 obj2$raw)

    obj <- mkm:::Proto$new(name = "test")
    expect_error(obj$read(path = miss_pth), "not found")
})
