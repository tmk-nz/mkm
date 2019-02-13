context("Collections")

test_that("Collections are of class 'Proto'", {
    obj <- Collection$new(name = "test")
    expect_s3_class(object = obj, class = c("Proto"))
})

test_that("Collections are initalised with all required fields, with expected defaults", {
    expect_error(object = Collection$new(),
                 regexp = "must be a character vector of length 1")
    obj <- Collection$new(name = "test")
    expect_null(obj$data)
    expect_length(obj$data, 0)
})

test_that("The `content` active binding accepts only a list containing uniquely named Items or Collections", {
    obj <- Collection$new(name = "test")
    expect_error(obj$content <- data.frame()) # Not a list
    expect_error(obj$content <- list(data.frame())) # Not the right type
    expect_silent(obj$content <- list(Item$new("Item1"), Collection$new("Colly")))
    expect_length(obj$content, 2)
    obj <- Collection$new(name = "test")
    expect_error(obj$content <- list(Item$new("Item"), Item$new("Item")), "unique names")
})

test_that("The `content` active binding will recurse sensibly", {
    obj <- Collection$new(name = "test")
    obj$content <- list(Item$new("Item1"), Item$new("Item2"), Collection$new("Colly"))
    # Can manipulate Items
    expect_error(obj$content$Item1 <- "Test", "must be a list")
    expect_error(obj$content$Item1$nah_bro <- "Test", "cannot add bindings to a locked environment")
    # Named indexing
    expect_silent(obj$content$Item1$content <- "Test")
    expect_equal(obj$content$Item1$content, "Test")
    # Numeric indexing
    expect_silent(obj$content[[1]]$content <- "TestUpdated")
    expect_equal(obj$content[[1]]$content, "TestUpdated")

    expect_equal(obj$content$Item1$name, "Item1")
    expect_silent(obj$content$Item1$name <- "Item1Updated")
    expect_equal(obj$content$Item1$name, "Item1Updated")

    # Other items remain untouched
    expect_null(obj$content$Item2$content)
    expect_equal(obj$content$Item2$name, "Item2")

    # Can manipulate collections
    expect_null(obj$content$Colly$content)
    expect_length(obj$content$Colly$content, 0)
    obj$content$Colly$content <- list(Item$new("NestedTest"))
    expect_equal(obj$content$Colly$content$NestedTest$name, "NestedTest")
    expect_equal(obj$content$Colly$content[[1]]$name, "NestedTest")
})

test_that("The `length` active binding gives number of items and is read only", {
    obj <- Collection$new(name = "test")
    expect_equal(obj$length, 0)
    obj$content <- list(Item$new("Item1"), Item$new("Item2"), Collection$new("Colly"))
    expect_equal(obj$length, 3)
    expect_error(obj$length <- 200, "read only")
})

test_that("The `names` active binding can recall and set names of the items", {
    obj <- Collection$new(name = "test")
    obj$content <- list(Item$new("Item1"), Item$new("Item2"), Collection$new("Colly"))
    expect_equal(obj$names, c("Item1", "Item2", "Colly"))

    expect_error(obj$names <- list(), "character vector")
    expect_error(obj$names <- "change", "length 3")

    expect_silent(obj$names <- c("Item1u", "Item2u", "Collyu"))
    expect_equal(obj$names, c("Item1u", "Item2u", "Collyu"))

    expect_error(obj$names <- c("Item", "Item", "Colly"), "must be unique")
})

test_that("Names on the `content` active binding update when content objects are changed", {
    obj <- Collection$new(name = "test")
    obj$content <- list(Item$new("Item1"), Item$new("Item2"), Collection$new("Colly"))
    expect_true(all(c("Item1", "Item2", "Colly") %in% obj$names))
    obj$content$Item1$name <- "Item1u"
    expect_true("Item1u" %in% obj$names)
    expect_true("Item1u" %in% names(obj$content))
    expect_true("Item1u" %in% names(obj$.__enclos_env__$private$.props$content))
    # Attempting to set a duplicate name
    # The name is actually set, but then reverted when it cascades to the `$content` binding.
    expect_error(obj$content$Item2$name <- "Item1u", "must have unique names")
    expect_equal(names(obj$.__enclos_env__$private$.props$content), obj$names)
})

test_that("Names on the `content` active binding respect content objects, even (perhaps, most importantly) on clones", {
    obj1 <- Collection$new(name = "test")
    obj1$content <- list(Item$new("Item1"), Item$new("Item2"), Collection$new("Colly"))
    obj2 <- obj1$clone()

    # Change obj1...
    # The names on obj2 should respect the pass-by-reference and the content-object-has-priority models
    # This despite the fact that obj2 is a clone (other properties of obj2 not linked... Just the names)
    obj1$content$Item1$name <- "Item1u"
    expect_true("Item1u" %in% obj2$names)
    expect_true("Item1u" %in% names(obj2$content))
    expect_true("Item1u" %in% names(obj2$.__enclos_env__$private$.props$content))

    # Prior notes:
    # This is a hangover from the $names issue.
    # One option would be possible to set names on the output of $content... [DONE]
    # This kind of breaks the model...
    # The trouble is that names on the items can change the collection.
    # There is nothing else that does this, so it could be posisble to make an exception... [DONE]
    # Maybe there is a Better Way
})

test_that("Collections can parse content from file", {
    clct <- mkm:::Collection$new(name = "test", path = "./test_files/mkm_basic_file.xlsx")
    clct$content <- list(
        Item$new(name = "meta", section_text = "takiwaR Metadata", parser = parse_rlist),
        Item$new(name = "substrate", section_text = "Substrate (% Cover)", parser = parse_tmat),
        Item$new(name = "prim_prod_c", section_text = "Primary Producers (Counts)", parser = parse_tmat),
        Item$new(name = "isf", section_text = "Iris size frequency", parser = parse_mat))
    expect_null(clct$raw)
    expect_warning(clct$parse(), "File not read yet")
    expect_type(clct$raw, "list")
    expect_type(clct$content, "list")
    expect_named(clct$content, c("meta", "substrate", "prim_prod_c", "isf"))
    expect_type(clct$content$meta$content, "list")
    expect_named(clct$content$meta$content, c("key", "another_key"))
    expect_true(is.matrix(clct$content$substrate$content))
    expect_equal(nrow(clct$content$substrate$content), 2)
    expect_true(is.matrix(clct$content$prim_prod_c$content))
    expect_equal(nrow(clct$content$prim_prod_c$content), 2)
    expect_true(is.matrix(clct$content$isf$content))
    expect_equal(nrow(clct$content$isf$content), 2)

    clct$content$meta$section_text <- NULL
    expect_error(clct$parse(), "1 item not found.+is undefined")
    clct$content$substrate$section_text <- 'test'
    expect_error(clct$parse(), "2 items not found.+matched 0 times")

})

test_that("Collections can parse a nested collection from file... ('Seperated Collection Mode')", {
    meta <- Item$new(name = "meta", section_text = "takiwaR Metadata", parser = parse_rlist)
    subs <- Item$new(name = "substrate", section_text = "Substrate (% Cover)", parser = parse_tmat)

    nested_clct <- mkm:::Collection$new(name = "nested", section_text = "Nested Section")
    nested_meta <- Item$new(name = "meta", section_text = "Nested Metadata", parser = parse_rlist)
    nested_subs <- Item$new(name = "substrate", section_text = "Nested Substrate", parser = parse_tmat)

    nested_clct$content <- list(nested_meta, nested_subs) # TODO: This could use templating!

    clct <- mkm:::Collection$new(name = "test", path = "./test_files/mkm_nested_collection.xlsx")
    clct$content <- list(meta, subs, nested_clct)

    expect_null(clct$raw)
    expect_warning(clct$parse(), "File not read yet")
    expect_type(clct$raw, "list")
    expect_type(clct$content, "list")
    expect_named(clct$content, c("meta", "substrate", "nested"))

    expect_type(clct$content$nested$content, "list")
    expect_named(clct$content$nested$content, c("meta", "substrate"))
    d <- clct$content$nested$content # TODO: Here a 'to_list' method would be useful...

    expect_type(d$meta$content, "list")
    expect_named(d$meta$content, c("key", "another_key"))
    expect_true(is.matrix(d$substrate$content))
    expect_equal(nrow(d$substrate$content), 2)
})

test_that("Collections can parse a nested collection from file... ('Chunking Mode')", {
    # Top level:
    meta <- Item$new(name = "meta", section_text = "takiwaR Metadata",
                     parser = parse_rlist)
    # Sub level:
    subs <- Item$new(name = "substrate", section_text = "Substrate (% Cover)",
                     parser = parse_tmat)
    pp <- Item$new(name = "prim_prod_c", section_text = "Primary Producers (Counts)",
                   parser = parse_tmat)
    isf <- Item$new(name = "isf", section_text = "Iris size frequency",
                    parser = parse_mat)

    nested_clct <- mkm:::Collection$new(name = "nested", section_text = "Substrate (% Cover)",
                                        content = list(subs, pp, isf))

    clct <- mkm:::Collection$new(name = "test", path = "./test_files/mkm_basic_file.xlsx")
    clct$content <- list(meta, nested_clct)

    expect_null(clct$raw)
    expect_warning(clct$parse(), "File not read yet")

    expect_type(clct$raw, "list")
    expect_type(clct$content, "list")
    expect_named(clct$content, c("meta", "nested"))
    expect_type(clct$content$meta$content, "list")
    expect_named(clct$content$meta$content, c("key", "another_key"))

    expect_type(clct$content$nested$content, "list")
    expect_named(clct$content$nested$content, c("substrate", "prim_prod_c", "isf"))
    d <- clct$content$nested$content # TODO: Here a 'to_list' method would be useful...

    expect_true(is.matrix(d$substrate$content))
    expect_equal(nrow(d$substrate$content), 2)
    expect_true(is.matrix(d$prim_prod_c$content))
    expect_equal(nrow(d$prim_prod_c$content), 2)
    expect_true(is.matrix(d$isf$content))
    expect_equal(nrow(d$isf$content), 2)

    clct$content$nested$section_text <- NULL
    expect_error(clct$parse(), "1 item not found.+is undefined")

    clct$content$nested$section_text <- "Substrate (% Cover)"
    clct$content$nested$content$substrate$section_text <- NULL
    expect_error(clct$parse(), "1 item not found.+is undefined")
})

test_that("`to_list()` produces a nice clean list (simple collection)", {
    clct <- mkm:::Collection$new(name = "test")
    clct$content <- list(
        Item$new(name = "lst", content = list()),
        Item$new(name = "mat", content = matrix()),
        Item$new(name = "df", content = data.frame()))
    out <- clct$to_list()
    expect_type(out, "list")
    expect_named(out, "test")
    expect_length(out, 1)
    expect_named(out[[1]], c("lst", "mat", "df"))
    expect_type(out[[1]]$lst, "list")
    expect_true(is.matrix(out[[1]]$mat))
    expect_s3_class(out[[1]]$df, "data.frame")
})

test_that("Rethink the to_list() [See comments...]", {
    # Have a 'content only' argument for a 'clean' list and a defualt that produces a nested list with properties?
})


test_that("`to_list()` produces a nice clean list (nested collection)", {
    clct <- mkm:::Collection$new(name = "test")
    nested_clct <-  mkm:::Collection$new(name = "nested")
    lst <- Item$new(name = "lst", content = list())
    mat <- Item$new(name = "mat", content = matrix())
    df <- Item$new(name = "df", content = data.frame())
    nested_clct$content <- list(mat, df)
    clct$content <- list(lst, nested_clct)
    out <- clct$to_list()

    expect_type(out, "list")
    expect_named(out, "test")
    expect_length(out, 1)

    expect_named(out$test, c("lst", "nested"))
    expect_type(out$test$lst, "list")
    expect_type(out$test$nested, "list")

    expect_named(out$test$nested, c("mat", "df"))
    expect_true(is.matrix(out$test$nested$mat))
    expect_s3_class(out$test$nested$df, "data.frame")
})

test_that("Collections can target an item to 'promote' to metadata...", {
    clct <- mkm:::Collection$new(name = "test", path = "./test_files/mkm_basic_file.xlsx")
    clct$content <- list(
        Item$new(name = "meta", section_text = "takiwaR Metadata", parser = parse_rlist),
        Item$new(name = "substrate", section_text = "Substrate (% Cover)", parser = parse_tmat))
    expect_silent(clct$read()$parse())
    # Target a content object to 'act' as metadata / top level properties
    expect_silent(clct$define_meta('meta'))
    expect_equal(clct$meta, clct$content$meta$content)

    # Must exist
    expect_error(clct$define_meta('bebop'), 'must reference an item in the collection')

    # Must reference a (uniquely) named list to be eligible
    expect_error(clct$define_meta('substrate'), 'must reference an item containing a named list')

    expect_error(clct$define_meta(c('meta', 'substrate')), 'must be a character vector of length 1')
    expect_error(clct$define_meta(1), 'must be a character vector of length 1')
    expect_error(clct$define_meta(list("name" = 3)), 'must be a character vector of length 1')
})

test_that("Collections can clone themselves (safely)", {
    clct <- mkm:::Collection$new(name = "test")
    nested_clct <-  mkm:::Collection$new(name = "nested")
    lst <- Item$new(name = "lst", content = list())
    mat <- Item$new(name = "mat", content = matrix())
    df <- Item$new(name = "df", content = data.frame())
    nested_clct$content <- list(mat, df)
    clct$content <- list(lst, nested_clct)

    clct2 <- clct$clone()
    expect_equal(clct$name, clct2$name)
    expect_equal(clct$names, clct2$names)
    # This is OK:
    clct$name <- "test_changed"
    expect_false(clct$name == clct2$name)
    # The **contents** remain pass-by-reference (on shallow clones):
    clct$content$lst$name <- "lst_changed"
    expect_equal(clct$names, clct2$names)
    expect_identical(clct$content, clct2$content)
    clct$content$lst_changed$content <- "Pass me by reference"
    expect_equal(clct$content, clct2$content)

    # Deep clones should not have this problem...
    clct3 <- clct$clone(deep = TRUE)
    clct$content$lst_changed$name <- "again"
    expect_equal(length(clct$names), length(clct3$names))
    expect_error(expect_equal(clct$names, clct3$names))
    expect_error(expect_identical(clct$content, clct3$content))

    expect_equal(clct$content[[1]]$content, clct3$content[[1]]$content)
    pre_change <- clct$content[[1]]$content
    clct$content[[1]]$content <- "I am not passed by reference"
    expect_equal(clct3$content[[1]]$content, pre_change)
})

test_that("Collections can template off other collections (safely)", {
    clct1 <- mkm:::Collection$new(name = "test", path = "./test_files/mkm_nested_collection.xlsx",
                                  section_text = "Nested Section", sheet = 1)
    nested_clct <-  mkm:::Collection$new(name = "nested")
    lst <- Item$new(name = "lst", content = list("a" = 1))
    mat <- Item$new(name = "mat", content = matrix())
    df <- Item$new(name = "df", content = data.frame())
    nested_clct$content <- list(mat, df)
    clct1$content <- list(lst, nested_clct)
    clct1$raw <- list(text = matrix(""), format = list(matrix("")))
    clct1$define_meta("lst")

    clct2 <- clct1$template()

    # These items are copied ("deep cloned"):
    # name, section_text
    expect_equal(clct1$name, clct2$name)
    expect_equal(clct1$section_text, clct2$section_text)

    # These items are not copied (should be NULLed):
    # path, sheet, raw, meta
    expect_equal(clct1$path, "./test_files/mkm_nested_collection.xlsx")
    expect_null(clct2$path)

    expect_equal(clct1$sheet, 1)
    expect_null(clct2$sheet)

    expect_type(clct1$raw, 'list')
    expect_null(clct2$raw)

    expect_type(clct1$meta, 'list')
    expect_named(clct1$meta, 'a')
    expect_null(clct2$meta)

    # content is, itself templated...
    expect_type(clct1$content, 'list')
    expect_type(clct1$content$lst$content, 'list')
    expect_named(clct1$content$lst$content, 'a')
    expect_true(is.matrix(clct1$content$nested$content$mat$content))

    expect_type(clct2$content, 'list')
    expect_null(clct2$content$lst$content)
    expect_null(clct2$content$nested$content$mat$content)

    # These templates hsould be deep clones
    pre_change <- clct1$name
    clct1$name <- "changed"
    expect_equal(clct2$name, pre_change)
})

test_that("Collections can parse bits twice", {
    clct <- mkm:::Collection$new(name = "test", path = "./test_files/mkm_basic_file.xlsx")
    clct$content <- list(
        Item$new(name = "meta", section_text = "takiwaR Metadata", parser = parse_rlist),
        Item$new(name = "substrate", section_text = "Substrate (% Cover)", parser = parse_tmat),
        Item$new(name = "substrate2", section_text = "Substrate (% Cover)", parser = parse_tmat))
    expect_null(clct$raw)
    expect_silent(clct$read()$parse())
    expect_equal(clct$content$substrate$content, clct$content$substrate2$content)
})


