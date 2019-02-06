context("Items")

test_that("Items are of class 'Proto'", {
    obj <- Item$new(name = "test")
    expect_s3_class(object = obj, class = c("Proto"))
})

test_that("Items are initalised with all required fields, with expected defaults", {
    expect_error(object = Item$new(),
                 regexp = "must be a character vector of length 1")
    obj <- Item$new(name = "test")
    req <- c(names(Proto$new("test")$properties), "content", "validator", "indicator", "parser")
    expect_length(object = obj$properties, n = length(req))
    expect_true(all(req %in% names(obj$properties)))

    expect_null(obj$content)
    expect_equal(obj$validator, validate::validator())
    expect_equal(obj$indicator, validate::indicator())
    expect_type(obj$parser, "closure")
    expect_equal(mode(obj$parser), "function")
    expect_null(obj$parser(1)) # The default function returns NULL
})

test_that("The `content` active binding accepts anything", {
    l <- list(A = 1)
    df <- data.frame(A=1:5)

    obj <- Item$new(name = "test", content = l)
    expect_equal(obj$content, l)
    expect_equal(obj[['content']], l)
    expect_equal(obj$get_prop('content'), l)

    obj$content <- df
    expect_equal(obj$content, df)

    obj$content <- "A"
    expect_equal(obj$content, "A")

    obj$content <- 1
    expect_equal(obj$content, 1)
})

test_that("The `validator` active binding accepts only validator objects", {
    df <- data.frame(A=1:5)
    v <- validate::validator()
    obj <- Item$new(name = "test", validator = v)
    expect_equal(obj$validator, v)
    obj$validator <- v
    expect_equal(obj$validator, v)

    expect_error(obj$validator <- "A")
    expect_error(obj$validator <- 1)
    expect_error(obj$validator <- list())
    expect_error(obj$validator <- data.frame())
})

test_that("The `indicator` active binding accepts only indicator objects", {
    ind <- validate::indicator()
    obj <- Item$new(name = "test", indicator = ind)
    expect_equal(obj$indicator, ind)
    obj$indicator <- ind
    expect_equal(obj$indicator, ind)

    expect_error(obj$indicator <- "A")
    expect_error(obj$indicator <- 1)
    expect_error(obj$indicator <- list())
    expect_error(obj$indicator <- data.frame())
})

test_that("The `parser` active binding accepts functions or characters that match a function", {
    obj <- Item$new(name = "test", parser = parse_df)
    expect_equal(obj$parser, parse_df)
    obj$parser <- "parse_mat"
    expect_equal(obj$parser, parse_mat)

    expect_error(obj$parser <- "A") # Not matched
    expect_error(obj$parser <- 1)
    expect_error(obj$parser <- list())
    expect_error(obj$parser <- data.frame())
})

test_that("Items can match their own section text", {
    obj <- Item$new(name = "test", section_text = "Text")
    haystack <- c("A", "B", "More", "Text", "Test")
    expect_equal(obj$match_section_text(haystack), 4)

    # Haystack is required
    expect_error(obj$match_section_text(), "must be a character vector")
    expect_error(obj$match_section_text(haystack = NULL), "must be a character vector")
    expect_error(obj$match_section_text(haystack = 1:8), "must be a character vector")
})

test_that("match_section_text() returns NA when not found, with the reason as a message attribute", {
    obj <- Item$new(name = "test", section_text = "Text")
    haystack <- c("A", "B", "More", "Text", "Test")

    # Section text must be defined
    obj$section_text <- NULL
    mtch <- obj$match_section_text(haystack)
    expect_equal(as.vector(mtch), NA_integer_)
    expect_match(comment(mtch), "undefined")

    # Must match
    obj$section_text <- "Meow"
    mtch <- obj$match_section_text(haystack)
    expect_equal(as.vector(mtch), NA_integer_)
    expect_match(comment(mtch), "not found")

    # Must match exactly once (else error) NB: This could be picked up by the above!
    haystack[1:2] <- "Meow"
    mtch <- obj$match_section_text(haystack)
    expect_equal(as.vector(mtch), NA_integer_)
    expect_match(comment(mtch), "more than once.+matched 2 times")

    haystack[1:3] <- "Meow"
    mtch <- obj$match_section_text(haystack)
    expect_equal(as.vector(mtch), NA_integer_)
    expect_match(comment(mtch), "more than once.+matched 3 times")
})

test_that("Items can parse content from file", {
    obj <- Item$new(name = "test", section_text = "takiwaR Metadata", path = "./test_files/mkm_single_item.xlsx")
    expect_warning(obj$parse(), "File not read yet")
    expect_warning(obj$parse(), "Parser function returned NULL")
    obj$parser <- parse_rlist
    expect_silent(obj$parse())
    expect_type(obj$content, "list")
    expect_named(obj$content, expected = c("key", "another_key", "testing_123"))
})

test_that("Items can parse content when provided", {
    obj <- Item$new(name = "test", parser = parse_rlist)
    text_dat <- matrix(c(NA, "key", "another_key", "testing 123",
                         NA, "value", "1", "2019-01-09"),
                       ncol = 2)
    expect_error(obj$parse(raw = text_dat))
    legit_dat <- list("text" = text_dat, "format" = NULL)
    expect_silent(obj$parse(raw = legit_dat))
    expect_equal(obj$raw, legit_dat)
    expect_equal(obj$content, parse_rlist(legit_dat$text))
})

test_that("`to_list()` produces a named list containing the content", {
    obj <- Item$new(name = "test", section_text = "takiwaR Metadata",
                    path = "./test_files/mkm_single_item.xlsx", parser = parse_rlist)
    expect_silent(obj$read()$parse())
    out <- obj$to_list()
    expect_type(out, 'list')
    expect_named(out, 'test')
    expect_type(out$test, 'list')
    # TODO: Move these to private properties so they are not exported?
    # expect_false(any(c('validator', 'indicator', 'parser') %in% names(attr(out, 'mkm')$properties)))
})

test_that("Items can clone themselves (safely)", {
    # obj$clone() # Normal R6 behaviour... (with a warning? Nope: Can't override $clone)
    obj1 <- Item$new(name = "test")
    expect_silent(obj2 <- obj1$clone())
    expect_true(obj1$name == obj2$name)
    obj2$name <- "new"
    expect_false(obj1$name == obj2$name)

    # With simple cloning these will have (literally) the same validator
    obj1$validator$rules <- validate::validator(a == 1)$rules
    names(obj1$validator) <- "new_name"
    obj1$validate()

    expect_identical(obj1$validator, obj2$validator)
    expect_identical(names(obj1$validator), names(obj2$validator))
    expect_identical(obj1$validator$rules, obj2$validator$rules)
    expect_equal(obj1$get_validation(), obj2$get_validation())

    # The default behavior of clone(deep=TRUE) is to copy fields which are R6 objects, but not copy fields which are environments, reference class objects, or other data structures which contain other reference-type objects (for example, a list with an R6 object).
    # https://r6.r-lib.org/articles/Introduction.html
    obj3 <- obj1$clone(deep = TRUE)
    obj1$validator$rules <- validate::validator(a == 1, b == 1, c == 2)$rules
    names(obj1$validator) <- "new_name_again" # This gets recycled, whatever...
    obj1$validate()

    expect_error(expect_identical(obj1$validator, obj3$validator), "equal but not identical")
    expect_error(expect_identical(names(obj1$validator), names(obj3$validator)))
    expect_error(expect_identical(obj1$validator$rules, obj3$validator$rules))
    expect_error(expect_equal(obj1$get_validation(), obj3$get_validation()))

    # TODO: `<-` method for MKM items...?
})

test_that("Consider moving essential (problematic) items out of $properties() [See comments...]", {
    # Namely: vlidator, indicator, parser.. Others?
    # Keep: name, path, sheet, section_text

})

test_that("Items can provide a template of themselves (safely)", {
    # Templating is like clone(), except relevant fields are pre-NULLed...
    obj1 <- Item$new(name = "test", validator = validate::validator(a == 1),
                     path = "./test_files/mkm_basic_file.xlsx",
                     sheet = 1,
                     content = list("a" = 1))$validate()
    obj1$raw <- list(text = matrix(""), format = list(matrix("")))
    obj2 <- obj1$template()

    # These items are copied ("deep cloned"):
    # name, section_text, parser, validator, indicator
    expect_equal(obj1$name, obj2$name)
    expect_equal(obj1$section_text, obj2$section_text)
    expect_equal(obj1$parser, obj2$parser)
    expect_identical(obj1$parser, obj2$parser)
    expect_equal(obj1$validator, obj2$validator)
    expect_error(expect_identical(obj1$validator, obj2$validator), "equal but not identical")
    expect_equal(obj1$indicator, obj2$indicator)
    expect_error(expect_identical(obj1$indicator, obj2$indicator), "equal but not identical")

    # These items are not copied (should be NULLed):
    # path, sheet, content, raw, indication, validation
    expect_equal(obj1$path, "./test_files/mkm_basic_file.xlsx")
    expect_null(obj2$path)

    expect_equal(obj1$sheet, 1)
    expect_null(obj2$sheet)

    expect_type(obj1$content, 'list')
    expect_null(obj2$content)

    expect_type(obj1$raw, 'list')
    expect_null(obj2$raw)

    expect_true(inherits(obj1$get_validation(), 'validation'))
    expect_silent(obj1$is_valid())
    expect_true(obj1$is_valid(quiet = TRUE))

    expect_null(obj2$.__enclos_env__$private$.validation)
    expect_message(obj2$is_valid())
    expect_false(obj2$is_valid(quiet = TRUE))

    expect_true(inherits(obj1$get_indication(), 'indication'))
    expect_null(obj2$.__enclos_env__$private$.indication)

    # These templates hsould be deep clones
    pre_change <- obj1$name
    obj1$name <- "changed"
    expect_equal(obj2$name, pre_change)
})
