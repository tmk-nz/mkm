context("Validation Sub-system")

test_that("Items can validate", {
    df <- data.frame(A=1:5)
    v <- validate::validator(A < 6, "A" %in% names(.))
    obj <- Item$new(name = "test", validator = v, content = df)
    expect_silent(obj$validate())
    expect_true(obj$is_valid())

    obj$validator <- validate::validator(A < 1, "B" %in% names(.))
    expect_false(obj$validate()$is_valid())
})

test_that("Validations with NAs are not valid, unless specifically allowed", {
    df <- data.frame(A=c(1:5, NA))
    v <- validate::validator(A < 6)
    obj <- Item$new(name = "test", validator = v, content = df)
    expect_false(obj$is_valid())
    expect_message(obj$is_valid(), "contains one or more missing \\(NA\\) values")
    expect_true(obj$is_valid(allow_na = TRUE))
})

# Errors and warnings are different from fails.
# Currently validator::confront returns TRUE from all() if there are errors (or warnings)
test_that("Validations with errors are not valid, unless specifically allowed", {
    df <- data.frame(A=1:5)
    v <- validate::validator(B < 6, C == -1) # No 'B' or 'C' columns == Error(s)
    obj <- Item$new(name = "test", validator = v, content = df)
    expect_false(obj$is_valid())
    expect_message(obj$is_valid(), "has 2 errors")
    expect_true(obj$is_valid(allow_errors = TRUE))
})

test_that("Validations with warnings are not valid, unless specifically allowed", {
    df <- data.frame(A=1:5)
    v <- validate::validator(A %in% matrix(1:5, ncol=2)) # Matrix row fill problem == Warning
    obj <- Item$new(name = "test", validator = v, content = df)
    expect_false(obj$is_valid())
    expect_message(obj$is_valid(), "has 1 warning")
    expect_true(obj$is_valid(allow_warnings = TRUE))
})

test_that("Collections can validate", {
    clct <- mkm:::Collection$new(name = "test")
    obj1 <- Item$new(name = "item1",
                     validator = validate::validator(A < 6, "A" %in% names(.)),
                     content = data.frame(A=1:5))
    obj2 <- Item$new(name = "item2",
                     validator = validate::validator(B >= 6, "B" %in% names(.)),
                     content = data.frame(B=6:10))
    clct$content <- list(obj1, obj2)

    expect_silent(clct$validate())
    expect_silent(clct$is_valid())
    expect_true(clct$is_valid())

    obj1$validator <- validate::validator(B < 6, C == -1)
    expect_false(clct$is_valid())
    expect_message(clct$is_valid(), "has 2 errors")

    obj1$validator <- validate::validator(A %in% matrix(1:5, ncol=2))
    expect_false(clct$is_valid())
    expect_message(clct$is_valid(), "has 1 warning")
})

test_that("Collections and Items have the same API", {
    clct <- mkm:::Collection$new(name = "test")
    obj <- Item$new(name = "item1")
    expect_equal(formals(clct$is_valid), formals(obj$is_valid))
})
