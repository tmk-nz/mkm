context("make_key")

test_that("Key generation behaves as expected for single objects", {
    expect_equal(make_key("a_nice_key"), "a_nice_key")
    expect_equal(make_key("a_number_1"), "a_number_1")
    expect_equal(make_key("   check_white_space    "), "check_white_space")
    expect_equal(make_key("Check_Caps"), "Check_Caps")
    expect_equal(make_key("Check Spaces"), "Check_Spaces")
    expect_equal(make_key("A Rubbi$h Key"), "A_Rubbi_h_Key")
    # Check start stripping / prepending
    expect_equal(make_key(".Dotted"), "xDotted")
    expect_equal(make_key(".....MultiDotted"), "xMultiDotted")
    expect_equal(make_key("_UScore"), "xUScore")
    expect_equal(make_key("_____MultiUScore"), "xMultiUScore")
    expect_equal(make_key("%SingleIllegal"), "xSingleIllegal")
    expect_equal(make_key("$ %^ &*(ManyIllegal"), "xManyIllegal")
    expect_equal(make_key("1 Num"), "x1_Num")
    expect_equal(make_key("1_Num"), "x1_Num")
    expect_equal(make_key("_1_Num"), "x1_Num")
})

test_that("Key generation behaves as expected for multiple objects", {
    expect_equal(make_key(c("Test Unique", "Test Unique")), c("Test_Unique", "Test_Unique_1"))
    expect_equal(make_key(c("Test Unique", "Test Unique"), make_unique = FALSE), c("Test_Unique", "Test_Unique"))
})

test_that("Key generation behaves as expected for reserved words", {
    expect_equal(make_key("TRUE"), "xTRUE")
    expect_equal(make_key("NA"), "xNA")
    expect_equal(make_key("if"), "xif")
    expect_equal(make_key("NULL"), "xNULL")
    expect_equal(make_key("Inf"), "xInf")

})

test_that("Key generation works for objects defined by reserved words (the ones the pass the parser anyway!)", {
    expect_equal(make_key(TRUE), "xTRUE")
    expect_equal(make_key(FALSE), "xFALSE")
    expect_equal(make_key(Inf), "xInf")
    expect_equal(make_key(NaN), "xNaN")
    expect_equal(make_key(NA), "xNA")
    expect_equal(make_key(c(NA, NA_character_, NA_complex_, NA_integer_, NA_real_)),
                 c("xNA", "xNA_1", "xNA_2", "xNA_3", "xNA_4"))
})

test_that("Key generation works for numeric objects", {
    expect_equal(make_key(1), "x1")
})

