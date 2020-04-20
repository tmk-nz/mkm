context("Test support functions")

test_that("`unique_not_na` works as expected", {
    expect_equal(unique_not_na(c(1,2,2,3)), c(1,2,3))
    expect_equal(unique_not_na(c(1,2,2,3)), c(1,2,3))
    expect_equal(unique_not_na(c(1,NA,NA,2,3)), c(1,2,3))
    expect_equal(unique_not_na(NA), NA)
    expect_equal(unique_not_na(NA_character_), NA_character_)
})

