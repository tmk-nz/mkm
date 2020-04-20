context("make_key")

test_that("Key generation does not touch valid names", {
    expect_equal(make_keys("valid"), "valid")
    expect_equal(make_keys("valid.name"), "valid.name")
    expect_equal(make_keys("valid_name"), "valid_name")
    expect_equal(make_keys("..2way"), "..2way")

    # "Ugly", but valid
    expect_equal(make_keys("._2way"), "._2way")
    expect_equal(make_keys("._"), "._")
    expect_equal(make_keys(".a"), ".a")
    expect_equal(make_keys("._____"), "._____")
    expect_equal(make_keys(".__.__"), ".__.__")
    expect_equal(make_keys(".__.....__"), ".__.....__")
    expect_equal(make_keys(".__a....__"), ".__a....__")
    expect_equal(make_keys("a______b"), "a______b")
    expect_equal(make_keys("a......b"), "a......b")
})

test_that('Missing values are converted to empty strings (then ".")', {
    # NA => "" => Therefore... Add a dot
    expect_equal(make_keys(NA), ".")
    expect_equal(make_keys(NA_integer_), ".")
    expect_equal(make_keys(NA_real_), ".")
    expect_equal(make_keys(NA_complex_), ".")
    expect_equal(make_keys(NA_character_), ".")
})

test_that("'Special' MKM rules are applied", {
    expect_equal(make_keys("% cover"), "percent_cover")
    expect_equal(make_keys("%_cover"), "percent_cover")
    expect_equal(make_keys("%.cover"), "percent.cover")

    expect_equal(make_keys("# plants"), "number_plants")
    expect_equal(make_keys("#_plants"), "number_plants")
    expect_equal(make_keys("#.plants"), "number.plants")

    expect_equal(make_keys("$ spent"), "dollar_spent")
    expect_equal(make_keys("$_spent"), "dollar_spent")
    expect_equal(make_keys("$.spent"), "dollar.spent")

    expect_equal(make_keys("per m^2"), "per_m_squared")
    expect_equal(make_keys("per_m^2"), "per_m_squared")
    expect_equal(make_keys("per.m^2"), "per.m.squared")

    expect_equal(make_keys("m^-2"), "per_m_squared")
    expect_equal(make_keys("m^-1"), "per_m")

    expect_equal(make_keys(".m^-2"), ".per.m.squared")
    expect_equal(make_keys(".m^-1"), ".per.m")

    expect_equal(make_keys(" m^-2"), ".per_m_squared")
    expect_equal(make_keys(" m^-1"), ".per_m")

    expect_equal(make_keys("some per m^2"), "some_per_m_squared")
    expect_equal(make_keys("some_per_m^2"), "some_per_m_squared")
    expect_equal(make_keys("some.per.m^2"), "some.per.m.squared")

    expect_equal(make_keys("some m^-2"), "some_per_m_squared")
    expect_equal(make_keys("some_m^-2"), "some_per_m_squared")
    expect_equal(make_keys("some.m^-2"), "some.per.m.squared")

    expect_equal(make_keys("some m^-1"), "some_per_m")
    expect_equal(make_keys("some_m^-1"), "some_per_m")
    expect_equal(make_keys("some.m^-1"), "some.per.m")

    expect_equal(make_keys("2 per m^2"), "..2_per_m_squared")
    expect_equal(make_keys("2_per_m^2"), "..2_per_m_squared")
    expect_equal(make_keys("2.per.m^2"), "..2.per.m.squared")

    expect_equal(make_keys("subs % cover"), "subs_percent_cover")
    expect_equal(make_keys("subs (% cover)"), "subs_percent_cover")
})

test_that("Non-syntactic characters at the beginning are 'filled'", {
    # Just "fill" with dots...
    expect_equal(make_keys(" "), ".")
    expect_equal(make_keys("@"), ".")
    expect_equal(make_keys("  lots.of.space"), "..lots.of.space")
    expect_equal(make_keys("  lots_of_space"), "..lots_of_space")
    expect_equal(make_keys(". "), "..")
})

test_that("The correct 'internal' seperator is guessed", {
    expect_equal(make_keys("valid"), "valid")
    # The rule is: Use "." if the string contains "." already and use "_" otherwise
    # Equivalently: Underscores are default, but dot takes precedence if present
    expect_equal(make_keys("foo bar baz"), "foo_bar_baz")
    expect_equal(make_keys("foo_bar baz"), "foo_bar_baz")
    expect_equal(make_keys("foo.bar baz"), "foo.bar.baz")
    expect_equal(make_keys(".foo bar baz"), ".foo.bar.baz")

    expect_equal(make_keys(" foo bar baz"), ".foo_bar_baz")
    expect_equal(make_keys(" foo_bar baz"), ".foo_bar_baz")
    expect_equal(make_keys(" foo.bar baz"), ".foo.bar.baz")

    expect_equal(make_keys(". . "), "....")
    expect_equal(make_keys(" . ."), "....")
    expect_equal(make_keys(". ."), "....")

    expect_equal(make_keys("a b c"), "a_b_c")
    expect_equal(make_keys("a.b c"), "a.b.c")
    expect_equal(make_keys(" a_b_c"), ".a_b_c")
    expect_equal(make_keys(" a_b c"), ".a_b_c")
    expect_equal(make_keys("...a b c"), "...a.b.c")
})

test_that("Multiple 'internal' seperators are replaced with ('collapsed to') a single", {
    expect_equal(make_keys("a_____@_____b"), "a_b")
    expect_equal(make_keys("a.....@.....b"), "a.b")
    expect_equal(make_keys("a     @     b"), "a_b")
    expect_equal(make_keys("a___..@__.__b"), "a.b")
    expect_equal(make_keys("a___  @__ __b"), "a_b")
    expect_equal(make_keys("a   ..@  .  b"), "a.b")
})

test_that("Adds leading dots to non-syntactic names", {
    # Prepend only
    expect_equal(make_keys("_"), "._")
    expect_equal(make_keys("_1"), "._1")
    expect_equal(make_keys("___"), ".___")
    expect_equal(make_keys("____"), ".____")

    expect_equal(make_keys(""), ".") # Prepend a dot.
    expect_equal(make_keys("1"), "...1") # Prepend three dots
    expect_equal(make_keys(".1"), "...1") # Prepend two dots

    expect_equal(make_keys("2way"), "..2way") # Prepend two dots
    expect_equal(make_keys(".2way"), "..2way") # Prepend one dot
    expect_equal(make_keys("..2way"), "..2way") # Do nothing!
    expect_equal(make_keys("_2way"), "._2way") # Prepend one dot only.
})


test_that("Dots are prepended to (easy) reserved words", {
    # My expectations are the same as the tidyverse (vctrs::vec_as_names)
    expect_equal(make_keys("if"), ".if")
    expect_equal(make_keys("else"), ".else")
    expect_equal(make_keys("repeat"), ".repeat")
    expect_equal(make_keys("while"), ".while")
    expect_equal(make_keys("function"), ".function")
    expect_equal(make_keys("for"), ".for")
    expect_equal(make_keys("next"), ".next")
    expect_equal(make_keys("break"), ".break")
    expect_equal(make_keys("TRUE"), ".TRUE")
    expect_equal(make_keys("FALSE"), ".FALSE")
    expect_equal(make_keys("NULL"), ".NULL")
    expect_equal(make_keys("Inf"), ".Inf")
    expect_equal(make_keys("NaN"), ".NaN")
    expect_equal(make_keys("NA"), ".NA")
    expect_equal(make_keys("NA_integer_"), ".NA_integer_")
    expect_equal(make_keys("NA_real_"), ".NA_real_")
    expect_equal(make_keys("NA_complex_"), ".NA_complex_")
    expect_equal(make_keys("NA_character_"), ".NA_character_")
})

test_that("Dots are prepended to (difficult) reserved words", {
    # Namely: "..." and "..j", where j is an integer.

    # These are "difficult" in the tidyverse becuase they can produce values
    # that conflict with the unique (positional) suffix.

    # NB: If these principals are adhered to then the minimal suffix needed is
    # ..j (not ...j) becuase the simplest name is "" which converts to "." +
    # "..j" = "...j", which is syntactic. For now, I'm going to stick with ...j

    # My expectations are different from the tidyverse (@2020-04-16)
    expect_equal(make_keys("..."), "....") # Just add a dot
    expect_equal(make_keys("..1"), "...1") # Just add a dot
    # c.f. Tidyverse @2020-04-16
    # https://principles.tidyverse.org/names-attribute.html
    # "..." and "..j": Both are repaired as if they were "".
    # r = "universal"; q = TRUE
    # vctrs::vec_as_names("...", repair = r, quiet = q) # [1] "...1"
    # vctrs::vec_as_names("..1", repair = r, quiet = q) # [1] "...1"
    # vctrs::vec_as_names("..2", repair = r, quiet = q) # [1] "...1" Seems problematic?
})

test_that("Key generation works for reserved word objects", {
    # Check (easy) raw objects
    # NB: if, else, repeat, while, function, for, next, break, ... and ..j
    # don't pass the parser, so we don't need to test them here!
    expect_equal(make_keys(TRUE), ".TRUE")
    expect_equal(make_keys(FALSE), ".FALSE")
    expect_equal(make_keys(NULL), character(0L)) # Nothing is always nothing.
    expect_equal(make_keys(Inf), ".Inf")
    expect_equal(make_keys(NaN), ".NaN")
})

test_that("Prior character replacement doesn't create non-syntactic names", {
    # Case 1: Fill and prepend
    expect_equal(make_keys("   "), "....") # "   " => "..." => "...."
    expect_equal(make_keys(".  "), "....") # ".  " => "..." => "...."
    expect_equal(make_keys("  2"), "...2") # "  2" => "..2" => "...2"
    expect_equal(make_keys(". 2"), "...2") # ". 2" => "..2" => "...2"

    # Fill = OK
    expect_equal(make_keys("   2"), "...2") # "   2" => "...2"
})

test_that("Correct unique suffixes are added", {
    # Needs unique suffxes:
    expect_equal(make_keys(c("", "")), c("....1", "....2")) # Add a dot and suffix
    expect_equal(make_keys(c("1", "1")), c("...1...1", "...1...2")) # Add three dots and suffix
    expect_equal(make_keys(c(".1", ".1")), c("...1...1", "...1...2")) # Add two dots and suffix
    expect_equal(make_keys(c("_", "_")), c("._...1", "._...2"))
    expect_equal(make_keys(c("_1", "_1")), c("._1...1", "._1...2"))

    # This is where I disagree with vctrs::vec_as_names
    expect_equal(make_keys(c("...", "...")), c(".......1", ".......2"))
    expect_equal(make_keys(c("..1", "..1")), c("...1...1", "...1...2"))
    expect_equal(make_keys(c("..2", "..2")), c("...2...1", "...2...2"))
    # c.f. Tidyverse @2020-04-16
    # r = "universal"; q = TRUE
    # vctrs::vec_as_names(c("...", "..."), repair = r, quiet = q) # "...1" "...2"
    # vctrs::vec_as_names(c("..1", "..1"), repair = r, quiet = q) # "...1" "...2"
    # Problematic:
    # vctrs::vec_as_names(c("..2", "..2"), repair = r, quiet = q) # "...1" "...2"
})

test_that("Key generation works for (difficult) mixed-string problems", {
    # Things really fall over (in the tidyverse) when we have mixed strings
    # https://github.com/r-lib/vctrs/issues/1013
    # As above, agree with the principals, but my expectations are different

    expect_equal(make_keys(c("", "1")), c(".", "...1")) # Add a dot. No suffixing needed
    # Add dots and then suffixes to positions 1 and 5:
    expect_equal(make_keys(c("", "5", "a", "b", "", "1")),
                           c("....1", "...5", "a", "b", "....5", "...1"))
    # Add dots (1 and 3, respectivly), then suffix both.
    expect_equal(make_keys(c("..1", "1")), c("...1...1", "...1...2"))

    # c.f. Tidyverse @2020-04-16
    # r = "universal"; q = TRUE
    # vctrs::vec_as_names(c("", "1"), repair = r, quiet = q)
    # # "...1" "...1" # This is not right, surely?
    # vctrs::vec_as_names(c("", "5", "a", "b", "", "1"), repair = r, quiet = q)
    # # "...1" "...5" "a"    "b"    "...5" "...1"
    # vctrs::vec_as_names(c("..1", "1"), repair = r, quiet = q)
    # # "...1" "...1"
})

test_that("Key generation works as expected on examples from ?vec_as_names", {
    expect_equal(make_keys(character(length = 3L)), c("....1", "....2", "....3"))
    # c.f vec_as_names @ 2020-04-18: c("...1", "...2", "...3")
    expect_equal(make_keys(c("x", NA)), c("x", "."))
    # c.f vec_as_names @ 2020-04-18: c("x", "...2")
    expect_equal(make_keys(c("", "")), c("....1", "....2"))
    # c.f vec_as_names @ 2020-04-18: c("...1", "...2")
    expect_equal(make_keys(c("x", "x")), c("x...1", "x...2"))
    # c.f vec_as_names @ 2020-04-18: c("x...1", "x...2")
    expect_equal(make_keys(c("..2", "...")), c("...2", "...."))
    # c.f vec_as_names @ 2020-04-18: c("...1", "...2")
    expect_equal(make_keys(c("", "x", NA)), c("....1", "x", "....3"))
    # c.f vec_as_names @ 2020-04-18: c("...1", "x", "...3")
    expect_equal(make_keys(c("y", "x", "x")), c("y", "x...2", "x...3"))
    # c.f vec_as_names @ 2020-04-18: c("y", "x...2", "x...3")
    expect_equal(make_keys(c("(y)", "_z")), c(".y", "._z"))
    # c.f vec_as_names @ 2020-04-18: c(".y.", "._z")
    expect_equal(make_keys(c(".2fa", "FALSE")), c("..2fa", ".FALSE"))
    # c.f vec_as_names @ 2020-04-18: c("..2fa", ".FALSE")
})

test_that("Key generation works for it's inital application!", {
    nmes <- c("takiwaR Metadata", "Substrate (% Cover)",
              "Primary Producers (Counts)", "Iris size frequency")
    expt <- c("takiwaR_Metadata", "Substrate_percent_Cover",
              "Primary_Producers_Counts", "Iris_size_frequency")
    expect_equal(make_keys(nmes), expt)
})

test_that("Key generation works for random left-over (perhaps already covered) applications", {
    expect_equal(make_keys(c("valid.name", "valid_name")),
                           c("valid.name", "valid_name"))
    expect_equal(make_keys(c(".", ".1",   "..2",  "...3", "....4")),
                           c(".", "...1", "...2", "...3", "....4"))
    expect_equal(make_keys(c("_",  "_1",  "__2",  "___3",  "____4")),
                           c("._", "._1", ".__2", ".___3", ".____4"))
    expect_equal(make_keys(c(".Dotted", "..TwoDots", "...ThreeDots", "....FourDots")),
                           c(".Dotted", "..TwoDots", "...ThreeDots", "....FourDots"))
    expect_equal(make_keys(c("_Score",  "__TwoScore",  "___ThreeScore",  "____FourScore")),
                           c("._Score", ".__TwoScore", ".___ThreeScore", ".____FourScore"))
    expect_equal(make_keys("_.. @messy_af..."), "._...messy.af")
    # Good exmaple of 'ugly with a purpose':
    expect_equal(make_keys(c("1 Num",       "1_Num",       "_1_Num")),
                           c("..1_Num...1", "..1_Num...2", "._1_Num"))
    # TODO: Might change casing in the future:
    expect_equal(make_keys(c("Mixed case", "Check_Caps")),
                           c("Mixed_case", "Check_Caps"))
})

test_that("`set_keys` works as expected", {
    v <- c(test = "test option", blue = "blue95", "green", `not a name` = "hh")
    expect_equal(names(set_keys(v)), c("test", "blue", "green", "not_a_name"))

    s <- c("takiwaR Metadata", "Substrate (% Cover)",
           "Primary Producers (Counts)", "Iris size frequency")
    e <- c("takiwaR_Metadata", "Substrate_percent_Cover",
           "Primary_Producers_Counts", "Iris_size_frequency")
    expect_equal(names(set_keys(s)), e)
})
