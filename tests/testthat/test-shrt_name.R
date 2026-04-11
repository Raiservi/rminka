test_that("produces correct abbreviations for valid names", {
  expect_equal(shrt_name("Diplodus sargus"), "dip.sar")
  expect_equal(shrt_name("Diplodus sargus sargus"), "dip.sar.sar")
  expect_equal(shrt_name("Diplodus"), "dip")
  expect_equal(shrt_name("DIPLODUS SarGUS"), "dip.sar")
  expect_equal(shrt_name("  Diplodus sargus  "), "dip.sar")
})

test_that("handles vectors of names correctly", {
  input_vector <- c("Diplodus sargus", "Diplodus cervinus",
                    "Diplodus sargus sargus", "Diplodus")
  expected_output <- c("dip.sar", "dip.cer", "dip.sar.sar", "dip")
  expect_equal(shrt_name(input_vector), expected_output)
})

test_that("stops with error for invalid input types", {
  expect_error(shrt_name(123), "cannot be a number")
  expect_error(shrt_name(NULL), "must be a non-empty character")
  expect_error(shrt_name(TRUE), "must be a non-empty character")
})

test_that("stops with error for empty, NA, or blank inputs", {
  expect_error(shrt_name(NA), "cannot contain NA")
  expect_error(shrt_name(""), "cannot contain NA")
  expect_error(shrt_name("   "), "cannot contain NA")
  expect_error(shrt_name(c("Homo sapiens", NA)), "cannot contain NA")
})

test_that("stops with error for incorrect number of words", {
  expect_error(shrt_name("one two three four"), "between 1 and 3 words")
  expect_error(shrt_name(c("Diplodus sargus", "one two three four")), "between 1 and 3 words")
})
