
library(testthat)

test_that("Function produces correct abbreviations for valid names", {

  expect_equal(shrt_name("Diplodus sargus"), "dip.sar")

  expect_equal(shrt_name("Diplodus sargus sargus"), "dip.sar.sar")

  expect_equal(shrt_name("Diplodus"), "dip")

  expect_equal(shrt_name("DIPLODUS SarGUS"), "dip.sar")

  expect_equal(shrt_name("  Diplodus sargus  "), "dip.sar")
})

test_that("Function handles vectors of names correctly", {

  input_vector <- c("Diplodus sargus", "Diplodus cervinus",
                    "Diplodus sargus sargus", "Diplodus")
  expected_output <- c("dip.sar", "dip.cer", "dip.sar.sar","dip")

  expect_equal(shrt_name(input_vector), expected_output)
})

test_that("Function stops with an error for invalid input types", {

  expect_error(
    shrt_name(123),
    "Input cannot be a number. Please provide a character string."
  )

  expect_error(
    shrt_name(NULL),
    "Input must be a non-empty character string or vector."
  )

  expect_error(
    shrt_name(TRUE),
    "Input must be a non-empty character string or vector."
  )
})

test_that("Function stops with an error for empty, NA, or blank inputs", {

  expect_error(
    shrt_name(NA),
    "Input cannot contain NA or empty strings."
  )

  expect_error(
    shrt_name(""),
    "Input cannot contain NA or empty strings."
  )

  expect_error(
    shrt_name("   "),
    "Input cannot contain NA or empty strings."
  )

  expect_error(
    shrt_name(c("Homo sapiens", NA)),
    "Input cannot contain NA or empty strings."
  )
})

test_that("Function stops with an error for incorrect number of words", {

  expect_error(
    shrt_name("one two trhee four"),
    "Each scientific name must contain between 1 and 3 words."
  )

  expect_error(
    shrt_name(c("Diplodus sargus", "one two trhee four")),
    "Each scientific name must contain between 1 and 3 words."
  )
})
