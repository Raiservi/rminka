test_that("get_wrm_tax handles network errors", {
  skip_if_not_installed("httr")
  mock_httr_GET_network_error <- function(url) {
    stop("Failed to connect to host: www.marinespecies.org")
  }
  with_mocked_bindings(
    GET = mock_httr_GET_network_error,
    .package = "httr",
    {
      expect_message(
        result <- get_wrm_tax("Qualquer nome"),
        regexp = "Network error: WoRMS API is unavailable or unreachable..*Failed to connect"
      )
      expect_null(result)
    }
  )
})

test_that("get_wrm_tax handles API HTTP errors", {
  skip_if_not_installed("httr")
  mock_httr_GET_error <- function(url) {
    structure(list(
      status_code = 404L,
      content = charToRaw("Not Found"),
      url = "https://www.marinespecies.org/rest/mock-error",
      headers = list("Content-Type" = "text/plain")
    ), class = "response")
  }
  with_mocked_bindings(
    GET = mock_httr_GET_error,
    .package = "httr",
    {
      expect_message(
        result <- get_wrm_tax("any name"),
        regexp = "WoRMS API request failed. Status code: 404"
      )
      expect_null(result)
    }
  )
})

test_that("get_wrm_tax handles API returning empty JSON", {
  skip_if_not_installed("httr")
  mock_httr_GET_empty <- function(url) {
    structure(list(
      status_code = 200L,
      content = charToRaw("[]"),
      url = "https://www.marinespecies.org/rest/mock-empty",
      headers = list("Content-Type" = "application/json")
    ), class = "response")
  }
  with_mocked_bindings(
    GET = mock_httr_GET_empty,
    .package = "httr",
    {
      expect_message(
        result <- get_wrm_tax("a name that returns empty"),
        regexp = "No taxon found for the scientific name: 'a name that returns empty'."
      )
      expect_null(result)
    }
  )
})

test_that("get_wrm_tax handles invalid or malformed JSON response", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  mock_httr_GET_success <- function(url) {
    structure(list(
      status_code = 200L,
      content = charToRaw('{"bad json"'),
      url = "https://www.marinespecies.org/rest/mock-malformed",
      headers = list("Content-Type" = "application/json")
    ), class = "response")
  }
  mock_fromJSON_fails <- function(...) {
    return(NULL)
  }
  with_mocked_bindings(.package = "httr", GET = mock_httr_GET_success, {
    with_mocked_bindings(.package = "jsonlite", fromJSON = mock_fromJSON_fails, {
      expect_message(
        result <- get_wrm_tax("a species name"),
        regexp = "API returned an empty or invalid response for: 'a species name'."
      )
      expect_null(result)
    })
  })
})

test_that("get_wrm_tax parses a valid JSON response correctly", {
  skip_if_not_installed("httr")
  valid_json <- '[{
    "AphiaID": 159782, "valid_AphiaID": 159782, "valid_name": "Diplodus sargus",
    "rank": "Species", "kingdom": "Animalia", "phylum": "Chordata",
    "class": "Actinopteri", "order": "Spariformes", "family": "Sparidae",
    "genus": "Diplodus", "isMarine": 1, "isBrackish": 1,
    "isFreshwater": 0, "isTerrestrial": 0, "isExtinct": 0
  }]'
  mock_httr_GET_success <- function(url) {
    structure(list(
      status_code = 200L,
      content = charToRaw(valid_json),
      url = "https://www.marinespecies.org/rest/mock-success",
      headers = list("Content-Type" = "application/json")
    ), class = "response")
  }
  with_mocked_bindings(
    GET = mock_httr_GET_success,
    .package = "httr",
    {
      result <- get_wrm_tax("Diplodus sargus")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_equal(result$valid_name, "Diplodus sargus")
      expect_true(as.logical(result$isMarine))
    }
  )
})

test_that("get_wrm_tax throws error for invalid input", {
  err_msg <- "'scientific_name' must be a single non-empty character string."
  expect_error(get_wrm_tax(NULL), regexp = err_msg, fixed = TRUE)
  expect_error(get_wrm_tax(12345), regexp = err_msg, fixed = TRUE)
  expect_error(get_wrm_tax(c("a", "b")), regexp = err_msg, fixed = TRUE)
  expect_error(get_wrm_tax("    "), regexp = err_msg, fixed = TRUE)
})

test_that("get_wrm_tax works for a valid species name", {
  skip_on_cran()
  skip_if_offline(host = "www.marinespecies.org")
  result <- get_wrm_tax("Diplodus sargus")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$valid_name, "Diplodus sargus")
})

test_that("get_wrm_tax handles JSON that parses to an empty list", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  mock_httr_GET_success <- function(url) {
    structure(list(
      status_code = 200L,
      content = charToRaw('{"some_valid":"json"}'),
      url = "https://www.marinespecies.org/rest/mock-empty-list",
      headers = list("Content-Type" = "application/json")
    ), class = "response")
  }
  mock_fromJSON_empty_list <- function(...) {
    return(list())
  }
  with_mocked_bindings(.package = "httr", GET = mock_httr_GET_success, {
    with_mocked_bindings(.package = "jsonlite", fromJSON = mock_fromJSON_empty_list, {
      expect_message(
        result <- get_wrm_tax("a species name"),
        regexp = "API returned an empty or invalid response for: 'a species name'."
      )
      expect_null(result)
    })
  })
})
