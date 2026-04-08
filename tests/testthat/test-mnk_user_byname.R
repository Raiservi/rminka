
library(testthat)

test_that("mnk_user_byname correctly processes a mocked API response", {

  mock_content_success <- list(
    total_results = 2, page = 1, per_page = 2,
    results = list(
      list(id = 123, login = "testuser1", name = "Test User One", observations_count = 50, created_at = "2023-01-01T12:00:00Z"),
      list(id = 456, login = "testuser2", name = NULL, observations_count = 100, created_at = "2023-01-02T13:00:00Z")
    )
  )

  mock_GET_generic_success <- function(url, path, query) {
    return(structure(list(status_code = 200L), class = c("response", "handle")))
  }

  local_mocked_bindings(
    GET = mock_GET_generic_success,
    content = function(x, as) mock_content_success,
    .package = "httr")
  {
    result <- mnk_user_byname("test")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 2)
    expect_equal(result$id, c(123, 456))
    expect_true(is.na(result$name[2]))
  }
})

test_that("mnk_user_byname correctly handles an API error", {

  mock_GET_error <- function(url, path, query) {
    return(structure(list(status_code = 500L), class = c("response", "handle")))
  }

  local_mocked_bindings(GET = mock_GET_error,.package = "httr")
  {
    expect_message(result <- mnk_user_byname("error"), "Minka API request failed. Status: 500")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})

test_that("mnk_user_byname handles API response without 'results' field", {

  mock_content_no_results <- list(message = "This is not the data you are looking for")

  mock_GET_generic_success <- function(url, path, query) {
    return(structure(list(status_code = 200L), class = c("response", "handle")))
  }

  local_mocked_bindings(
    GET = mock_GET_generic_success,
    content = function(x, as) mock_content_no_results,
    .package = "httr")
  {
    expect_message(
      result <- mnk_user_byname("no_results"),
      "API response was not in the expected format \\(missing a 'results' list\\)."
    )
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})

