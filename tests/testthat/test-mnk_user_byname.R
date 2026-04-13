
test_that("processes mocked API response", {
  skip_if_not_installed("httr")

  mock_resp <- list(
    total_results = 2, page = 1, per_page = 2,
    results = list(
      list(id = 123, login = "testuser1", name = "Test User One",
           observations_count = 50, created_at = "2023-01-01T12:00:00Z"),
      list(id = 456, login = "testuser2", name = NULL,
           observations_count = 100, created_at = "2023-01-02T13:00:00Z")
    )
  )

  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code = 200L), class = "response"),
    content = function(x,...) mock_resp,
    .package = "httr"
  )

  result <- mnk_user_byname("test")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(123, 456))
  expect_true(is.na(result$name[2]))
})

test_that("handles API error", {
  skip_if_not_installed("httr")

  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code = 500L), class = "response"),
    .package = "httr"
  )

  expect_message(result <- mnk_user_byname("error"), "Status: 500")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles response without results field", {
  skip_if_not_installed("httr")

  mock_resp <- list(message = "This is not the data you are looking for")

  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code = 200L), class = "response"),
    content = function(x,...) mock_resp,
    .package = "httr"
  )

  expect_message(
    result <- mnk_user_byname("no_results"),
    "not in the expected format"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("processes mocked API response", {
  skip_if_not_installed("httr")
  mock_resp <- list(
    total_results = 2, page = 1, per_page = 2,
    results = list(
      list(id = 123, login = "testuser1", name = "Test User One",
           observations_count = 50, created_at = "2023-01-01T12:00:00Z"),
      list(id = 456, login = "testuser2", name = NULL,
           observations_count = 100, created_at = "2023-01-02T13:00:00Z")
    )
  )
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) mock_resp,
    .package = "httr"
  )
  result <- mnk_user_byname("test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(123, 456))
  expect_true(is.na(result$name[2]))
})

test_that("handles API error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) TRUE,
    status_code = function(...) 500L,
    .package = "httr"
  )
  expect_message(result <- mnk_user_byname("error"), "Status: 500")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles response without results field", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) list(message = "no results"),
    .package = "httr"
  )
  expect_message(result <- mnk_user_byname("no_results"), "not in the expected format")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("validates query is missing", {
  expect_error(mnk_user_byname(), "must be a single")
})

test_that("validates query is null", {
  expect_error(mnk_user_byname(NULL), "must be a single")
})

test_that("validates query is not character", {
  expect_error(mnk_user_byname(123), "must be a single")
})

test_that("validates query length is not one", {
  expect_error(mnk_user_byname(c("a", "b")), "must be a single")
})

test_that("validates query is NA", {
  expect_error(mnk_user_byname(NA_character_), "must be a single")
})

test_that("returns empty tibble when results is empty", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) list(results = list()),
    .package = "httr"
  )
  result <- mnk_user_byname("empty")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles missing or malformed created_at", {
  skip_if_not_installed("httr")
  mock_resp <- list(
    results = list(
      list(id = 1, login = "u1", name = "A", observations_count = 1, created_at = NULL),
      list(id = 2, login = "u2", name = "B", observations_count = 2, created_at = "bad-date")
    )
  )
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) mock_resp,
    .package = "httr"
  )
  result <- mnk_user_byname("dates")
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$created_at)))
})

test_that("handles content that is not a list", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) "string",
    .package = "httr"
  )
  expect_message(result <- mnk_user_byname("bad"), "not in the expected format")
  expect_equal(nrow(result), 0)
})
