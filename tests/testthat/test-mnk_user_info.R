test_that("returns correct list for existing user", {
  skip_if_not_installed("httr")
  json <- '{"total_results":1,"results":[{"id":6,"login":"ramonservitje","spam":false,"suspended":false,"created_at":"2022-04-16T15:47:14+00:00","observations_count":1259,"identifications_count":70,"activity_count":1329,"species_count":336,"icon_url":"/attachments/users/icons/6-medium.jpg?1658326226"}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=200L, content=charToRaw(json)), class="response"),
    .package = "httr"
  )
  res <- mnk_user_info(6)
  expect_type(res, "list")
  expect_equal(res$id, 6)
  expect_equal(res$login, "ramonservitje")
  expect_equal(res$observations_count, 1259)
  expect_equal(res$identifications_count, 70)
  expect_equal(res$species_count, 336)
  expect_equal(res$activity_count, 1329)
  expect_equal(res$spam, FALSE)
  expect_equal(res$suspended, FALSE)
})

test_that("returns NULL invisibly when user not found", {
  skip_if_not_installed("httr")
  json <- '{"total_results":0,"results":[]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=200L, content=charToRaw(json)), class="response"),
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(999), "No user details found")
  expect_null(res)
})

test_that("returns NULL invisibly on HTTP error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=404L, content=charToRaw('{}')), class="response"),
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "Status")
  expect_null(res)
})

test_that("returns NULL invisibly on network error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) stop("timeout"),
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "Network error")
  expect_null(res)
})

test_that("validates input and errors on missing id", {
  expect_error(mnk_user_info(), "must be provided")
})

test_that("validates input and errors on vector id", {
  expect_error(mnk_user_info(c(1,2)), "must be a single")
})

test_that("accepts character id", {
  skip_if_not_installed("httr")
  json <- '{"total_results":1,"results":[{"id":6,"login":"ramonservitje"}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=200L, content=charToRaw(json)), class="response"),
    .package = "httr"
  )
  res <- mnk_user_info("6")
  expect_equal(res$id, 6)
})

test_that("errors on NULL id_user", {
  expect_error(mnk_user_info(NULL), "must be provided")
})

test_that("errors on non-atomic id_user", {
  expect_error(mnk_user_info(list(6)), "must be a single")
})

test_that("returns NULL when response is empty string", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=200L, content=charToRaw("")), class="response"),
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "empty or null")
  expect_null(res)
})

test_that("returns NULL when response is literal null", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=200L, content=charToRaw("null")), class="response"),
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "empty or null")
  expect_null(res)
})

test_that("returns NULL when results field is missing", {
  skip_if_not_installed("httr")
  json <- '{"total_results":1}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=200L, content=charToRaw(json)), class="response"),
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "No user details found")
  expect_null(res)
})

test_that("applies default values for missing fields", {
  skip_if_not_installed("httr")
  json <- '{"total_results":1,"results":[{"id":10,"login":"testuser"}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code=200L, content=charToRaw(json)), class="response"),
    .package = "httr"
  )
  res <- mnk_user_info(10)
  expect_equal(res$id, 10)
  expect_equal(res$login, "testuser")
  expect_true(is.na(res$name))
  expect_true(is.na(res$orcid))
  expect_true(is.na(res$observations_count))
  expect_equal(res$roles, list())
})

test_that("encodes special characters in user id", {
  skip_if_not_installed("httr")
  called_path <- NULL
  testthat::local_mocked_bindings(
    GET = function(url, path, ...) {
      called_path <<- path
      structure(list(status_code=200L, content=charToRaw('{"results":[{"id":1}]}')), class="response")
    },
    .package = "httr"
  )
  mnk_user_info("a b")
  expect_match(called_path, "a%20b")
})
