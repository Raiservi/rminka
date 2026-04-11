test_that("returns correct list for existing user", {
  json_success <- '{
    "total_results": 1,
    "page": 1,
    "per_page": 1,
    "results": [{
      "id": 6,
      "login": "ramonservitje",
      "spam": false,
      "suspended": false,
      "created_at": "2022-04-16T15:47:14+00:00",
      "login_autocomplete": "ramonservitje",
      "login_exact": "ramonservitje",
      "name": "",
      "name_autocomplete": "",
      "orcid": null,
      "icon": "/attachments/users/icons/6-thumb.jpg?1658326226",
      "observations_count": 1259,
      "identifications_count": 70,
      "journal_posts_count": 0,
      "activity_count": 1329,
      "species_count": 336,
      "universal_search_rank": 1259,
      "roles": [],
      "site_id": 1,
      "icon_url": "/attachments/users/icons/6-medium.jpg?1658326226"
    }]
  }'

  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    status_code = function(...) 200L,
    content = function(...) json_success,
    .package = "httr"
  )

  res <- mnk_user_info(6)

  expect_type(res, "list")
  expect_equal(res$id, 6)
  expect_equal(res$login, "ramonservitje")
  expect_equal(res$observations_count, 1259L)
  expect_equal(res$identifications_count, 70L)
  expect_equal(res$species_count, 336L)
  expect_equal(res$activity_count, 1329L)
  expect_equal(res$spam, FALSE)
  expect_equal(res$suspended, FALSE)
  expect_equal(res$icon_url, "/attachments/users/icons/6-medium.jpg?1658326226")
})

test_that("returns NULL invisibly when user not found", {
  json_empty <- '{"total_results":0,"page":1,"per_page":1,"results":[]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json_empty,
    .package = "httr"
  )

  expect_message(res <- mnk_user_info(999), "No user details found")
  expect_null(res)
})

test_that("returns NULL invisibly on HTTP error", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) TRUE,
    status_code = function(...) 404L,
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "Status code: 404")
  expect_null(res)
})

test_that("returns NULL invisibly on network error", {
  testthat::local_mocked_bindings(
    GET = function(...) stop("timeout"),
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "Network error")
  expect_null(res)
})

test_that("validates input and errors on missing id", {
  expect_error(mnk_user_info(), "'id_user' must be provided")
})

test_that("validates input and errors on vector id", {
  expect_error(mnk_user_info(c(1,2)), "must be a single")
})

test_that("accepts character id", {
  json_success <- '{"total_results":1,"page":1,"per_page":1,"results":[{"id":6,"login":"ramonservitje","spam":false,"suspended":false,"created_at":"2022-04-16T15:47:14+00:00","observations_count":1259,"identifications_count":70,"journal_posts_count":0,"activity_count":1329,"species_count":336,"universal_search_rank":1259,"roles":[],"site_id":1,"icon_url":"/attachments/users/icons/6-medium.jpg?1658326226"}]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json_success,
    .package = "httr"
  )
  res <- mnk_user_info("6")
  expect_equal(res$id, 6)
})

# --- NUEVOS TESTS PARA 100% COBERTURA ---

test_that("errors on NULL id_user", {
  expect_error(mnk_user_info(NULL), "'id_user' must be provided")
})

test_that("errors on non-atomic id_user", {
  expect_error(mnk_user_info(list(6)), "must be a single")
})

test_that("returns NULL invisibly when response content is empty string", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) "",
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "empty or null response")
  expect_null(res)
})

test_that("returns NULL invisibly when response content is literal null", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) "null",
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "empty or null response")
  expect_null(res)
})

test_that("returns NULL invisibly when results field is missing", {
  json_no_results <- '{"total_results":1,"page":1}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json_no_results,
    .package = "httr"
  )
  expect_message(res <- mnk_user_info(6), "No user details found")
  expect_null(res)
})

test_that("applies default values for missing fields", {
  json_minimal <- '{"total_results":1,"results":[{"id":10,"login":"testuser"}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json_minimal,
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
  testthat::local_mocked_bindings(
    GET = function(url, path, ...) {
      expect_equal(path, "v1/users/a%20b")
      structure(list(), class = "response")
    },
    http_error = function(...) FALSE,
    content = function(...) '{"results":[{"id":1}]}',
    .package = "httr"
  )
  mnk_user_info("a b")
})
