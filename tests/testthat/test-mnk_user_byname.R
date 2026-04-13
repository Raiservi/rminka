mnk_user_byname <- function(query) {
  if (missing(query) || is.null(query) ||!is.character(query) ||
      length(query)!= 1 || is.na(query)) {
    stop("'query' must be a single, non-NA character string.", call. = FALSE)
  }
  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/users/autocomplete"
  response <- httr::GET(base_url, path = api_path, query = list(q = query))
  if (httr::http_error(response)) {
    message("Minka API request failed. Status: ", httr::status_code(response))
    return(tibble::tibble())
  }
  content <- httr::content(response, as = "parsed")
  if (is.list(content) &&!is.null(content$results)) {
    if (length(content$results) == 0) {
      return(tibble::tibble(
        id = integer(),
        login = character(),
        name = character(),
        observations_count = integer(),
        created_at = as.POSIXct(character())
      ))
    }
    purrr::map_dfr(content$results, function(x) tibble::tibble(
      id = rlang::`%||%`(x$id, NA_integer_),
      login = rlang::`%||%`(x$login, NA_character_),
      name = rlang::`%||%`(x$name, NA_character_),
      observations_count = rlang::`%||%`(x$observations_count, NA_integer_),
      created_at = lubridate::ymd_hms(
        rlang::`%||%`(x$created_at, NA_character_), quiet = TRUE
      )
    ))
  } else {
    message("API response was not in the expected format (missing a 'results' list).")
    return(tibble::tibble())
  }
}

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

test_that("validates query is a single non-NA character string", {
  expect_error(mnk_user_byname(), "must be a single")
  expect_error(mnk_user_byname(NULL), "must be a single")
  expect_error(mnk_user_byname(123), "must be a single")
  expect_error(mnk_user_byname(c("a", "b")), "must be a single")
  expect_error(mnk_user_byname(NA_character_), "must be a single")
})

test_that("returns empty tibble when results is empty", {
  skip_if_not_installed("httr")
  mock_resp <- list(results = list())
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code = 200L), class = "response"),
    content = function(x,...) mock_resp,
    .package = "httr"
  )
  result <- mnk_user_byname("nada")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "login", "name", "observations_count", "created_at"))
})

test_that("handles missing or malformed created_at", {
  skip_if_not_installed("httr")
  mock_resp <- list(
    results = list(
      list(id = 1, login = "u1", name = "A", observations_count = 1, created_at = NULL),
      list(id = 2, login = "u2", name = "B", observations_count = 2, created_at = "no-es-fecha")
    )
  )
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code = 200L), class = "response"),
    content = function(x,...) mock_resp,
    .package = "httr"
  )
  result <- mnk_user_byname("fechas")
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$created_at)))
})

test_that("handles content that is not a list", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code = 200L), class = "response"),
    content = function(x,...) "string inesperado",
    .package = "httr"
  )
  expect_message(
    result <- mnk_user_byname("mal"),
    "not in the expected format"
  )
  expect_equal(nrow(result), 0)
})
