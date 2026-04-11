test_that("mnk_obs_byday handles small date ranges in one go", {
  skip_if_not_installed("httptest")
  httptest::with_mock_api({
    obs <- mnk_obs_byday(d1 = "2024-05-20", d2 = "2024-05-20", quiet = TRUE)
    expect_s3_class(obs, "tbl_df")
    expect_equal(nrow(obs), 1)
    expect_equal(obs$id, 101)
  })
})

test_that("mnk_obs_byday subdivides requests when total results are large", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if (!is.null(p$d1) && !is.null(p$d2)) return(15000L)
      if (!is.null(p$day)) return(1L)
      0L
    },
    byday_download_chunk = function(params, total_res, quiet, limit_download) {
      tibble::tibble(
        id = 200L + params$day,
        observed_on = sprintf("2024-04-%02d", params$day)
      )
    },
    .package = "rminka"
  )
  obs <- mnk_obs_byday(d1 = "2024-04-01", d2 = "2024-04-02", quiet = TRUE)
  expect_s3_class(obs, "tbl_df")
  expect_equal(nrow(obs), 2)
  expect_equal(obs$id, c(201, 202))
})

test_that("mnk_obs_byday downloads a full month as a single chunk", {
  skip_if_not_installed("httptest")
  httptest::with_mock_api({
    obs <- mnk_obs_byday(d1 = "2024-03-01", d2 = "2024-04-30", quiet = TRUE)
    expect_s3_class(obs, "tbl_df")
    expect_equal(nrow(obs), 2)
    expect_equal(obs$id, c(301, 302))
  })
})

test_that("mnk_obs_byday handles partial months day by day", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if (!is.null(p$d1)) return(12000L)
      if (!is.null(p$day)) return(2L)
      0L
    },
    byday_download_chunk = function(params, ...) {
      tibble::tibble(id = params$day * 10 + 1:2)
    },
    .package = "rminka"
  )
  obs <- mnk_obs_byday("2024-05-10", "2024-05-12", quiet = TRUE)
  expect_equal(nrow(obs), 6)
  expect_equal(obs$id, c(101,102,111,112,121,122))
})

test_that("mnk_obs_byday converts numeric bounds to API params", {
  skip_if_not_installed("tibble")
  captured <- list()
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      captured <<- p
      return(1L)
    },
    byday_download_chunk = function(...) tibble::tibble(id=1),
    .package = "rminka"
  )
  mnk_obs_byday("2024-01-01","2024-01-01", bounds = c(42.2, 2.2, 38.2, 0.6), quiet = TRUE)
  expect_equal(captured$nelat, 42.2)
  expect_equal(captured$nelng, 2.2)
  expect_equal(captured$swlat, 38.2)
  expect_equal(captured$swlng, 0.6)
  expect_null(captured$bounds)
})

test_that("mnk_obs_byday converts annotation vector", {
  skip_if_not_installed("tibble")
  captured <- list()
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) { captured <<- p; 1L },
    byday_download_chunk = function(...) tibble::tibble(id=1),
    .package = "rminka"
  )
  mnk_obs_byday("2024-01-01","2024-01-01", annotation = c(12, 34), quiet = TRUE)
  expect_equal(captured$term_id, 12)
  expect_equal(captured$term_value_id, 34)
})

test_that("mnk_obs_byday removes duplicate ids across days", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) if(!is.null(p$d1)) 20000L else 1L,
    byday_download_chunk = function(params, ...) {
      tibble::tibble(id = 999L)
    },
    .package = "rminka"
  )
  obs <- mnk_obs_byday("2024-06-01","2024-06-02", quiet = TRUE)
  expect_equal(nrow(obs), 1)
  expect_equal(obs$id, 999)
})

test_that("byday_get_total_results returns 0 on http_error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class="response"),
    http_error = function(...) TRUE,
    .package = "httr"
  )
  expect_equal(rminka:::byday_get_total_results(list()), 0)
})

test_that("byday_download_chunk skips page on error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class="response"),
    http_error = function(...) TRUE,
    content = function(...) stop("should not be called"),
    .package = "httr"
  )
  out <- rminka:::byday_download_chunk(list(), total_res = 200, quiet = TRUE, limit_download = TRUE)
  expect_equal(nrow(out), 0)
})

test_that("byday_process_results returns empty tibble", {
  expect_equal(nrow(rminka:::byday_process_results(list())), 0)
})

test_that("byday_download_chunk stops when API returns empty results", {
  skip_if_not_installed("httr")
  calls <- 0
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class="response"),
    http_error = function(...) FALSE,
    content = function(...) { calls <<- calls + 1; list(results = list()) },
    .package = "httr"
  )
  out <- rminka:::byday_download_chunk(list(), 400, TRUE, TRUE)
  expect_equal(calls, 1)
  expect_equal(nrow(out), 0)
})

test_that("mnk_obs_byday validates dates", {
  expect_error(mnk_obs_byday("2024-13-01","2024-01-02"), "must be in 'yyyy-mm-dd'")
  expect_error(mnk_obs_byday("2024-02-01","2024-01-01"), "cannot be after")
})

test_that("mnk_obs_byday validates bounds and annotation", {
  expect_error(mnk_obs_byday("2024-01-01","2024-01-01", bounds = 1:3), "must be a numeric vector of length 4")
  expect_error(mnk_obs_byday("2024-01-01","2024-01-01", annotation = 1), "must be a numeric vector of length 2")
})

test_that("mnk_obs_byday shows messages with quiet=FALSE", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) 0L,
    .package = "rminka"
  )
  expect_message(mnk_obs_byday("2024-01-01","2024-01-01", quiet=FALSE), "No records found")
})

test_that("mnk_obs_byday prints subdivision progress", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) if(!is.null(p$d1)) 15000L else 1L,
    byday_download_chunk = function(...) tibble::tibble(id=1),
    .package = "rminka"
  )
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "Total > 10,000")
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "Processing month")
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "has 1 records")
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "Overall process complete")
})

test_that("mnk_obs_byday downloads full month <=10000", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if(!is.null(p$d1)) return(15000L)
      if(!is.null(p$month) && is.null(p$day)) return(5000L)
      0L
    },
    byday_download_chunk = function(...) tibble::tibble(id=301),
    .package = "rminka"
  )
  obs <- mnk_obs_byday("2024-03-01","2024-03-31", quiet=TRUE)
  expect_equal(obs$id, 301)
})

test_that("mnk_obs_byday downloads full month >10000 day by day", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if(!is.null(p$d1)) return(20000L)
      if(!is.null(p$month) && is.null(p$day)) return(15000L)
      if(!is.null(p$day)) return(1L)
      0L
    },
    byday_download_chunk = function(params, ...) tibble::tibble(id = params$day),
    .package = "rminka"
  )
  obs <- mnk_obs_byday("2024-03-01","2024-03-03", quiet=TRUE)
  expect_equal(nrow(obs), 3)
})

test_that("mnk_obs_byday converts sf bounds", {
  skip_if_not_installed("sf")
  poly_sfc <- sf::st_as_sfc("POLYGON((0 38, 2 38, 2 42, 0 42, 0 38))", crs = 4326)
  poly_sf <- sf::st_sf(geometry = poly_sfc)
  cap <- NULL
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p){ cap <<- p; 0L },
    .package = "rminka"
  )
  mnk_obs_byday("2024-01-01", "2024-01-01", bounds = poly_sf, quiet = TRUE)
  expect_equal(cap$swlng, 0)
  expect_equal(cap$swlat, 38)
  expect_equal(cap$nelng, 2)
  expect_equal(cap$nelat, 42)
})

test_that("mnk_obs_byday shows messages for small full month", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if (!is.null(p$d1)) return(15000L)
      if (!is.null(p$month) && is.null(p$day)) return(5000L)
      0L
    },
    byday_download_chunk = function(...) tibble::tibble(id = 1),
    .package = "rminka"
  )
  expect_message(mnk_obs_byday("2024-03-01", "2024-03-31", quiet = FALSE), "Month has 5,000")
  expect_message(mnk_obs_byday("2024-03-01", "2024-03-31", quiet = FALSE), "Downloading month in one go")
})

test_that("mnk_obs_byday shows messages for large full month", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if (!is.null(p$d1)) return(20000L)
      if (!is.null(p$month) && is.null(p$day)) return(15000L)
      if (!is.null(p$day)) return(1L)
      0L
    },
    byday_download_chunk = function(params, ...) tibble::tibble(id = params$day),
    .package = "rminka"
  )
  msgs <- testthat::capture_messages(mnk_obs_byday("2024-03-01", "2024-03-31", quiet = FALSE))
  expect_true(any(grepl("Month > 10,000", msgs)))
  expect_true(any(grepl("Day: 1 has 1 records", msgs)))
  expect_true(any(grepl("Day: 3 has 1 records", msgs)))
})
