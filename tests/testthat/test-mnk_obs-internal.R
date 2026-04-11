test_that("failed pings return empty tibble and message", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(GET = function(...) structure(list(status_code = 500), class = "response"),
                                  http_error = function(...) TRUE, status_code = function(...) 500L,.package = "httr")
  expect_message(res1 <- rminka:::download_paginated_data(list(q="x"), quiet = FALSE), "PING query failed with code: 500")
  expect_equal(res1$count, 0)
  expect_message(res2 <- rminka:::download_month_data(list(), 2024, 5, quiet = FALSE, remaining_limit = 100), "PING query failed for the month of")
  expect_equal(res2$count, 0)
})

test_that("skips page with error and trims excess", {
  skip_if_not_installed("httr")
  call <- 0
  testthat::local_mocked_bindings(GET = function(...) { call <<- call + 1; structure(list(), class="response") },
                                  http_error = function(...) call == 2,
                                  content = function(...) list(total_results = 400, results = replicate(200, list(id = 1), simplify = FALSE)),.package = "httr")
  expect_message(res_err <- rminka:::download_paginated_data(list(), total_res = 400, quiet = FALSE, numeric_limit = 350), "Error on page 2 - skipping")
  expect_equal(res_err$count, 200)
  testthat::local_mocked_bindings(GET = function(...) structure(list(), class="response"), http_error = function(...) FALSE,
                                  content = function(...) list(total_results = 500, results = replicate(200, list(id = 1), simplify = FALSE)),.package = "httr")
  res_cut <- rminka:::download_paginated_data(list(), total_res = 500, quiet = TRUE, numeric_limit = 150)
  expect_equal(res_cut$count, 150)
})

test_that("download_month_data shows messages with quiet=FALSE", {
  skip_if_not_installed("httr")
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(GET = function(...) structure(list(), class="response"), http_error = function(...) FALSE,
                                  content = function(...) list(total_results = 0),.package = "httr")
  expect_message(rminka:::download_month_data(list(), 2024, 3, quiet = FALSE, remaining_limit = 100), "No records were found for March 2024")
  testthat::local_mocked_bindings(content = function(...) list(total_results = 15000),.package = "httr")
  testthat::local_mocked_bindings(download_paginated_data = function(...) list(data = tibble::tibble(id = 1), count = 1),.package = "rminka")
  msgs <- testthat::capture_messages(rminka:::download_month_data(list(), 2024, 4, quiet = FALSE, remaining_limit = 5))
  expect_true(any(grepl("Total > 10,000. Subdividing by DAY", msgs)))
  expect_true(any(grepl("Downloading day: 1", msgs)))
})

test_that("exhausted limits stop loops and show messages", {
  skip_if_not_installed("httr")
  skip_if_not_installed("tibble")
  res1 <- rminka:::download_month_data(list(), 2024, 1, quiet = TRUE, remaining_limit = 0)
  expect_equal(res1$count, 0)
  testthat::local_mocked_bindings(GET = function(...) structure(list(), class="response"), http_error = function(...) FALSE,
                                  content = function(...) list(total_results = 15000),.package = "httr")
  testthat::local_mocked_bindings(download_paginated_data = function(..., numeric_limit) { n <- min(3, numeric_limit); list(data = tibble::tibble(id = seq_len(n)), count = n) },.package = "rminka")
  res2 <- rminka:::download_month_data(list(), 2024, 5, quiet = TRUE, remaining_limit = 3)
  expect_equal(res2$count, 3)
  testthat::local_mocked_bindings(download_month_data = function(...) list(data = tibble::tibble(id = 1:6000), count = 6000),.package = "rminka")
  msgs <- testthat::capture_messages(mnk_obs(year = 2024, quiet = FALSE))
  expect_true(any(grepl("STARTING ANNUAL DOWNLOAD", msgs)))
  expect_true(any(grepl("Download limit reached", msgs)))
})

test_that("mnk_obs trims if an internal function ignores the limit", {
  skip_if_not_installed("tibble")
  testthat::local_mocked_bindings(download_paginated_data = function(...) list(data = tibble::tibble(id = 1:15000), count = 15000),.package = "rminka")
  res <- mnk_obs(query = "x", quiet = TRUE, limit_download = TRUE)
  expect_equal(nrow(res), 10000)
})
