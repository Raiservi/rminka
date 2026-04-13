test_that("handles invalid input", {
  expect_error(mnk_proj_info(), "must provide either")
})

test_that("handles network error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) stop("fail"),
    .package = "httr"
  )
  expect_message(result <- mnk_proj_info(project_id = 123), "Network error: Minka API is unavailable")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles HTTP error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(status_code = 404L), class = "response"),
    http_error = function(...) TRUE,
    .package = "httr"
  )
  result <- mnk_proj_info(project_id = "not_found")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles empty string response", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) "",
    .package = "httr"
  )
  result <- mnk_proj_info(project_id = "empty")
  expect_equal(nrow(result), 0)
})

test_that("handles null json response", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) "null",
    .package = "httr"
  )
  result <- mnk_proj_info(project_id = "null")
  expect_equal(nrow(result), 0)
})

test_that("handles no results response", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) '{"results":[]}',
    .package = "httr"
  )
  result <- mnk_proj_info(project_id = "no_results")
  expect_equal(nrow(result), 0)
})

test_that("returns project info from fourth result", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":420,"title":"Test Project","created_at":"2023-01-01T12:00:00Z","place_id":101,"slug":"test-project","description":"A test description.","user_ids":[10,20,30]}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json,
    .package = "httr"
  )
  result <- mnk_proj_info(project_id = 420)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, 420)
  expect_equal(result$title, "Test Project")
  expect_equal(result$subscrib_users, 3)
})

test_that("handles missing fields", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":777,"title":"Missing"}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json,
    .package = "httr"
  )
  result <- mnk_proj_info(project_id = 777)
  expect_equal(result$id, 777)
  expect_true(is.na(result$created_at))
  expect_true(is.na(result$place_id))
  expect_equal(result$subscrib_users, 0)
})

test_that("works with grpid argument", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":888,"title":"Project By Group ID","created_at":"2023-01-01T12:00:00Z","place_id":102,"slug":"project-by-group","description":"Found by grpid.","user_ids":[1,2,3]}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json,
    .package = "httr"
  )
  result <- mnk_proj_info(grpid = "test-group")
  expect_equal(result$id, 888)
  expect_equal(result$title, "Project By Group ID")
  expect_equal(result$subscrib_users, 3)
})
