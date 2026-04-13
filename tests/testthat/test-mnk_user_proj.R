test_that("throws error for invalid id_user", {
  expect_error(mnk_user_proj(NULL), "single, non-NA numeric")
  expect_error(mnk_user_proj(NA_real_), "single, non-NA numeric")
  expect_error(mnk_user_proj(c(1, 2)), "single, non-NA numeric")
  expect_error(mnk_user_proj("a string"), "single, non-NA numeric")
})

test_that("handles API HTTP errors", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(url,...) structure(list(status_code = 404L), class = "response"),
    http_error = function(...) TRUE,
    status_code = function(...) 404L,
    .package = "httr"
  )
  expect_message(result <- mnk_user_proj(999999), "Status: 404")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id","title","description","slug","icon","place_id","created_at"))
})

test_that("handles unexpected JSON format", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(url,...) structure(list(status_code = 200L), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) list(message = "Unexpected format"),
    .package = "httr"
  )
  result <- mnk_user_proj(12345)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id","title","description","slug","icon","place_id","created_at"))
})

test_that("returns tibble for valid user id", {
  skip_if_not_installed("httr")
  mock_content <- list(
    results = list(
      list(id = 101, title = "Proyecto Alpha", description = "Desc del proyecto Alpha",
           slug = "alpha-proj", icon = "icon_alpha.png", place_id = 202,
           created_at = "2023-01-01T12:00:00Z"),
      list(id = 102, title = "Proyecto Beta", description = NULL,
           slug = "beta-proj", icon = NULL, place_id = 203,
           created_at = "2023-01-02T12:00:00Z")
    )
  )
  testthat::local_mocked_bindings(
    GET = function(url,...) structure(list(status_code = 200L), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) mock_content,
    .package = "httr"
  )
  result <- mnk_user_proj(6)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(101, 102))
  expect_equal(result$title[1], "Proyecto Alpha")
  expect_true(is.na(result$description[2]))
  expect_true(is.na(result$icon[2]))
})

test_that("handles network error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) stop("Network failure"),
    .package = "httr"
  )
  expect_message(result <- mnk_user_proj(123), "Network error: Minka API is unavailable")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id","title","description","slug","icon","place_id","created_at"))
})
