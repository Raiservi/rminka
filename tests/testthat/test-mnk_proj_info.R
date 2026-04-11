test_that("handles invalid input", {
  expect_error(mnk_proj_info(project_id = NULL, grpid = NULL), "must provide either")
  expect_error(mnk_proj_info(project_id = c("1", "2")), "project_id.*single")
  expect_error(mnk_proj_info(grpid = c("group1", "group2")), "grpid.*single")
})

test_that("handles network and HTTP errors", {
  skip_if_not_installed("httr")

  testthat::local_mocked_bindings(
    GET = function(...) stop("Network failure"),
    .package = "httr"
  )
  expect_message(result_net <- mnk_proj_info(project_id = 123), "Network error")
  expect_null(result_net)

  testthat::local_mocked_bindings(
    GET = function(...) structure(
      list(status_code = 404L,
           headers = list(`content-type` = "application/json"),
           content = charToRaw('{"error":"not found"}')),
      class = "response"),
    .package = "httr"
  )
  expect_message(result_http <- mnk_proj_info(project_id = "not_found"), "Status code: 404")
  expect_null(result_http)
})

test_that("handles empty, null, or no-result responses", {
  skip_if_not_installed("httr")

  mock_GET <- function(url, query, ...) {
    content_str <- switch(query$id,
                          "empty_string" = "",
                          "null_json" = "null",
                          "no_results" = '{"results":[]}',
                          '{"other_field":1}'
    )
    structure(
      list(status_code = 200L,
           headers = list(`content-type` = "application/json"),
           content = charToRaw(content_str)),
      class = "response")
  }
  testthat::local_mocked_bindings(GET = mock_GET, .package = "httr")

  expect_message(mnk_proj_info(project_id = "empty_string"), "empty or null")
  expect_message(mnk_proj_info(project_id = "null_json"), "empty or null")
  expect_message(mnk_proj_info(project_id = "no_results"), "No project details found")
  expect_message(mnk_proj_info(project_id = "other"), "No project details found")
})

test_that("with users=FALSE returns project info list", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":420,"title":"Test Project","description":"A test description.","slug":"test-project","created_at":"2023-01-01T12:00:00Z","place_id":101,"user_ids":[10,20,30]}]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(
      list(status_code = 200L,
           headers = list(`content-type` = "application/json"),
           content = charToRaw(json)),
      class = "response"),
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 420, users = FALSE)
  expect_type(result, "list")
  expect_named(result, c("id","title","created_at","subscrib_users","place_id","slug","description"))
  expect_equal(result$id, 420)
})

test_that("with users=TRUE returns tibble of user IDs", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"user_ids":[10,20,30]}]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(
      list(status_code = 200L,
           headers = list(`content-type` = "application/json"),
           content = charToRaw(json)),
      class = "response"),
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 420, users = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_named(result, "id_users")
  expect_equal(nrow(result), 3)
})

test_that("with users=TRUE handles no users", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":421,"user_ids":[]}]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(
      list(status_code = 200L,
           headers = list(`content-type` = "application/json"),
           content = charToRaw(json)),
      class = "response"),
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 421, users = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles missing fields when users=FALSE", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":777,"title":"Missing"}]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(
      list(status_code = 200L,
           headers = list(`content-type` = "application/json"),
           content = charToRaw(json)),
      class = "response"),
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 777)
  expect_true(is.na(result$created_at))
  expect_true(is.na(result$place_id))
  expect_equal(result$subscrib_users, 0)
})

test_that("works with grpid argument", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":888,"title":"Project By Group ID","created_at":"2023-01-01T12:00:00Z","user_ids":[1,2,3],"place_id":102,"slug":"project-by-group","description":"This project was found by grpid."}]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(
      list(status_code = 200L,
           headers = list(`content-type` = "application/json"),
           content = charToRaw(json)),
      class = "response"),
    .package = "httr"
  )

  result <- mnk_proj_info(grpid = "test-group")
  expect_type(result, "list")
  expect_equal(result$id, 888)
  expect_equal(result$title, "Project By Group ID")
  expect_equal(result$subscrib_users, 3)
})
