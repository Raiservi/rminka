

skip_if_not_installed("mockery")
library(testthat)
library(httr)
library(jsonlite)
library(tibble)


test_that("mnk_proj_info handles invalid input", {
  expect_error(mnk_proj_info(project_id = NULL, grpid = NULL), "You must provide either 'project_id' or 'grpid'")
  expect_error(mnk_proj_info(project_id = c("1", "2")), "'project_id' must be a single character string or number.")
  expect_error(mnk_proj_info(grpid = c("group1", "group2")), "'grpid' must be a single character string or number.")
})


test_that("mnk_proj_info handles network and HTTP errors", {

  local_mocked_bindings(
    GET = function(...) stop("Network failure"),
    .package = "httr"
  )
  expect_message(
    result_net <- mnk_proj_info(project_id = 123),
    "Network error: Minka API is unavailable."
  )
  expect_null(result_net)


  mock_GET_http_error <- function(url, ...) {
    structure(
      list(
        url = url,
        status_code = 404L,
        headers = list("Content-Type" = "application/json"),
        content = charToRaw('{"error": "not found"}')
      ),
      class = c("response", "handle")
    )
  }
  local_mocked_bindings(GET = mock_GET_http_error, .package = "httr")

  expect_message(
    result_http <- mnk_proj_info(project_id = "not_found"),
    "Minka API request failed. Status code: 404"
  )
  expect_null(result_http)
})

test_that("mnk_proj_info handles empty, null, or no-result API responses", {
  mock_GET_various_empty <- function(url, query, ...) {
    content_str <- switch(query$id,
                          "empty_string" = "",
                          "null_json" = "null",
                          "no_results" = '{"results": []}',
                          "other" = '{"other_field": 1}' # <-- Error de sintaxis corregido
    )
    structure(
      list(
        url = url,
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        content = charToRaw(content_str)
      ),
      class = c("response", "handle")
    )
  }
  local_mocked_bindings(GET = mock_GET_various_empty, .package = "httr")

  expect_message(mnk_proj_info(project_id = "empty_string"), "API returned an empty or null response")
  expect_message(mnk_proj_info(project_id = "null_json"), "API returned an empty or null response")
  expect_message(mnk_proj_info(project_id = "no_results"), "No project details found")
  expect_message(mnk_proj_info(project_id = "other"), "No project details found")
})


test_that("mnk_proj_info with users=FALSE returns project info list", {
  mock_json <- '{ "results": [{ "id": 420, "title": "Test Project", "description": "A test description.", "slug": "test-project", "created_at": "2023-01-01T12:00:00Z", "place_id": 101, "user_ids": [10, 20, 30] }] }'

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(url = url, status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(mock_json)),
        class = c("response", "handle")
      )
    },
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 420, users = FALSE)

  expect_type(result, "list")
  expect_named(result, c("id", "title", "created_at", "subscrib_users", "place_id", "slug", "description"))
  expect_equal(result$id, 420)
})

test_that("mnk_proj_info with users=TRUE returns a tibble of user IDs", {
  mock_json <- '{ "results": [{ "user_ids": [10, 20, 30] }] }'

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(url = url, status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(mock_json)),
        class = c("response", "handle")
      )
    },
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 420, users = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "id_users")
  expect_equal(nrow(result), 3)
})

test_that("mnk_proj_info with users=TRUE handles no users", {
  mock_json <- '{ "results": [ { "id": 421, "user_ids": [] } ] }'

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(url = url, status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(mock_json)),
        class = c("response", "handle")
      )
    },
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 421, users = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "id_users")
  expect_equal(nrow(result), 0)
})


test_that("mnk_proj_info handles missing fields correctly when users=FALSE", {
  mock_json <- '{ "results": [ { "id": 777, "title": "Missing" } ] }'

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(url = url, status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(mock_json)),
        class = c("response", "handle")
      )
    },
    .package = "httr"
  )

  result <- mnk_proj_info(project_id = 777)

  expect_true(is.na(result$created_at))
  expect_true(is.na(result$place_id))
  expect_equal(result$subscrib_users, 0)
})


test_that("mnk_proj_info works correctly with the 'grpid' argument", {
  mock_json_response <- '{
    "results": [{
      "id": 888,
      "title": "Project By Group ID",
      "created_at": "2023-01-01T12:00:00Z",
      "user_ids": [1, 2, 3],
      "place_id": 102,
      "slug": "project-by-group",
      "description": "This project was found by grpid."
    }]
  }'

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(
          url = url,
          status_code = 200L,
          headers = list("Content-Type" = "application/json"),
          content = charToRaw(mock_json_response)
        ),
        class = c("response", "handle")
      )
    },
    .package = "httr"
  )

  result <- mnk_proj_info(grpid = "test-group")

  expect_type(result, "list")
  expect_named(result, c("id", "title", "created_at", "subscrib_users", "place_id", "slug", "description"))
  expect_equal(result$id, 888)
  expect_equal(result$title, "Project By Group ID")
  expect_equal(result$subscrib_users, 3)
})
