test_that("throws error for invalid id_user", {
  expect_error(mnk_user_proj(NULL), "single, non-NA numeric")
  expect_error(mnk_user_proj(NA_real_), "single, non-NA numeric")
  expect_error(mnk_user_proj(c(1, 2)), "single, non-NA numeric")
  expect_error(mnk_user_proj("a string"), "single, non-NA numeric")
})

test_that("handles API HTTP errors", {
  skip_if_not_installed("httr")

  testthat::local_mocked_bindings(
    GET = function(url,...) {
      structure(list(
        url = url,
        status_code = 404L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('{"detail": "Not found."}')
      ), class = "response")
    },
    .package = "httr"
  )

  expect_message(result <- mnk_user_proj(999999), "Status: 404")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles unexpected JSON format", {
  skip_if_not_installed("httr")

  testthat::local_mocked_bindings(
    GET = function(url,...) {
      structure(list(
        url = url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('{"message": "Unexpected format"}')
      ), class = "response")
    },
    .package = "httr"
  )

  expect_message(result <- mnk_user_proj(12345), "not in the expected format")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("returns tibble for valid user id", {
  skip_if_not_installed("httr")

  json_ok <- '{
    "results": [
      {
        "id": 101,
        "title": "Proyecto Alpha",
        "description": "Desc del proyecto Alpha",
        "slug": "alpha-proj",
        "icon": "icon_alpha.png",
        "place_id": 202,
        "created_at": "2023-01-01T12:00:00Z"
      },
      {
        "id": 102,
        "title": "Proyecto Beta",
        "description": null,
        "slug": "beta-proj",
        "icon": null,
        "place_id": 203,
        "created_at": "2023-01-02T12:00:00Z"
      }
    ]
  }'

  testthat::local_mocked_bindings(
    GET = function(url,...) {
      structure(list(
        url = url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(json_ok)
      ), class = "response")
    },
    .package = "httr"
  )

  result <- mnk_user_proj(6)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(c("id","title","description","slug","icon","place_id","created_at") %in% names(result)))
  expect_equal(result$id[1], 101)
  expect_equal(result$title[1], "Proyecto Alpha")
  expect_true(is.na(result$description[2]))
  expect_true(is.na(result$icon[2]))
})
