

skip_if_not_installed("httptest")

library(testthat)



test_that("mnk_user_proj throws error for invalid id_user", {
  expect_error(mnk_user_proj(NULL), "You must provide a single, non-NA numeric 'id_user'.")
  expect_error(mnk_user_proj(NA_real_), "You must provide a single, non-NA numeric 'id_user'.")
  expect_error(mnk_user_proj(c(1, 2)), "You must provide a single, non-NA numeric 'id_user'.")
  expect_error(mnk_user_proj("a string"), "You must provide a single, non-NA numeric 'id_user'.")
})


test_that("mnk_user_proj handles API HTTP errors", {

  mock_httr_GET <- function(url) {

    if (grepl("/users/999999/", url)) {
      response_obj <- list(
        url = url,
        status_code = 404L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('{"detail": "Not found."}')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    }
    stop("Mock no configurado para esta URL: ", url)
  }


  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {

      expect_message(result <- mnk_user_proj(999999), regexp = "Minka API request failed. Status: 404")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
    }
  )
})


test_that("mnk_user_proj handles unexpected JSON format", {

  mock_response_json <- '{"message": "Unexpected format"}'

  mock_httr_GET <- function(url) {
    if (grepl("/users/12345/", url)) {
      response_obj <- list(
        url = url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(mock_response_json)
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    }
    stop("Mock no configurado para esta URL: ", url)
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {

      expect_message(result <- mnk_user_proj(12345), regexp = "API response was not in the expected format")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
    }
  )
})


test_that("mnk_user_proj returns a tibble for a valid user ID", {

  mock_response_json <- '{
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

  mock_httr_GET <- function(url) {

    if (grepl("/users/6/", url)) {
      response_obj <- list(
        url = url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(mock_response_json)
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    }
    stop("Mock no configurado para esta URL: ", url)
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      result <- mnk_user_proj(6)


      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 2)


      expected_cols <- c("id", "title", "description", "slug", "icon", "place_id", "created_at")
      expect_true(all(expected_cols %in% names(result)))


      expect_equal(result$id[1], 101)
      expect_equal(result$title[1], "Proyecto Alpha")


      expect_true(is.na(result$description[2]))
      expect_true(is.na(result$icon[2]))
    }
  )
})
