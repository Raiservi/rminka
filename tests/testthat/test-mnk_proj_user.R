test_that("mnk_proj_user validates input", {
  expect_error(mnk_proj_user(), "must provide 'project_id'")
  expect_error(mnk_proj_user(NULL), "must provide 'project_id'")
  expect_error(mnk_proj_user(c(1, 2)), "single character string or number")
  expect_error(mnk_proj_user(list(1)), "single character string or number")
})

test_that("handles network error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) stop("timeout"),
    .package = "httr"
  )
  expect_message(res <- mnk_proj_user(123), "Network error")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 16)
})

test_that("handles HTTP error", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) TRUE,
    status_code = function(...) 404L,
    .package = "httr"
  )
  # tu función imprime "Status: 404"
  expect_message(res <- mnk_proj_user(456), "Status:")
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 16)
})

test_that("handles empty response", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) "",
    .package = "httr"
  )
  # tu versión no emite mensaje aquí
  res <- mnk_proj_user(1)
  expect_equal(nrow(res), 0)
})

test_that("handles null json response", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) "null",
    .package = "httr"
  )
  res <- mnk_proj_user(2)
  expect_equal(nrow(res), 0)
})

test_that("handles no results", {
  skip_if_not_installed("httr")
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) '{"results":[]}',
    .package = "httr"
  )
  # tu versión tampoco emite mensaje aquí
  res <- mnk_proj_user(999)
  expect_equal(nrow(res), 0)
})

test_that("handles project with no user_ids", {
  skip_if_not_installed("httr")
  json <- '{"results":[{"id":420}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json,
    .package = "httr"
  )
  res <- mnk_proj_user(420)
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 16)
})

test_that("returns user metadata for participants", {
  skip_if_not_installed("httr")
  json_proj <- '{"results":[{"id":420,"user_ids":[4,6,11]}]}'

  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json_proj,
    .package = "httr"
  )

  testthat::local_mocked_bindings(
    mnk_user_info = function(id) {
      tibble::tibble(
        id = as.integer(id),
        login = paste0("user", id),
        name = NA_character_,
        created_at = as.POSIXct(NA),
        observations_count = NA_integer_,
        identifications_count = NA_integer_,
        species_count = NA_integer_,
        activity_count = NA_integer_,
        journal_posts_count = NA_integer_,
        orcid = NA_character_,
        icon_url = NA_character_,
        site_id = NA_integer_,
        roles = list(NULL),
        spam = NA,
        suspended = NA,
        universal_search_rank = NA_integer_
      )
    },
    .package = "rminka"
  )

  res <- mnk_proj_user(420)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_equal(res$id, c(4L, 6L, 11L))
  expect_equal(res$login, c("user4", "user6", "user11"))
  expect_equal(ncol(res), 16)
})

test_that("accepts character project_id", {
  skip_if_not_installed("httr")
  json_proj <- '{"results":[{"id":1,"user_ids":[99]}]}'
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    http_error = function(...) FALSE,
    content = function(...) json_proj,
    .package = "httr"
  )
  testthat::local_mocked_bindings(
    mnk_user_info = function(id) tibble::tibble(id = 99L, login = "x",
                                                name = NA_character_, created_at = as.POSIXct(NA),
                                                observations_count = NA_integer_, identifications_count = NA_integer_,
                                                species_count = NA_integer_, activity_count = NA_integer_,
                                                journal_posts_count = NA_integer_, orcid = NA_character_,
                                                icon_url = NA_character_, site_id = NA_integer_, roles = list(NULL),
                                                spam = NA, suspended = NA, universal_search_rank = NA_integer_),
    .package = "rminka"
  )
  res <- mnk_proj_user("1")
  expect_equal(nrow(res), 1)
  expect_equal(res$id, 99L)
})
