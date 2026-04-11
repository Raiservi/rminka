test_that("calls mnk_obs with correct basic parameters", {
  skip_if_not_installed("mockery")

  m <- mockery::mock()
  mockery::stub(mnk_user_obs, "mnk_obs", m)

  mnk_user_obs(user_id = 123, year = 2024)

  mockery::expect_called(m, 1)
  mockery::expect_args(
    m, 1,
    user_id = 123,
    year = 2024,
    month = NULL,
    day = NULL,
    quiet = FALSE,
    limit_download = TRUE
  )
})

test_that("passes all optional parameters correctly", {
  skip_if_not_installed("mockery")

  m <- mockery::mock()
  mockery::stub(mnk_user_obs, "mnk_obs", m)

  mnk_user_obs(
    user_id = "test_user",
    year = 2025,
    month = 8,
    day = 15,
    quiet = TRUE,
    limit_download = FALSE
  )

  mockery::expect_called(m, 1)
  mockery::expect_args(
    m, 1,
    user_id = "test_user",
    year = 2025,
    month = 8,
    day = 15,
    quiet = TRUE,
    limit_download = FALSE
  )
})

test_that("requires user_id and year", {
  expect_error(mnk_user_obs(year = 2024), "user_id.*missing")
  expect_error(mnk_user_obs(user_id = 123), "year.*missing")
})
