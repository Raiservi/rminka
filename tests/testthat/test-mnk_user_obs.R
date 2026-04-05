library(testthat)
library(mockery)

test_that("mnk_user_obs calls mnk_obs with the correct basic parameters", {
  m <- mockery::mock()
  mockery::stub(where = mnk_user_obs, what = "mnk_obs", how = m)

  mnk_user_obs(user_id = 123, year = 2024)

  mockery::expect_called(m, 1)

  mockery::expect_args(m, 1,
                       user_id = 123,
                       year = 2024,
                       month = NULL,
                       day = NULL,
                       quiet = FALSE,
                       limit_download = TRUE
  )
})

test_that("mnk_user_obs passes all optional parameters correctly", {
  m <- mockery::mock()
  mockery::stub(where = mnk_user_obs, what = "mnk_obs", how = m)

  mnk_user_obs(
    user_id = "test_user",
    year = 2025,
    month = 8,
    day = 15,
    quiet = TRUE,
    limit_download = FALSE
  )

  mockery::expect_called(m, 1)

  mockery::expect_args(m, 1,
                       user_id = "test_user",
                       year = 2025,
                       month = 8,
                       day = 15,
                       quiet = TRUE,
                       limit_download = FALSE
  )
})

test_that("mnk_user_obs requires user_id and year", {
  expect_error(
    mnk_user_obs(year = 2024),
    "argument \"user_id\" is missing, with no default"
  )

  expect_error(
    mnk_user_obs(user_id = 123),
    "argument \"year\" is missing, with no default"
  )
})
