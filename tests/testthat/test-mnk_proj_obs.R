test_that("calls mnk_obs with correct basic parameters", {
  skip_if_not_installed("mockery")

  m <- mockery::mock()
  mockery::stub(mnk_proj_obs, "mnk_obs", m)

  mnk_proj_obs(project_id = 123, year = 2024)

  mockery::expect_called(m, 1)
  mockery::expect_args(
    m, 1,
    project_id = 123,
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
  mockery::stub(mnk_proj_obs, "mnk_obs", m)

  mnk_proj_obs(
    project_id = 999,
    year = 2025,
    month = 8,
    day = 15,
    quiet = TRUE,
    limit_download = FALSE
  )

  mockery::expect_called(m, 1)
  mockery::expect_args(
    m, 1,
    project_id = 999,
    year = 2025,
    month = 8,
    day = 15,
    quiet = TRUE,
    limit_download = FALSE
  )
})

test_that("requires project_id and year", {
  expect_error(mnk_proj_obs(year = 2024), "project_id.*missing")
  expect_error(mnk_proj_obs(project_id = 123), "year.*missing")
})
