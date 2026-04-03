
test_that("mnk_proj_obs calls mnk_obs with the correct basic parameters", {
  m <- mockery::mock()
  mockery::stub(where = mnk_proj_obs, what = "mnk_obs", how = m)

  mnk_proj_obs(project_id = 123, year = 2024)

  mockery::expect_called(m, 1)

  mockery::expect_args(m, 1,
                       project_id = 123,
                       year = 2024,
                       month = NULL,
                       day = NULL,
                       quiet = FALSE,
                       limit_download = TRUE
  )
})

test_that("mnk_proj_obs passes all optional parameters correctly", {
  m <- mockery::mock()
  mockery::stub(where = mnk_proj_obs, what = "mnk_obs", how = m)

  mnk_proj_obs(
    project_id = 999,
    year = 2025,
    month = 8,
    day = 15,
    quiet = TRUE,
    limit_download = FALSE
  )

  mockery::expect_called(m, 1)

  mockery::expect_args(m, 1,
                       project_id = 999,
                       year = 2025,
                       month = 8,
                       day = 15,
                       quiet = TRUE,
                       limit_download = FALSE
  )
})

test_that("mnk_proj_obs requires project_id and year", {
  expect_error(
    mnk_proj_obs(year = 2024),
    "argument \"project_id\" is missing, with no default"
  )

  expect_error(
    mnk_proj_obs(project_id = 123),
    "argument \"year\" is missing, with no default"
  )
})
