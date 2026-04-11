test_that("converts data frame to sf POINT with selected columns", {
  skip_if_not_installed("sf")

  df <- data.frame(
    id = 1:2,
    latitude = c(40.9, 41.0),
    longitude = c(0.8, 0.9),
    observed_on = c("2024-05-01", "2024-05-02"),
    taxon_name = c("sp1", "sp2"),
    extra = c("x", "y")
  )

  out <- mnk_obs_sf(df, id, taxon_name)

  expect_s3_class(out, "sf")
  expect_true(all(sf::st_is(out, "POINT")))
  expect_equal(nrow(out), 2)
  expect_true(all(c("id", "taxon_name", "latitude", "longitude", "observed_on", "geometry") %in% names(out)))
  expect_false("extra" %in% names(out))
})

test_that("keeps latitude and longitude when keep_coords is TRUE", {
  skip_if_not_installed("sf")
  df <- data.frame(id = 1, latitude = 41, longitude = 2)
  out <- mnk_obs_sf(df, id, keep_coords = TRUE)
  expect_true("latitude" %in% names(out))
  expect_true("longitude" %in% names(out))
})

test_that("removes latitude and longitude when keep_coords is FALSE", {
  skip_if_not_installed("sf")
  df <- data.frame(id = 1, latitude = 41, longitude = 2)
  out <- mnk_obs_sf(df, id, keep_coords = FALSE)
  expect_false("latitude" %in% names(out))
  expect_false("longitude" %in% names(out))
  expect_s3_class(out, "sf")
})

test_that("coerces observed_on to Date", {
  skip_if_not_installed("sf")
  df <- data.frame(id = 1, latitude = 41, longitude = 2, observed_on = "2024-01-15")
  out <- mnk_obs_sf(df, id)
  expect_s3_class(out$observed_on, "Date")
  expect_equal(out$observed_on, as.Date("2024-01-15"))
})

test_that("filters rows with missing coordinates", {
  skip_if_not_installed("sf")
  df <- data.frame(
    id = 1:4,
    latitude = c(41, NA, 42, 43),
    longitude = c(2, 3, NA, 4)
  )
  out <- mnk_obs_sf(df, id)
  expect_equal(nrow(out), 2)
  expect_equal(out$id, c(1, 4))
})

test_that("removes duplicate rows", {
  skip_if_not_installed("sf")
  df <- data.frame(
    id = c(1, 1, 2),
    latitude = c(41, 41, 42),
    longitude = c(2, 2, 3)
  )
  out <- mnk_obs_sf(df, id)
  expect_equal(nrow(out), 2)
})

test_that("errors when data is not a data.frame", {
  expect_error(mnk_obs_sf(list(a = 1)), "`data` must be a data.frame or tibble")
})

test_that("errors when latitude or longitude are missing", {
  df1 <- data.frame(id = 1, lat = 1, longitude = 2)
  df2 <- data.frame(id = 1, latitude = 1, lon = 2)
  expect_error(mnk_obs_sf(df1, id), "must contain `latitude` and `longitude`")
  expect_error(mnk_obs_sf(df2, id), "must contain `latitude` and `longitude`")
})

test_that("accepts tidyselect helpers", {
  skip_if_not_installed("sf")
  skip_if_not_installed("tidyselect")
  df <- data.frame(
    id = 1,
    latitude = 41,
    longitude = 2,
    taxon_name = "a",
    taxon_rank = "species",
    other = "x"
  )
  out <- mnk_obs_sf(df, tidyselect::starts_with("taxon_"))
  expect_true(all(c("taxon_name", "taxon_rank") %in% names(out)))
  expect_false("other" %in% names(out))
})

test_that("respects custom crs", {
  skip_if_not_installed("sf")
  df <- data.frame(id = 1, latitude = 41, longitude = 2)
  out <- mnk_obs_sf(df, id, crs = 3857)
  expect_equal(sf::st_crs(out)$epsg, 3857)
})

test_that("works when observed_on is absent", {
  skip_if_not_installed("sf")
  df <- data.frame(id = 1, latitude = 41, longitude = 2)
  out <- mnk_obs_sf(df, id)
  expect_false("observed_on" %in% names(out))
  expect_equal(nrow(out), 1)
})

test_that("works with tibble input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("tibble")
  df <- tibble::tibble(id = 1:2, latitude = c(40, 41), longitude = c(0, 1))
  out <- mnk_obs_sf(df, id)
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)
})
