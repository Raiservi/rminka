test_that("writes single sf layer to geopackage", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1:2, x = c(0.8, 0.9), y = c(40.9, 41.0)),
                      coords = c("x", "y"), crs = 4326)
  tmp <- tempfile(fileext = ".gpkg")
  res <- export_mnk_qgis(points = pts, file = tmp)
  expect_true(file.exists(tmp))
  expect_type(res, "character")
  out <- sf::st_read(tmp, layer = "points", quiet = TRUE)
  expect_equal(nrow(out), 2)
  unlink(tmp)
})

test_that("adds gpkg extension automatically", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0), coords = c("x","y"), crs = 4326)
  tmp <- tempfile()
  on.exit(unlink(paste0(tmp, ".gpkg")), add = TRUE)
  export_mnk_qgis(pts = pts, file = tmp)
  expect_true(file.exists(paste0(tmp, ".gpkg")))
})

test_that("writes multiple named layers", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0), coords = c("x","y"), crs = 4326)
  poly <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    ),
    crs = 4326
  )
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  export_mnk_qgis(exampl_points = pts, exampl_area = poly, file = tmp)
  layers <- sf::st_layers(tmp)$name
  expect_setequal(layers, c("exampl_points", "exampl_area"))
})

test_that("transforms to target crs", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1, x = 2, y = 41), coords = c("x","y"), crs = 4326)
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  export_mnk_qgis(pts = pts, file = tmp, crs = 3857)
  out <- sf::st_read(tmp, quiet = TRUE)
  expect_equal(sf::st_crs(out)$epsg, 3857)
})

test_that("drops Z and M dimensions", {
  skip_if_not_installed("sf")
  pt_z <- sf::st_sf(id = 1, geom = sf::st_sfc(sf::st_point(c(0,0,10)), crs = 4326))
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  export_mnk_qgis(zpts = pt_z, file = tmp)
  out <- sf::st_read(tmp, quiet = TRUE)
  expect_false(any(grepl("Z", class(out$geom[[1]]))))
})

test_that("repairs invalid geometries", {
  skip_if_not_installed("sf")
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  bad <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,0), c(2,2), c(2,0), c(0,2), c(0,0))))
    , crs = 3857)
  bad_sf <- sf::st_sf(id = 1, geom = bad)
  expect_false(sf::st_is_valid(bad_sf))
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  export_mnk_qgis(bad = bad_sf, file = tmp, crs = 3857)
  out <- sf::st_read(tmp, quiet = TRUE)
  expect_true(sf::st_is_valid(out))
})

test_that("removes non-geometry list columns", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0), coords = c("x","y"), crs = 4326)
  pts$photos <- list(c("a.jpg","b.jpg"))
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  export_mnk_qgis(pts = pts, file = tmp)
  out <- sf::st_read(tmp, quiet = TRUE)
  expect_false("photos" %in% names(out))
})

test_that("errors when no layers provided", {
  expect_error(export_mnk_qgis(file = tempfile()), "Provide at least one sf object")
})

test_that("errors when layers are unnamed", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0), coords = c("x","y"), crs = 4326)
  expect_error(export_mnk_qgis(pts, file = tempfile()), "All inputs must be named")
})

test_that("errors when object is not sf", {
  tmp <- tempfile(fileext = ".gpkg")
  expect_error(export_mnk_qgis(not_sf = data.frame(a=1), file = tmp), "is not an sf object")
})

test_that("overwrite TRUE replaces existing file", {
  skip_if_not_installed("sf")
  pts1 <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0), coords = c("x","y"), crs = 4326)
  pts2 <- sf::st_as_sf(data.frame(id = 2, x = 1, y = 1), coords = c("x","y"), crs = 4326)
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  export_mnk_qgis(a = pts1, file = tmp, overwrite = TRUE)
  export_mnk_qgis(b = pts2, file = tmp, overwrite = TRUE)
  layers <- sf::st_layers(tmp)$name
  expect_equal(layers, "b")
})

test_that("overwrite FALSE appends layers", {
  skip_if_not_installed("sf")
  pts1 <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0), coords = c("x","y"), crs = 4326)
  pts2 <- sf::st_as_sf(data.frame(id = 2, x = 1, y = 1), coords = c("x","y"), crs = 4326)
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  export_mnk_qgis(a = pts1, file = tmp, overwrite = TRUE)
  export_mnk_qgis(b = pts2, file = tmp, overwrite = FALSE)
  layers <- sf::st_layers(tmp)$name
  expect_setequal(layers, c("a","b"))
})

test_that("errors when file cannot be removed", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0), coords = c("x","y"), crs = 4326)
  tmp <- tempfile(fileext = ".gpkg")
  file.create(tmp)
  testthat::local_mocked_bindings(
    file.exists = function(...) TRUE,
    unlink = function(...) 0L,
    .package = "base"
  )
  expect_error(export_mnk_qgis(pts = pts, file = tmp, overwrite = TRUE),
               "Cannot remove existing file")
})

test_that("errors when file is missing", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(data.frame(id = 1, x = 0, y = 0),
                      coords = c("x","y"), crs = 4326)

  expect_error(export_mnk_qgis(pts = pts), "You must provide a 'file' path")
})
