#' Export sf Minka Objects to a GeoPackage for QGIS
#'
#' Writes one or more \code{sf} objects to a GeoPackage (\code{.gpkg}) with
#' one layer per object. The function is designed for a smooth QGIS workflow:
#' it ensures valid geometries, drops Z/M dimensions, removes list-columns that
#' GDAL cannot write, and transforms to a target CRS.
#'
#' @param... One or more objects of class \code{sf}. Each object must be named;
#' the name is used as the layer name in the GeoPackage (e.g.,
#' \code{export_mnk_qgis(points = pts, polygons = polys)}).
#' @param file Character. Path to the output GeoPackage. Default is
#' \code{"datos_qgis.gpkg"}. The \code{.gpkg} extension is added if missing.
#' @param crs CRS to transform to before writing. Accepts anything valid for
#' \code{sf::st_transform()} (e.g., \code{4326} or \code{"EPSG:4326"}).
#' Default is \code{4326} (WGS 84).
#' @param overwrite Logical. If \code{TRUE} and \code{file} exists, the file is
#' deleted before writing. If \code{FALSE} and the file exists, the function
#' appends layers (and will error if a layer name already exists). Default is
#' \code{TRUE}.
#'
#' @return Invisibly returns the normalized path to the written GeoPackage.
#' Called for its side effect of writing the file.
#'
#' @details
#' The function performs the following steps for each layer:
#' \enumerate{
#' \item Transform to \code{crs} with \code{sf::st_transform()}.
#' \item Drop Z and M dimensions with \code{sf::st_zm()}.
#' \item Repair invalid geometries with \code{sf::st_make_valid()}.
#' \item Remove non-geometry list-columns, which GDAL cannot write to GPKG.
#' \item Write the layer with \code{sf::st_write()} using the GPKG driver.
#' }
#' If \code{overwrite = TRUE}, the existing file is removed first. If the file
#' cannot be removed (e.g., it is open in QGIS), the function stops with an
#' informative error.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # points
#' pts <- st_as_sf(data.frame(id = 1:2, x = c(0.8, 0.9), y = c(40.9, 41.0)),
#' coords = c("x", "y"), crs = 4326)
#'
#' # polygon
#' poly <- st_as_sf(st_sfc(
#' st_polygon(list(rbind(c(0.7,40.8), c(1.0,40.8), c(1.0,41.1),
#' c(0.7,41.1), c(0.7,40.8)))),
#' crs = 4326))
#' # The names of the layers will be exampl_points and  exampl_area
#' export_mnk_qgis(exampl_points = pts, exampl_area = poly,
#' file = tempfile(fileext = ".gpkg"))
#' }
#'
#' @export
export_mnk_qgis <- function(..., file = "datos_qgis.gpkg", crs = 4326, overwrite = TRUE) {
  # collect layers
  layers <- list(...)
  if (length(layers) == 0L) {
    stop("Provide at least one sf object.", call. = FALSE)
  }
  layer_names <- names(layers)
  if (is.null(layer_names) || any(!nzchar(layer_names))) {
    stop("All inputs must be named: export_mnk_qgis(points = pts, polygons = polys).",
         call. = FALSE)
  }

  # normalize file name
  if (!grepl("\\.gpkg$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".gpkg")
  }

  # handle existing file
  if (file.exists(file)) {
    if (isTRUE(overwrite)) {
      unlink(file)
      if (file.exists(file)) {
        stop("Cannot remove existing file '", file,
             "'. Close it in QGIS or choose another path.", call. = FALSE)
      }
    }
  }

  # write layers
  for (nm in layer_names) {
    x <- layers[[nm]]
    if (!inherits(x, "sf")) {
      stop("Object '", nm, "' is not an sf object.", call. = FALSE)
    }

    # transform CRS
    x <- sf::st_transform(x, crs)

    # drop Z/M
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")

    # repair geometries (quietly)
    if (any(!sf::st_is_valid(x))) {
      x <- suppressWarnings(sf::st_make_valid(x))
    }

    # drop non-geometry list-columns (GDAL cannot write them)
    is_list_col <- vapply(x, is.list, logical(1))
    is_sfc_col <- vapply(x, inherits, logical(1), what = "sfc")
    drop_cols <- is_list_col &!is_sfc_col
    if (any(drop_cols)) {
      x <- x[,!drop_cols, drop = FALSE]
    }

    # write layer
    sf::st_write(
      obj = x,
      dsn = file,
      layer = nm,
      driver = "GPKG",
      append = file.exists(file),
      delete_layer = isTRUE(overwrite),
      quiet = TRUE
    )
  }

  invisible(normalizePath(file, winslash = "/"))
}
