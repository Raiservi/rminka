#' Convert Minka observations to an sf POINT object
#'
#' Takes a tibble returned by `mnk_obs()` and turns it into an sf object,
#' keeping only the columns you ask for. Latitude and longitude are always
#' used for geometry, and `observed_on` is coerced to Date if present.
#'
#' @param data A data.frame or tibble with at least `latitude` and `longitude`.
#' @param ... Columns to keep, using tidyselect syntax (e.g. `id`, `taxon_name`,
#'   `starts_with("taxon_")`). Geometry columns are added automatically.
#' @param crs Coordinate reference system for the output. Default is 4326 (WGS84),
#'   which works directly with leaflet.
#' @param keep_coords Logical. If TRUE, keep `latitude` and `longitude` as
#'   regular columns in addition to the geometry. Default TRUE.
#'
#' @return An sf object of type POINT with the selected attributes.
#' @export

mnk_obs_sf <- function(data, ..., crs = 4326, keep_coords = TRUE) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame or tibble")
  }
  if (!all(c("latitude", "longitude") %in% names(data))) {
    stop("`data` must contain `latitude` and `longitude` columns")
  }

  # select requested columns, always add lat/lon and observed_on if available
  out <- data |>
    dplyr::select(
      ...,
      dplyr::all_of(c("latitude", "longitude")),
      dplyr::any_of("observed_on")
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of("observed_on"), as.Date)
    ) |>
    dplyr::filter(
      !is.na(.data$latitude),
      !is.na(.data$longitude)
    ) |>
    dplyr::distinct()

  sf::st_as_sf(
    out,
    coords = c("longitude", "latitude"),
    crs = crs,
    remove = !keep_coords
  )
}
