#' Convert Minka Observations to sf
#'
#' Converts a tibble returned by \code{\link{mnk_obs}} into an sf POINT
#' object. Keeps selected columns and coerces \code{observed_on} to Date
#' when present.
#'
#' @param data a data frame or tibble with at least \code{latitude} and
#'   \code{longitude} columns, typically from \code{mnk_obs()}.
#' @param ... columns to retain, using tidyselect syntax. \code{latitude}
#'   and \code{longitude} are always added for geometry.
#' @param crs coordinate reference system for the output, defaults to 4326
#'   (WGS84).
#' @param keep_coords logical. If TRUE, retains \code{latitude} and
#'   \code{longitude} as regular columns in addition to geometry.
#' @return an sf object with POINT geometry (EPSG:4326 by default) and the
#'   selected attributes. Returns an empty sf object if no valid coordinates
#'   are present.
#' @seealso \code{\link{mnk_obs}} for downloading observations.
#' @importFrom dplyr select all_of any_of mutate across filter distinct
#' @importFrom sf st_as_sf
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' obs <- mnk_obs(taxon_name = "Parablennius pilicornis", year = 2024)
#' obs_sf <- mnk_obs_sf(obs, id, taxon_name, observed_on)
#' obs_sf
#' }
mnk_obs_sf <- function(data, ..., crs = 4326, keep_coords = TRUE) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame or tibble", call. = FALSE)
  }
  if (!all(c("latitude", "longitude") %in% names(data))) {
    stop("`data` must contain `latitude` and `longitude` columns", call. = FALSE)
  }

  out <- dplyr::select(
    data,
    ...,
    dplyr::all_of(c("latitude", "longitude")),
    dplyr::any_of("observed_on")
  )
  out <- dplyr::mutate(
    out,
    dplyr::across(dplyr::any_of("observed_on"), as.Date)
  )
  out <- dplyr::filter(
    out,
    !is.na(.data$latitude),
    !is.na(.data$longitude)
  )
  out <- dplyr::distinct(out)

  sf::st_as_sf(
    out,
    coords = c("longitude", "latitude"),
    crs = crs,
    remove = !keep_coords
  )
}
