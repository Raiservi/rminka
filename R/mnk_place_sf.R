#' @title Get the sf geometry of a Minka place
#' @description Retrieve the geometry of a Minka place as an `sf` object given
#' its `place_id`. The GeoJSON returned by the Minka API is always in
#' EPSG:4326 (WGS84). By default the function returns the geometry in this
#' CRS, but you can request a different output CRS.
#' @param place_id a single integer id for a Minka place. This id is unique for
#' each Minka place.
#' @param crs coordinate reference system for the output geometry, as an EPSG
#' code or anything accepted by `sf::st_crs()`. Defaults to `4326`.
#' @return An `sf` object with the place geometry. Returns `NULL` invisibly on
#' network error or empty response.
#' @examples
#' \dontrun{
#' # WGS84 (default)
#' sf_sant_feliu <- mnk_place_sf(place_id = 265)
#'
#' # Reprojected to ETRS89 / UTM zone 31N
#' sf_sant_feliu_25831 <- mnk_place_sf(place_id = 265, crs = 25831)
#' }
#' @export
mnk_place_sf <- function(place_id, crs = 4326) {

  if (!is.numeric(place_id) || length(place_id)!= 1 || is.na(place_id)) {
    stop("You must provide a single non-empty numerical 'place_id'.", call. = FALSE)
  }

  if (missing(crs) || is.null(crs)) crs <- 4326

  base_url <- "https://api.minka-sdg.org"
  q_path <- paste0("/v1/places/", as.character(place_id))

  response <- httr::GET(base_url, path = q_path)

  if (httr::http_error(response)) {
    message("Minka API request failed. Status code: ", httr::status_code(response))
    return(invisible(NULL))
  }

  response_content <- httr::content(response, as = "text", encoding = "UTF-8")
  if (nchar(response_content) == 0) {
    message("API returned an empty response.")
    return(invisible(NULL))
  }

  parsed_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)

  if (is.null(parsed_json$results) || length(parsed_json$results) == 0) {
    message("No places found for your query.")
    return(invisible(NULL))
  }

  final_tibble <- purrr::map_dfr(parsed_json$results, function(x) {
    tibble::tibble(
      geojson_string = as.character(jsonlite::toJSON(x$geometry_geojson, auto_unbox = TRUE))
    )
  })

  sf_object <- tibble::as_tibble(final_tibble) %>%
    dplyr::mutate(
      sf_geometry = purrr::map(geojson_string, function(raw_string) {
        cleaned_string <- stringr::str_replace_all(raw_string, "\\\\", "")
        cleaned_string <- sub('^"', '', cleaned_string)
        cleaned_string <- sub('"$', '', cleaned_string)

        sf_geom <- tryCatch({
          suppressWarnings(sf::st_geometry(sf::st_read(cleaned_string, quiet = TRUE))[[1]])
        }, error = function(e) {
          sf::st_point()
        })
        return(sf_geom)
      })
    ) %>%
    dplyr::select(-geojson_string) %>%
    sf::st_as_sf(sf_column_name = "sf_geometry", crs = 4326)

  # reproyección opcional - solo si es diferente de 4326
  if (!identical(suppressWarnings(as.numeric(crs)), 4326)) {
    sf_object <- sf::st_transform(sf_object, crs)
  }

  return(sf_object)
}
