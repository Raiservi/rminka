#' @title Get Minka Place as sf Object
#' @description Retrieve a Minka place as a simple features (`sf`) object given
#' its `place_id`. The function queries the Minka API and returns the place
#' boundary geometry together with descriptive metadata. The source GeoJSON is
#' always in EPSG:4326 (WGS 84). By default the output retains this CRS, but an
#' alternative CRS may be requested.
#' @param place_id a single integer identifier for a Minka place. Each place has
#' a unique identifier.
#' @param crs coordinate reference system for the output geometry, supplied as
#' an EPSG code or any value accepted by \code{sf::st_crs()}. Defaults to
#' \code{4326}.
#' @return An \code{sf} object with one row per place and the following columns.
#' Returns \code{NULL} invisibly if the request fails or the response is empty:
#' \describe{
#'   \item{place_id}{Place identifier, integer.}
#'   \item{name}{Place name, character.}
#'   \item{display_name}{Full display name, character.}
#'   \item{slug}{URL slug, character.}
#'   \item{uuid}{Universally unique identifier, character.}
#'   \item{place_type}{Type of place, character or \code{NA}.}
#'   \item{admin_level}{Administrative level, integer or \code{NA}.}
#'   \item{bbox_area}{Bounding box area, numeric.}
#'   \item{location}{Centroid as "lat,lon" string, character.}
#'   \item{geometry}{Simple feature geometry (POINT, POLYGON or MULTIPOLYGON),
#'   CRS 4326 unless transformed.}
#' }
#' @examples
#' \dontrun{
#' # Retrieve place 253 in WGS 84
#' place <- mnk_place_sf(place_id = 253)
#'
#' # Retrieve the same place reprojected to ETRS89 / UTM zone 31N
#' place_25831 <- mnk_place_sf(place_id = 253, crs = 25831)
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

  response_content <- rawToChar(response$content)
  response_content <- enc2utf8(response_content)

  if (!nzchar(response_content)) {
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
      place_id = suppressWarnings(as.integer(rlang::`%||%`(x$id, NA_integer_))),
      name = rlang::`%||%`(x$name, NA_character_),
      display_name = rlang::`%||%`(x$display_name, NA_character_),
      slug = rlang::`%||%`(x$slug, NA_character_),
      uuid = rlang::`%||%`(x$uuid, NA_character_),
      place_type = rlang::`%||%`(x$place_type, NA_character_),
      admin_level = suppressWarnings(as.integer(rlang::`%||%`(x$admin_level, NA_integer_))),
      bbox_area = suppressWarnings(as.numeric(rlang::`%||%`(x$bbox_area, NA_real_))),
      location = rlang::`%||%`(x$location, NA_character_),
      geojson_string = as.character(jsonlite::toJSON(rlang::`%||%`(x$geometry_geojson, NULL), auto_unbox = TRUE, null = "null"))
    )
  })

  sf_object <- final_tibble %>%
    dplyr::mutate(
      geometry = purrr::map(final_tibble$geojson_string, function(raw_string) {
        if (is.na(raw_string) || raw_string == "null" || raw_string == "") {
          return(sf::st_point())
        }
        cleaned_string <- stringr::str_replace_all(raw_string, "\\\\", "")
        cleaned_string <- sub('^"', '', cleaned_string)
        cleaned_string <- sub('"$', '', cleaned_string)

        tryCatch({
          suppressWarnings(sf::st_geometry(sf::st_read(cleaned_string, quiet = TRUE))[[1]])
        }, error = function(e) {
          sf::st_point()
        })
      })
    ) %>%
    dplyr::select(-"geojson_string") %>%
    sf::st_as_sf(sf_column_name = "geometry", crs = 4326)

  if (!identical(suppressWarnings(as.numeric(crs)), 4326)) {
    sf_object <- sf::st_transform(sf_object, crs)
  }

  sf_object <- sf_object[, c("place_id","name","display_name","slug","uuid",
                             "place_type","admin_level","bbox_area","location","geometry")]

  return(sf_object)
}
