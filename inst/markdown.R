library(rminka)
library(dplyr)
library(tibble)
library(sf)
library(leaflet)
#=========================================================================================
mnk_obs_sf <- function(data,..., crs = 4326, keep_coords = TRUE) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame or tibble")
  }
  if (!all(c("latitude", "longitude") %in% names(data))) {
    stop("`data` must contain `latitude` and `longitude` columns")
  }

  # select requested columns, always keep lat/lon and observed_on if available
  out <- data |>
    dplyr::select(
      ...,
      latitude,
      longitude,
      dplyr::any_of("observed_on")
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of("observed_on"), as.Date)
    ) |>
    dplyr::filter(!is.na(latitude),!is.na(longitude)) |>
    dplyr::distinct()

  sf::st_as_sf(
    out,
    coords = c("longitude", "latitude"),
    crs = crs,
    remove =!keep_coords
  )
}
#========================================================================================

export_qgis <- function(..., file = "datos_qgis.gpkg", crs = 4326, overwrite = TRUE) {
  capas <- list(...)

  if (length(capas) == 0) stop("Pasa al menos un sf")
  if (is.null(names(capas)) || any(names(capas) == "")) {
    stop("Nombra cada capa: export_qgis(puntos = pts, poligonos = polys)")
  }

  # 1. si existe y quieres sobrescribir, intenta borrarlo
  if (file.exists(file)) {
    if (!overwrite) stop("El archivo ya existe. Usa overwrite = TRUE o cambia el nombre.")
    unlink(file)
    if (file.exists(file)) {
      stop("No puedo borrar '", file, "'. Ciérralo en QGIS primero.")
    }
  }

  # 2. escribe cada capa
  for (nm in names(capas)) {
    x <- capas[[nm]]
    if (!inherits(x, "sf")) stop("'", nm, "' no es sf")

    x <- st_transform(x, crs)
    x <- st_zm(x, drop = TRUE) # quita Z/M
    x <- st_make_valid(x) # repara por si acaso

    # quita columnas lista que rompen GPKG
    keep <-!vapply(x, is.list, TRUE) | vapply(x, inherits, TRUE, "sfc")
    x <- x[, keep]

    st_write(x, dsn = file, layer = nm, append = file.exists(file), quiet = TRUE)
    message("✓ ", nm, ": ", nrow(x), " entidades")
  }
  message("Guardado en ", normalizePath(file))
  invisible(file)
}










#==================================================================================
places <- mnk_places_byname("Forum")
places[,1:6]

place <- mnk_place_sf(253)

place <- place |>
  mutate(
    name = places[2,3],
    id = places[2,1]
  )
place

# 1. descarga y añade columnas

obs_place <- rminka::mnk_place_obs(place_id= 253, year = 2025, month = 2)

obs_place

forum_sf <- leaflet() |>
  addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
  addProviderTiles("OpenStreetMap", group = "OSM") |>
  addPolygons(
    data = place,
    color = "#2c7fb8", weight = 2, fillOpacity = 0.1,
    label = ~name
  ) |>
  addLayersControl(baseGroups = c("Satélite", "OSM"))

forum_sf

forum_obs_sf <- mnk_obs_sf(data = obs_place, taxon_name,url_picture, observed_on, id)

forum_sf |>
  addMarkers(
    data = forum_obs_sf,
    popup = ~paste0(
      "<b>", taxon_name, "</b><br>",
      "ID: ", id, "<br>",
      "User. ", "use_login","<br>",
      "Fecha: ", observed_on,"<br>",
      "<img src='",url_picture, "' width='100'>","<br>"
    ))


obs <- mnk_obs(taxon_name= "Diplodus sargus", year=2025, user_id=6, quiet = TRUE)

mnk_user_info(4)




leaflet() |>
  addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
  addProviderTiles("OpenStreetMap", group = "OSM") |>
  addMarkers(
    data = obs_sf_pl,
    popup = ~paste0(
      "<b>", taxon_name, "</b><br>",
      "Obs ID: ", id, "<br>",
      "User name: ", user_login, "<br>",
      "Date: ", observed_on, "<br>",
      "Specie: ", taxon_name, "<br>",
      "<img src='", url_picture, "' width='100' style='border-radius:5px; margin:1px 0;'>","<br>",
      "<a href='", uri, "' target='_blank'>View in Minka-sdg.com</a>"


    )
  ) |>
  addLayersControl(baseGroups = c("Satélite", "OSM"))

export_qgis(observaciones = forum_obs_sf , zona =place, file = "Estudio_forum.gpkg")

#==================================================================================

shrt <-shrt_name (c("Diplodus sargus sargus", "Diplodus vulgaris", "Diplodus cervinus"))
shrt
shrt2 <-shrt <-shrt_name ("Diplodus sargus")
shrt2

mnk_user_proj(6)

mnk_user_obs(user_id=6, year=2025, month=8)

mnk_user_info(6)

mnk_user_byname(query="xavier")
################################################################################

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

  # 1. Extrae TODO en un tibble, no solo la geometría
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

  # 2. Convierte SOLO geojson_string a geometría sf
  sf_object <- final_tibble %>%
    dplyr::mutate(
      geometry = purrr::map(geojson_string, function(raw_string) {
        if (is.na(raw_string) || raw_string == "null" || raw_string == "") {
          return(sf::st_point()) # geometría vacía
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
    dplyr::select(-geojson_string) %>%
    sf::st_as_sf(sf_column_name = "geometry", crs = 4326)

  # 3. Reproyección opcional
  if (!identical(suppressWarnings(as.numeric(crs)), 4326)) {
    sf_object <- sf::st_transform(sf_object, crs)
  }

  # ordena columnas para que sea legible
  sf_object <- sf_object[, c("place_id","name","display_name","slug","uuid",
                             "place_type","admin_level","bbox_area","location","geometry")]

  return(sf_object)
}

place<- mnk_place_sf(place_id = 265)

place

export_mnk_qgis(sant_feliu=place, file = "biomarato.gpkg")

sf  <-leaflet(place) |>
  addProviderTiles("OpenStreetMap", group = "OSM") |>
  addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
  addPolygons(
    color = "#2c4fb8",
    weight = 2,
    opacity = 1,
    fillOpacity = 0.4,
    label = ~display_name, # information added from previous function
    highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)
  ) |>
  addLayersControl(baseGroups = c("Satélite", "OSM"))
sf


place

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

g <- export_mnk_qgis(place)

export_mnk_qgis(sant_feliu = place, file = "capa.gpkg")


