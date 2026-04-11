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

obs



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
