#' @title Get complete taxonomy from WoRMS as a flat tibble
#' @description This function downloads the complete taxonomy from the World Register of Marine Species (WoRMS) for a given scientific name or taxon.
#' @details You need to provide the exact scientific name (e.g., "Genus species") or a higher-level taxon (e.g., "Genus", "Familia"). The function will return the information for the first exact match found.
#' @param scientific_name A string with the scientific name to search for.
#' @return A single-row `tibble` containing the taxonomic and habitat information for the specified name. Returns `NULL` invisibly if the taxon is not found or in case of an API/network error.
#' @examples \dontrun{
#' # Get data for a species
#' diplodus_sargus_df <- get_wrm_tax("Diplodus sargus")
#' print(diplodus_sargus_df)
#'
#' # Get data for a genus
#' diplodus_genus_df <- get_wrm_tax("Diplodus")
#' print(diplodus_genus_df)
#' }

library(jsonlite)
library(httr)
library(tibble)

get_wrm_tax <- function(scientific_name) {

  # Definir el operador %||% dentro de la función para que sea autónoma
  `%||%` <- function(a, b) {
    if (is.null(a)) b else a
  }

  # 1. Validación de la entrada
  if (is.null(scientific_name) ||!is.character(scientific_name) || length(scientific_name)!= 1 || nchar(trimws(scientific_name)) == 0) {
    stop("'scientific_name' must be a single non-empty character string.")
  }

  # 2. Construcción de la URL
  encoded_name <- gsub(" ", "%20", scientific_name)
  api_url <- sprintf("https://www.marinespecies.org/rest/AphiaRecordsByName/%s?like=false&marine_only=false&offset=1", encoded_name)

  # 3. Llamada a la API con manejo de errores de red
  response <- tryCatch({
    httr::GET(url = api_url)
  }, error = function(e) {
    message("Network error: WoRMS API is unavailable or unreachable. ", e$message)
    return(NULL)
  })

  if (is.null(response)) return(invisible(NULL))

  # 4. Verificación del estado de la respuesta HTTP
  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("WoRMS API request failed. Status code: ", status)
    return(invisible(NULL))
  }

  # 5. Procesamiento del contenido
  response_content <- httr::content(response, as = "text", encoding = "UTF-8")

  if (nchar(response_content) <= 2) {
    message("No taxon found for the scientific name: '", scientific_name, "'.")
    return(invisible(NULL))
  }

  parsed_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)

  if (is.null(parsed_json) || length(parsed_json) == 0) {
    message("API returned an empty or invalid response for: '", scientific_name, "'.")
    return(invisible(NULL))
  }

  taxon_data <- parsed_json[[1]]

  # 6. Construcción del tibble plano usando la estructura preferida
  output_tibble <- tibble::tibble(
    valid_AphiaID = taxon_data$valid_AphiaID %||% NA_integer_,
    valid_name = taxon_data$valid_name %||% NA_character_,
    rank = taxon_data$rank %||% NA_character_,
    kingdom = taxon_data$kingdom %||% NA_character_,
    phylum = taxon_data$phylum %||% NA_character_,
    class = taxon_data$class %||% NA_character_,
    order = taxon_data$order %||% NA_character_,
    family = taxon_data$family %||% NA_character_,
    genus = taxon_data$genus %||% NA_character_,
    isMarine = as.logical(taxon_data$isMarine %||% NA),
    isBrackish = as.logical(taxon_data$isBrackish %||% NA),
    isFreshwater = as.logical(taxon_data$isFreshwater %||% NA),
    isTerrestrial = as.logical(taxon_data$isTerrestrial %||% NA),
    isExtinct = as.logical(taxon_data$isExtinct %||% NA)
  )

  return(output_tibble)
 }

