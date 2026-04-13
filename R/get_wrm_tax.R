##' Get WoRMS Taxonomy
#'
#' Downloads taxonomic information from the World Register of Marine Species
#' (WoRMS) for a given scientific name. Returns the first exact match found.
#'
#' @param scientific_name a character string with the scientific name to
#' search for, for example "Diplodus sargus" or "Diplodus".
#' @return a one-row tibble with taxonomic and habitat flags. Returns
#' \code{NULL} invisibly if the taxon is not found or the request fails.
#' The tibble contains the following columns:
#' \describe{
#' \item{valid_AphiaID}{WoRMS AphiaID, integer.}
#' \item{valid_name}{accepted scientific name, character.}
#' \item{rank}{taxonomic rank, character.}
#' \item{kingdom}{kingdom, character.}
#' \item{phylum}{phylum, character.}
#' \item{class}{class, character.}
#' \item{order}{order, character.}
#' \item{family}{family, character.}
#' \item{genus}{genus, character.}
#' \item{isMarine}{marine flag, logical.}
#' \item{isBrackish}{brackish flag, logical.}
#' \item{isFreshwater}{freshwater flag, logical.}
#' \item{isTerrestrial}{terrestrial flag, logical.}
#' \item{isExtinct}{extinct flag, logical.}
#' }
#' @export
#' @examples
#' \dontrun{
#' # Get data for a species
#' get_wrm_tax("Diplodus sargus")
#'
#' # Get data for a genus
#' get_wrm_tax("Diplodus")
#' }
#' @export
get_wrm_tax <- function(scientific_name) {

  if (is.null(scientific_name) ||!is.character(scientific_name) || length(scientific_name)!= 1 || nchar(trimws(scientific_name)) == 0) {
    stop("'scientific_name' must be a single non-empty character string.")
  }

  encoded_name <- gsub(" ", "%20", scientific_name)

  api_url <- sprintf(
    "https://www.marinespecies.org/rest/AphiaRecordsByName/%s?like=false&marine_only=false&offset=1",
    encoded_name
  )

  response <- tryCatch({
    httr::GET(url = api_url)
  }, error = function(e) {
    message("Network error: WoRMS API is unavailable or unreachable. ", e$message)
    return(NULL)
  })

  if (is.null(response)) return(invisible(NULL))

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("WoRMS API request failed. Status code: ", status)
    return(invisible(NULL))
  }

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

  output_tibble <- tibble::tibble(
    valid_AphiaID = rlang::`%||%`(taxon_data$valid_AphiaID, NA_integer_),
    valid_name = rlang::`%||%`(taxon_data$valid_name, NA_character_),
    rank = rlang::`%||%`(taxon_data$rank, NA_character_),
    kingdom = rlang::`%||%`(taxon_data$kingdom, NA_character_),
    phylum = rlang::`%||%`(taxon_data$phylum, NA_character_),
    class = rlang::`%||%`(taxon_data$class, NA_character_),
    order = rlang::`%||%`(taxon_data$order, NA_character_),
    family = rlang::`%||%`(taxon_data$family, NA_character_),
    genus = rlang::`%||%`(taxon_data$genus, NA_character_),
    isMarine = as.logical(rlang::`%||%`(taxon_data$isMarine, NA)),
    isBrackish = as.logical(rlang::`%||%`(taxon_data$isBrackish, NA)),
    isFreshwater = as.logical(rlang::`%||%`(taxon_data$isFreshwater, NA)),
    isTerrestrial = as.logical(rlang::`%||%`(taxon_data$isTerrestrial, NA)),
    isExtinct = as.logical(rlang::`%||%`(taxon_data$isExtinct, NA))
  )

  return(output_tibble)
}
