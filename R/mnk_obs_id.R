
#' Get Minka Observation Details
#'
#' Retrieves full details for a single Minka observation by its identifier.
#' Queries the Minka API and returns the complete record flattened into a
#' tibble.
#'
#' @param id a single integer. The Minka observation identifier.
#' @param meta a logical value. Reserved for future use. Currently ignored.
#' @return a one-row tibble with the observation data. The result is the
#'   flattened JSON response from the Minka API and contains more than 160
#'   columns covering taxon, user, photos, identifications, and quality
#'   metrics. Key fields include:
#'   \describe{
#'     \item{id}{observation identifier, integer.}
#'     \item{uuid}{unique identifier, character.}
#'     \item{observed_on}{observation date, character.}
#'     \item{created_at}{creation date, character.}
#'     \item{updated_at}{last update, character.}
#'     \item{quality_grade}{quality grade, character.}
#'     \item{latitude}{latitude in WGS84, numeric.}
#'     \item{longitude}{longitude in WGS84, numeric.}
#'     \item{taxon.id}{taxon identifier, integer.}
#'     \item{taxon.name}{scientific name, character.}
#'     \item{taxon.rank}{taxonomic rank, character.}
#'     \item{user.id}{observer identifier, integer.}
#'     \item{user.login}{observer login, character.}
#'     \item{photos}{list column with photo metadata.}
#'     \item{identifications}{list column with identifications.}
#'   }
#'   Returns `NULL` invisibly if the observation is not found or the request
#'   fails.
#' @export
#' @examples
#' \dontrun{
#' obs <- mnk_obs_id(6475)
#' obs$id
#' obs$taxon.name
#' obs$quality_grade
#' }
mnk_obs_id <- function(id, meta = FALSE) {

  if (is.null(id) || length(id) == 0 || is.na(id[1]) ||!is.atomic(id) || length(id) > 1) {
    stop("You must provide a single, non-empty, non-NA ID for the observation.")
  }
  id_char <- as.character(id[1])
  if (nchar(trimws(id_char)) == 0) {
    stop("You must provide a single, non-empty, non-NA ID for the observation.")
  }

  base_url <- "https://api.minka-sdg.org"
  api_path <- "/v1/observations/"
  q_path <- paste0(api_path, id_char)

  response <- httr::GET(base_url, path = q_path, as = "text")

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("Minka API request failed for observation ID ", id_char, ". Status code: ", status)
    return(invisible(NULL))
  }

  response_content <- httr::content(response, as = "text", encoding = "UTF-8")

  if (nchar(response_content) == 0 || response_content == "null") {
    message("API returned an empty or null response for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  parsed_json_full <- tryCatch({
    jsonlite::fromJSON(response_content, simplifyVector = TRUE, flatten = TRUE)
  }, error = function(e) {
    stop("Failed to parse JSON response for observation ID ", id_char, ": ", e$message)
  })

  if (!is.list(parsed_json_full) &&!is.data.frame(parsed_json_full)) {
    message("No data found or unexpected JSON structure (atomic type) for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  if (!is.null(parsed_json_full$results) && is.data.frame(parsed_json_full$results) && nrow(parsed_json_full$results) > 0) {
    df_result <- parsed_json_full$results
    if (nrow(df_result) > 1) {
      warning("Multiple observations found for ID ", id_char, ". Returning only the first one.")
      df_result <- df_result[1, ]
    }
  } else if (is.data.frame(parsed_json_full) && nrow(parsed_json_full) > 0) {
    df_result <- parsed_json_full
  } else {
    message("No data found or unexpected JSON structure for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  return(tibble::as_tibble(df_result))
}
