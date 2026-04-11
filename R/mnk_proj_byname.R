#' @title Search Minka Projects by Name
#' @description Retrieves projects whose title contains a given string from the
#' Minka API.
#' @details Queries the \code{/v1/projects/autocomplete} endpoint. This
#' function is mainly used to obtain the project identifier for other functions.
#' @param query a single character string contained in the project name.
#' @return A tibble with one row per matching project. Returns an empty tibble
#' with zero rows if no matches are found. Columns are:
#' \describe{
#' \item{id}{Project identifier, integer.}
#' \item{title}{Project title, character.}
#' \item{place_id}{Associated place identifier, integer or `NA`.}
#' \item{slug}{URL slug, character.}
#' \item{created_at}{Creation timestamp, character.}
#' \item{updated_at}{Last update timestamp, character.}
#' \item{project_type}{Project type, character.}
#' \item{description}{Project description, character.}
#' }
#' @examples
#' \dontrun{
#' # Search for projects containing "Biomarato 2025"
#' mnk_proj_byname(query = "Biomarato 2025")
#' }
#' @export
mnk_proj_byname <- function(query) {

  if (is.null(query) || length(query) == 0 || is.na(query[1]) ||
      !is.character(query) || nchar(trimws(query[1])) == 0) {
    stop("You must provide a single, non-empty, non-NA character 'query' for the project search.")
  }
  if (length(query) > 1) {
    stop("You must provide a single query string. Only one query is accepted.")
  }

  base_url <- "https://api.minka-sdg.org"
  # sin stringr: codifica espacios como %20, acentos, etc.
  q_enc <- utils::URLencode(query, reserved = TRUE)
  q_path <- paste0("/v1/projects/autocomplete?q=", q_enc)

  response <- httr::GET(base_url, path = q_path)

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("Minka API request failed for query '", query, "'. Status code: ", status)
    return(invisible(NULL))
  }

  content <- httr::content(response, as = "parsed", encoding = "UTF-8")

  if (is.null(content)) {
    message("API returned an empty or null response for query '", query, "'.")
    return(invisible(NULL))
  }

  if (!is.list(content) || is.null(content$results) || length(content$results) == 0) {
    message("No projects found for query '", query, "'.")
    return(tibble::tibble())
  }

  purrr::map_dfr(content$results, function(x) tibble::tibble(
    id = rlang::`%||%`(x$id, NA_integer_),
    title = rlang::`%||%`(x$title, NA_character_),
    place_id = rlang::`%||%`(x$place_id, NA_integer_),
    slug = rlang::`%||%`(x$slug, NA_character_),
    created_at = rlang::`%||%`(x$created_at, NA_character_),
    updated_at = rlang::`%||%`(x$updated_at, NA_character_),
    project_type = rlang::`%||%`(x$project_type, NA_character_),
    description = rlang::`%||%`(x$description, NA_character_)
  ))
}
