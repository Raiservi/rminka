#' @title Information on Minka projects
#' @description Get information on Minka projects selected by a string contained in the project name.
#' @param query A string that is contained in the project name.
#' @return A data frame with all the projects that contain the string with some details of those projects.
#' @examples \dontrun{
#' mnk_obs <- mnk_proj_byname(query="Biomarato 2025")
#' mnk_obs_id(m_obs$id[1])
#' }
#' @export
#'
# La función, con el if para el NULL de `as="parsed"`
mnk_proj_byname <- function(query) {
  #... (comprobaciones de query y llamada a httr::GET sin cambios)...
  if (is.null(query) || length(query) == 0 || is.na(query[1]) ||!is.character(query) || nchar(trimws(query[1])) == 0) {
    stop("You must provide a single, non-empty, non-NA character 'query' for the project search.")
  }
  if (length(query) > 1) {
    stop("You must provide a single query string. Only one query is accepted.")
  }

  base_url <- "https://api.minka-sdg.org"
  query_parsed <- stringr::str_replace_all(query, " ", "%20")
  q_path <- paste0("/v1/projects/autocomplete?q=", query_parsed)
  response <- httr::GET(base_url, path = q_path)

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("Minka API request failed for query '", query, "'. Status code: ", status)
    return(invisible(NULL))
  }

  content <- httr::content(response, as = "parsed", encoding = "UTF-8")

  # --- ¡AQUÍ LA CORRECCIÓN CLAVE PARA EL TEST DE RESPUESTA NULA! ---
  # `httr::content(as="parsed")` devuelve NULL si el body está vacío o es 'null'.
  # Este bloque captura ese caso específico.
  if (is.null(content)) {
    message("API returned an empty or null response for query '", query, "'.")
    return(invisible(NULL))
  }
  # --------------------------------------------------------------------

  if (!is.list(content) || is.null(content$results) || length(content$results) == 0) {
    message("No projects found for query '", query, "'.")
    return(tibble::tibble())
  }

  final_tibble <- purrr::map_dfr(content$results, ~tibble::tibble(
    id =.x$id %||% NA_integer_,
    title =.x$title %||% NA_character_,
    place_id =.x$place_id %||% NA_integer_,
    slug =.x$slug %||% NA_character_,
    created_at =.x$created_at %||% NA_character_,
    updated_at =.x$updated_at %||% NA_character_,
    project_type =.x$project_type %||% NA_character_,
    description =.x$description %||% NA_character_
  ))

  return(final_tibble)
}
