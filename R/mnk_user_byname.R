#' @title Search Minka Users by Login Name
#' @description Retrieves users whose login name contains a given string from the
#' Minka API.
#' @details Queries the \code{/v1/users/autocomplete} endpoint. This function is
#' mainly used to obtain the user identifier for other functions.
#' @param query A single character string contained in the user login name.
#' @return A tibble with one row per matching user. If no matches are found,
#' it returns an empty tibble with the same column structure. Columns are:
#' \describe{
#'   \item{id}{User identifier, integer.}
#'   \item{login}{Username, character.}
#'   \item{name}{Full name, character.}
#'   \item{observations_count}{Number of observations, integer.}
#'   \item{created_at}{Account creation timestamp, POSIXct.}
#' }
#' @examples
#' \dontrun{
#' # Search for users whose login contains "xavier"
#' mnk_user_byname(query = "xavier")
#' }
#' @export
mnk_user_byname <- function(query) {

  if (missing(query) || is.null(query) || !is.character(query) ||
      length(query) != 1 || is.na(query)) {
    stop("'query' must be a single, non-NA character string.", call. = FALSE)
  }

  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/users/autocomplete"

  empty_res <- tibble::tibble(
    id = integer(),
    login = character(),
    name = character(),
    observations_count = integer(),
    created_at = as.POSIXct(character())
  )

  response <- httr::GET(base_url, path = api_path, query = list(q = query))

  if (httr::http_error(response)) {
    message("Minka API request failed. Status: ", httr::status_code(response))
    return(empty_res)
  }

  content <- httr::content(response, as = "parsed")

  if (is.list(content) && !is.null(content$results)) {
    purrr::map_dfr(content$results, function(x) tibble::tibble(
      id = x$id %||% NA_integer_,
      login = x$login %||% NA_character_,
      name = x$name %||% NA_character_,
      observations_count = x$observations_count %||% NA_integer_,
      created_at = lubridate::ymd_hms(x$created_at %||% NA_character_, quiet = TRUE)
    ))
  } else {
    message("API response was not in the expected format (missing a 'results' list).")
    return(empty_res)
  }
}
