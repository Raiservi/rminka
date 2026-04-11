#' @title Projects Subscribed by a Minka User
#' @description Retrieves projects to which a user is explicitly subscribed
#'   from Minka and returns project metadata as a tibble.
#' @param id_user a single integer or numeric value identifying the user.
#' @return A tibble with one row per subscribed project. Returns an empty
#'   tibble with zero rows if the user has no subscriptions. Columns are:
#' \describe{
#'   \item{id}{Project identifier, integer.}
#'   \item{title}{Project title, character.}
#'   \item{description}{Project description, character.}
#'   \item{slug}{URL slug, character.}
#'   \item{icon}{Icon URL, character.}
#'   \item{place_id}{Associated place identifier, integer or `NA`.}
#'   \item{created_at}{Creation date, character in ISO 8601 format.}
#' }
#' @examples
#' \dontrun{
#' mnk_user_proj(6)
#' }
#' @export
#'
mnk_user_proj <- function(id_user) {

  if (is.null(id_user) || length(id_user) != 1 || !is.numeric(id_user) ||
      is.na(id_user)) {
    stop("You must provide a single, non-NA numeric 'id_user'.", call. = FALSE)
  }

  api_url <- paste0("https://api.minka-sdg.org/v1/users/", id_user, "/projects")

  response <- httr::GET(api_url)

  if (httr::http_error(response)) {
    message("Minka API request failed. Status: ", httr::status_code(response))
    return(tibble::tibble())
  }

  content <- httr::content(response, as = "parsed")

  if (is.list(content) && !is.null(content$results)) {
    purrr::map_dfr(content$results, function(x) tibble::tibble(
      id = rlang::`%||%`(x$id, NA_integer_),
      title = rlang::`%||%`(x$title, NA_character_),
      description = rlang::`%||%`(x$description, NA_character_),
      slug = rlang::`%||%`(x$slug, NA_character_),
      icon = rlang::`%||%`(x$icon, NA_character_),
      place_id = rlang::`%||%`(x$place_id, NA_integer_),
      created_at = rlang::`%||%`(x$created_at, NA_character_)
    ))
  } else {
    message("API response was not in the expected format (missing a 'results' list).")
    return(tibble::tibble())
  }
}
mnk_user_proj <- function(id_user) {

  if (is.null(id_user) || length(id_user) != 1 || !is.numeric(id_user) ||
      is.na(id_user)) {
    stop("You must provide a single, non-NA numeric 'id_user'.", call. = FALSE)
  }

  api_url <- paste0("https://api.minka-sdg.org/v1/users/", id_user, "/projects")

  response <- httr::GET(api_url)

  if (httr::http_error(response)) {
    message("Minka API request failed. Status: ", httr::status_code(response))
    return(tibble::tibble())
  }

  content <- httr::content(response, as = "parsed")

  if (is.list(content) && !is.null(content$results)) {
    purrr::map_dfr(content$results, function(x) tibble::tibble(
      id = rlang::`%||%`(x$id, NA_integer_),
      title = rlang::`%||%`(x$title, NA_character_),
      description = rlang::`%||%`(x$description, NA_character_),
      slug = rlang::`%||%`(x$slug, NA_character_),
      icon = rlang::`%||%`(x$icon, NA_character_),
      place_id = rlang::`%||%`(x$place_id, NA_integer_),
      created_at = rlang::`%||%`(x$created_at, NA_character_)
    ))
  } else {
    message("API response was not in the expected format (missing a 'results' list).")
    return(tibble::tibble())
  }
}
