#' @title Projects Subscribed by a Minka User
#' @description Retrieves projects to which a user is explicitly subscribed
#'   from Minka and returns project metadata as a tibble.
#' @param id_user A single integer or numeric value identifying the user.
#' @return A tibble with one row per subscribed project. If no
#'   subscriptions are found or an error occurs, it returns an empty tibble
#'   with the same column structure. Columns are:
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

  empty_res <- tibble::tibble(
    id = integer(),
    title = character(),
    description = character(),
    slug = character(),
    icon = character(),
    place_id = integer(),
    created_at = character()
  )

  api_url <- paste0("https://api.minka-sdg.org/v1/users/", id_user, "/projects")

  response <- tryCatch({
    httr::GET(api_url)
  }, error = function(e) {
    message("Network error: Minka API is unavailable.")
    return(NULL)
  })

  if (is.null(response) || httr::http_error(response)) {
    if (!is.null(response)) {
      message("Minka API request failed. Status: ", httr::status_code(response))
    }
    return(empty_res)
  }

  content <- httr::content(response, as = "parsed")

  if (is.list(content) && !is.null(content$results) && length(content$results) > 0) {


    return(purrr::map_dfr(content$results, function(x) tibble::tibble(
      id = x$id %||% NA_integer_,
      title = x$title %||% NA_character_,
      description = x$description %||% NA_character_,
      slug = x$slug %||% NA_character_,
      icon = x$icon %||% NA_character_,
      place_id = x$place_id %||% NA_integer_,
      created_at = x$created_at %||% NA_character_
    )))

  } else {

    return(empty_res)
  }
}
