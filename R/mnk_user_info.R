#' @title Get Information About a Specific Minka User
#' @description Retrieves public profile information for a user from the Minka
#' API.
#' @details Queries the \code{/v1/users/{id}} endpoint. The \code{id_user}
#' corresponds to the identifier in profile URLs such as
#' \url{https://minka-sdg.org/users/6}.
#' @param id_user a single atomic value, numeric or character, representing the
#' user identifier.
#' @return A tibble with one row containing user metadata, or `NULL` invisibly
#' if the user is not found or a network error occurs. Columns are:
#' \describe{
#' \item{id}{User identifier, integer.}
#' \item{login}{Username, character.}
#' \item{name}{Full name, character.}
#' \item{created_at}{Account creation timestamp, POSIXct.}
#' \item{observations_count}{Number of observations, integer.}
#' \item{identifications_count}{Number of identifications, integer.}
#' \item{species_count}{Number of observed species, integer.}
#' \item{activity_count}{Activity score, integer.}
#' \item{journal_posts_count}{Number of journal posts, integer.}
#' \item{orcid}{ORCID identifier, character or `NA`.}
#' \item{icon_url}{URL of profile image, character.}
#' \item{site_id}{Site identifier, integer.}
#' \item{roles}{List-column with user roles, list.}
#' \item{spam}{Spam flag, logical.}
#' \item{suspended}{Suspended flag, logical.}
#' \item{universal_search_rank}{Search rank, integer.}
#' }
#' @examples
#' \dontrun{
#' # Information about user_id 4 (xasalvador)
#' mnk_user_info(4)
#' }
#' @export
mnk_user_info <- function(id_user) {

  if (missing(id_user) || is.null(id_user)) {
    stop("'id_user' must be provided.", call. = FALSE)
  }
  if (!is.atomic(id_user) || length(id_user)!= 1) {
    stop("'id_user' must be a single character string or number.", call. = FALSE)
  }

  id_for_msg <- as.character(id_user)
  base_url <- "https://api.minka-sdg.org"
  api_path <- paste0("v1/users/", utils::URLencode(id_for_msg, reserved = TRUE))

  response <- tryCatch({
    httr::GET(url = base_url, path = api_path)
  }, error = function(e) {
    message("Network error: Minka API is unavailable. ", e$message)
    return(NULL)
  })

  if (is.null(response)) {
    return(invisible(NULL))
  }

  if (httr::http_error(response)) {
    message("Minka API request failed. Status code: ", httr::status_code(response))
    return(invisible(NULL))
  }

  response_content <- httr::content(response, as = "text", encoding = "UTF-8")
  if (nchar(response_content) == 0 || identical(response_content, "null")) {
    message("API returned an empty or null response for user: ", id_for_msg, ".")
    return(invisible(NULL))
  }

  xx <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
  if (is.null(xx$results) || length(xx$results) == 0) {
    message("No user details found for id_user = ", id_for_msg, ".")
    return(invisible(NULL))
  }
  user_data <- xx$results[[1]]

  output <- tibble::tibble(
    id = rlang::`%||%`(user_data$id, NA_integer_),
    login = rlang::`%||%`(user_data$login, NA_character_),
    name = rlang::`%||%`(user_data$name, NA_character_),
    created_at = lubridate::ymd_hms(
      rlang::`%||%`(user_data$created_at, NA_character_), quiet = TRUE
    ),
    observations_count = rlang::`%||%`(user_data$observations_count, NA_integer_),
    identifications_count = rlang::`%||%`(user_data$identifications_count, NA_integer_),
    species_count = rlang::`%||%`(user_data$species_count, NA_integer_),
    activity_count = rlang::`%||%`(user_data$activity_count, NA_integer_),
    journal_posts_count = rlang::`%||%`(user_data$journal_posts_count, NA_integer_),
    orcid = rlang::`%||%`(user_data$orcid, NA_character_),
    icon_url = rlang::`%||%`(user_data$icon_url, NA_character_),
    site_id = rlang::`%||%`(user_data$site_id, NA_integer_),
    roles = list(rlang::`%||%`(user_data$roles, list())),
    spam = rlang::`%||%`(user_data$spam, NA),
    suspended = rlang::`%||%`(user_data$suspended, NA),
    universal_search_rank = rlang::`%||%`(
      user_data$universal_search_rank, NA_integer_
    )
  )

  return(output)
}
#'
mnk_user_info <- function(id_user) {

  if (missing(id_user) || is.null(id_user)) {
    stop("'id_user' must be provided.", call. = FALSE)
  }
  if (!is.atomic(id_user) || length(id_user)!= 1) {
    stop("'id_user' must be a single character string or number.", call. = FALSE)
  }

  id_for_msg <- as.character(id_user)
  base_url <- "https://api.minka-sdg.org"
  api_path <- paste0("v1/users/", utils::URLencode(id_for_msg, reserved = TRUE))

  response <- tryCatch({
    httr::GET(url = base_url, path = api_path)
  }, error = function(e) {
    message("Network error: Minka API is unavailable. ", e$message)
    return(NULL)
  })

  if (is.null(response)) {
    return(invisible(NULL))
  }

  if (httr::http_error(response)) {
    message("Minka API request failed. Status code: ", httr::status_code(response))
    return(invisible(NULL))
  }

  response_content <- httr::content(response, as = "text", encoding = "UTF-8")
  if (nchar(response_content) == 0 || identical(response_content, "null")) {
    message("API returned an empty or null response for user: ", id_for_msg, ".")
    return(invisible(NULL))
  }

  xx <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
  if (is.null(xx$results) || length(xx$results) == 0) {
    message("No user details found for id_user = ", id_for_msg, ".")
    return(invisible(NULL))
  }
  user_data <- xx$results[[1]]

  output <- tibble::tibble(
    id = rlang::`%||%`(user_data$id, NA_integer_),
    login = rlang::`%||%`(user_data$login, NA_character_),
    name = rlang::`%||%`(user_data$name, NA_character_),
    created_at = as.POSIXct(
      rlang::`%||%`(user_data$created_at, NA_character_),
      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
    ),
    observations_count = rlang::`%||%`(user_data$observations_count, NA_integer_),
    identifications_count = rlang::`%||%`(user_data$identifications_count, NA_integer_),
    species_count = rlang::`%||%`(user_data$species_count, NA_integer_),
    activity_count = rlang::`%||%`(user_data$activity_count, NA_integer_),
    journal_posts_count = rlang::`%||%`(user_data$journal_posts_count, NA_integer_),
    orcid = rlang::`%||%`(user_data$orcid, NA_character_),
    icon_url = rlang::`%||%`(user_data$icon_url, NA_character_),
    site_id = rlang::`%||%`(user_data$site_id, NA_integer_),
    roles = list(rlang::`%||%`(user_data$roles, list())),
    spam = rlang::`%||%`(user_data$spam, NA),
    suspended = rlang::`%||%`(user_data$suspended, NA),
    universal_search_rank = rlang::`%||%`(
      user_data$universal_search_rank, NA_integer_
    )
  )

  return(output)
}
