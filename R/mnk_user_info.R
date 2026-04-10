#' Get Information About a Specific Minka User
#'
#' @description
#' Retrieves public profile information for a Minka user from the Minka API.
#'
#' @details
#' The function queries the \code{/v1/users/{id}} endpoint of the Minka API
#' (\url{https://api.minka-sdg.org}). The \code{id_user} corresponds to the
#' numeric identifier visible in Minka profile URLs (e.g.,
#' \url{https://minka-sdg.org/users/6}).
#'
#' @param id_user A single atomic value (numeric or character) representing the
#' Minka user ID.
#'
#' @return
#' A named list containing key details about the user:
#' \code{id}, \code{login}, \code{name}, \code{created_at},
#' \code{observations_count}, \code{identifications_count},
#' \code{species_count}, \code{activity_count}, \code{journal_posts_count},
#' \code{orcid}, \code{icon_url}, \code{site_id}, \code{roles}, \code{spam},
#' \code{suspended}, \code{universal_search_rank}.
#'
#' If the user is not found or a network error occurs, the function returns
#' \code{NULL} invisibly and emits a message.
#'
#' @importFrom httr GET http_error status_code content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' # Get information for user ID 6
#' mnk_user_info(6)
#'
#' # Using a character ID
#' mnk_user_info("6")
#' }
#' @export
mnk_user_info <- function(id_user) {

  # Validate input
  if (missing(id_user) || is.null(id_user)) {
    stop("'id_user' must be provided.", call. = FALSE)
  }
  if (!is.atomic(id_user) || length(id_user)!= 1) {
    stop("'id_user' must be a single character string or number.", call. = FALSE)
  }

  id_for_msg <- as.character(id_user)
  base_url <- "https://api.minka-sdg.org"
  api_path <- paste0("v1/users/", utils::URLencode(id_for_msg, reserved = TRUE))

  # Request
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
    status <- httr::status_code(response)
    message("Minka API request failed. Status code: ", status)
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

  get_safe_value <- function(obj, field, default = NA) {
    val <- obj[[field]]
    if (is.null(val)) default else val
  }

  output <- list()
  output[["id"]] <- get_safe_value(user_data, "id")
  output[["login"]] <- get_safe_value(user_data, "login", NA_character_)
  output[["name"]] <- get_safe_value(user_data, "name", NA_character_)
  output[["created_at"]] <- get_safe_value(user_data, "created_at", NA_character_)
  output[["observations_count"]] <- get_safe_value(user_data, "observations_count", NA_integer_)
  output[["identifications_count"]] <- get_safe_value(user_data, "identifications_count", NA_integer_)
  output[["species_count"]] <- get_safe_value(user_data, "species_count", NA_integer_)
  output[["activity_count"]] <- get_safe_value(user_data, "activity_count", NA_integer_)
  output[["journal_posts_count"]] <- get_safe_value(user_data, "journal_posts_count", NA_integer_)
  output[["orcid"]] <- get_safe_value(user_data, "orcid", NA_character_)
  output[["icon_url"]] <- get_safe_value(user_data, "icon_url", NA_character_)
  output[["site_id"]] <- get_safe_value(user_data, "site_id", NA_integer_)
  output[["roles"]] <- get_safe_value(user_data, "roles", list())
  output[["spam"]] <- get_safe_value(user_data, "spam", NA)
  output[["suspended"]] <- get_safe_value(user_data, "suspended", NA)
  output[["universal_search_rank"]] <- get_safe_value(user_data, "universal_search_rank", NA_integer_)

  return(output)
}
