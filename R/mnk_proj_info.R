#' Get Information About a Specific Minka Project
#'
#' @description
#' This function downloads information for a specific Minka project using either
#' its unique `project_id` or its group identifier (`grpid`).
#'
#' @details
#' You must provide either a `project_id` or a `grpid`. If you don't know the
#' ID, you can use the `mnk_proj_byname()` function to find it.
#'
#' A Minka `grpid` or slug is typically the project name formatted as a single
#' string with words separated by hyphens (e.g., 'biomarato-barcelona-2025').
#' You can find it in the URL of the project's page on the Minka website.
#'
#' @param project_id A single character string or number representing the unique
#' Minka project ID.
#' @param grpid The group identifier (slug) or ID for the project.
#' @param users A logical value (`TRUE` or `FALSE`). If `TRUE`, the function
#' returns a tibble containing only the IDs of users subscribed to the project.
#' If `FALSE` (the default), it returns a list with the main project information.
#'
#' @return
#' By default, a list containing key details about the project.
#' If `users = TRUE`, it returns a `tibble` with a single column `id_users`
#' containing the numeric IDs of subscribed users.
#' If the project is not found or a network error occurs, it returns `NULL`
#' and prints a message.
#'
#' @importFrom tibble tibble
#' @importFrom httr GET http_error status_code content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' # First, find a project ID
#' mnk_proj_byname("Biomarato Barcelona 2025")
#'
#' # Get main information for project ID '420'
#' mnk_proj_info(project_id = "420")
#'
#' # Get only the subscriber IDs for the same project
#' mnk_proj_info(project_id = "420", users = TRUE)
#' }
#' @export
#'
mnk_proj_info <- function(project_id = NULL, grpid = NULL, users = FALSE) {

  if (is.null(project_id) && is.null(grpid)) {
    stop("You must provide either 'project_id' or 'grpid'. Both cannot be NULL.")
  }

  if (!is.null(project_id) && (!is.atomic(project_id) || length(project_id)!= 1)) {
    stop("'project_id' must be a single character string or number.")
  }
  if (!is.null(grpid) && (!is.atomic(grpid) || length(grpid)!= 1)) {
    stop("'grpid' must be a single character string or number.")
  }

  id_for_msg <- if (!is.null(project_id)) as.character(project_id) else as.character(grpid)
  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/projects"
  query_params <- list()
  if (!is.null(project_id)) query_params$id <- as.character(project_id)
  if (!is.null(grpid)) query_params$q <- as.character(grpid)

  response <- tryCatch({
    httr::GET(url = base_url, path = api_path, query = query_params)
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
  if (nchar(response_content) == 0 || response_content == "null") {
    message("API returned an empty or null response for project: ", id_for_msg, ".")
    return(invisible(NULL))
  }

  xx <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
  if (is.null(xx$results) || length(xx$results) == 0) {
    message("No project details found for id_project = ", id_for_msg, ".")
    return(invisible(NULL))
  }
  project_data <- xx$results[[1]]

  get_safe_value <- function(obj, field, default = NA) {
    val <- obj[[field]]
    if (is.null(val)) default else val
  }

  user_ids_vec <- unlist(get_safe_value(project_data, "user_ids", list()))

  if (users == TRUE) {
    if (is.null(user_ids_vec) || length(user_ids_vec) == 0) {
      return(tibble::tibble(id_users = integer(0)))
    } else {
      return(tibble::tibble(id_users = user_ids_vec))
    }
  }

  output <- list()
  output[["id"]] <- get_safe_value(project_data, "id")
  output[["title"]] <- get_safe_value(project_data, "title", NA_character_)
  output[["created_at"]] <- get_safe_value(project_data, "created_at", NA_character_)
  output[["subscrib_users"]] <- length(user_ids_vec)
  output[["place_id"]] <- get_safe_value(project_data, "place_id", NA_integer_)
  output[["slug"]] <- get_safe_value(project_data, "slug", NA_character_)
  output[["description"]] <- get_safe_value(project_data, "description", NA_character_)

  return(output)
}
