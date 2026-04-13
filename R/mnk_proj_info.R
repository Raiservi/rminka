#' @title Get Information About a Specific Minka Project
#' @description Retrieves information for a specific Minka project using either
#' its unique `project_id` or its group identifier (`grpid`).
#' @details You must provide either `project_id` or `grpid`. If you do not know
#' the identifier, use \code{\link{mnk_proj_byname}} to find it.
#'
#' A Minka `grpid` or slug is typically the project name formatted with
#' hyphens (e.g., 'biomarato-barcelona-2025'). You can find it in the URL of
#' the project's page.
#' @param project_id a single character string or number representing the unique
#' Minka project identifier.
#' @param grpid a single character string or number representing the group
#' identifier (slug) for the project.
#' @param users logical. If TRUE, returns a tibble with subscribed user IDs. If
#' FALSE (default), returns a list with project details.
#' @return If `users = FALSE` (default), a list with project metadata. If
#' `users = TRUE`, a tibble with one column `id_users`. Returns `NULL`
#' invisibly if the project is not found or on network error. List elements
#' are:
#' \describe{
#' \item{id}{Project identifier, integer.}
#' \item{title}{Project title, character.}
#' \item{created_at}{Creation timestamp, character in ISO 8601 format.}
#' \item{subscrib_users}{Number of subscribed users, integer.}
#' \item{place_id}{Associated place identifier, integer or `NA`.}
#' \item{slug}{URL slug, character.}
#' \item{description}{Project description, character.}
#' }
#' @examples
#' \dontrun{
#' # First find the project ID
#' mnk_proj_byname("Biomarato Barcelona 2025")
#'
#' # Get main information for project ID 420
#' mnk_proj_info(project_id = "420")
#'
#' # Get only subscriber IDs
#' mnk_proj_info(project_id = "420", users = TRUE)
#' }
#' @export
mnk_proj_info <- function(project_id = NULL, grpid = NULL, users = FALSE) {

  if (is.null(project_id) && is.null(grpid)) {
    stop("You must provide either 'project_id' or 'grpid'. Both cannot be NULL.",
         call. = FALSE)
  }

  if (!is.null(project_id) && (!is.atomic(project_id) || length(project_id)!= 1)) {
    stop("'project_id' must be a single character string or number.", call. = FALSE)
  }
  if (!is.null(grpid) && (!is.atomic(grpid) || length(grpid)!= 1)) {
    stop("'grpid' must be a single character string or number.", call. = FALSE)
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

  response_content <- httr::content(response, as = "text", encoding = "UTF-8",
                                    type = "application/json")
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

  user_ids_vec <- unlist(rlang::`%||%`(project_data$user_ids, list()))

  if (isTRUE(users)) {
    if (length(user_ids_vec) == 0) {
      return(tibble::tibble(id_users = integer(0)))
    } else {
      return(tibble::tibble(id_users = user_ids_vec))
    }
  }

  tibble::tibble(
    id = project_data$id %||% NA_integer_,
    title = project_data$title %||% NA_character_,
    created_at = project_data$created_at %||% NA_character_,
    subscrib_users = length(user_ids_vec),
    place_id = project_data$place_id %||% NA_integer_,
    slug = project_data$slug %||% NA_character_,
    description = project_data$description %||% NA_character_
  )

  return(output)
}
