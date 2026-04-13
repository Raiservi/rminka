#' @title Get Metadata for a Minka Project
#' @description Retrieves detailed information for a specific Minka project
#'   using either its unique \code{project_id} or its group identifier (\code{grpid}).
#' @details You must provide either \code{project_id} or \code{grpid}. If you
#'   do not know the identifier, use \code{\link{mnk_proj_byname}} to find it.
#'   A Minka \code{grpid} or slug is typically the project name formatted with
#'   hyphens (e.g., 'biomarato-barcelona-2025'). You can find it in the URL of
#'   the project's page.
#' @param project_id A single character string or number representing the unique
#'   Minka project identifier.
#' @param grpid A single character string or number representing the group
#'   identifier (slug) for the project.
#' @return A single-row \code{tibble} with project metadata. If the project is
#'   not found, returns an empty tibble with the same column structure.
#'   Columns are:
#' \describe{
#'   \item{id}{Project identifier, integer.}
#'   \item{title}{Project title, character.}
#'   \item{created_at}{Creation timestamp, character in ISO 8601 format.}
#'   \item{subscrib_users}{Total number of subscribed users, integer.}
#'   \item{place_id}{Associated place identifier, integer or \code{NA}.}
#'   \item{slug}{URL slug, character.}
#'   \item{description}{Project description, character.}
#' }
#' @export
#' @examples
#' \dontrun{
#' # Get metadata for project ID 420
#' mnk_proj_info(project_id = 420)
#'
#' # Get metadata using the project slug
#' mnk_proj_info(grpid = "biomarato-barcelona-2025")
#' }
mnk_proj_info <- function(project_id = NULL, grpid = NULL) {

  if (is.null(project_id) && is.null(grpid)) {
    stop("You must provide either 'project_id' or 'grpid'.", call. = FALSE)
  }

  empty_meta <- tibble::tibble(
    id = integer(),
    title = character(),
    created_at = character(),
    subscrib_users = integer(),
    place_id = integer(),
    slug = character(),
    description = character()
  )

  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/projects"
  query_params <- list()
  if (!is.null(project_id)) query_params$id <- as.character(project_id)
  if (!is.null(grpid)) query_params$q <- as.character(grpid)

  response <- tryCatch({
    httr::GET(url = base_url, path = api_path, query = query_params)
  }, error = function(e) {
    message("Network error: Minka API is unavailable.")
    return(NULL)
  })

  if (is.null(response) || httr::http_error(response)) {
    return(empty_meta)
  }

  response_content <- httr::content(response, as = "text", encoding = "UTF-8")
  if (!nzchar(response_content) || response_content == "null") {
    return(empty_meta)
  }

  xx <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
  if (is.null(xx$results) || length(xx$results) == 0) {
    return(empty_meta)
  }

  project_data <- xx$results[[1]]
  user_ids_vec <- unlist(project_data$user_ids %||% list())

  tibble::tibble(
    id = project_data$id %||% NA_integer_,
    title = project_data$title %||% NA_character_,
    created_at = project_data$created_at %||% NA_character_,
    subscrib_users = length(user_ids_vec), # Solo devolvemos la cuenta, no los IDs
    place_id = project_data$place_id %||% NA_integer_,
    slug = project_data$slug %||% NA_character_,
    description = project_data$description %||% NA_character_
  )
}
