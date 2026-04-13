#' @title
#' Get Project Users With Metadata
#' @description
#' Queries the Minka API for a given project identifier, extracts the
#' associated user identifiers, and returns a tibble with full metadata
#' for each participant by calling \code{mnk_user_info()}.
#' The function is type-stable: it always returns a tibble with the same
#' sixteen columns, even when the project does not exist or the API
#' request fails. Individual user lookup failures produce a row of
#' \code{NA} values and do not interrupt the overall query.
#' @param project_id A single atomic character or numeric value giving
#' the project identifier. Vectors of length greater than one are not
#' accepted.
#' @return A tibble with one row per participant and sixteen columns:
#' \describe{
#' \item{id}{Integer. User identifier.}
#' \item{login}{Character. Username.}
#' \item{name}{Character. Full name.}
#' \item{created_at}{POSIXct. Account creation time.}
#' \item{observations_count}{Integer. Number of observations.}
#' \item{identifications_count}{Integer. Number of identifications.}
#' \item{species_count}{Integer. Number of observed species.}
#' \item{activity_count}{Integer. Total activity count.}
#' \item{journal_posts_count}{Integer. Number of journal posts.}
#' \item{orcid}{Character. ORCID identifier.}
#' \item{icon_url}{Character. URL of profile icon.}
#' \item{site_id}{Integer. Site identifier.}
#' \item{roles}{List. User roles.}
#' \item{spam}{Logical. Spam flag.}
#' \item{suspended}{Logical. Suspension flag.}
#' \item{universal_search_rank}{Integer. Search rank.}
#' }
#' @details
#' The function performs a GET request to
#' \code{https://api.minka-sdg.org/v1/projects?id={project_id}}, extracts
#' \code{results[[1]]$user_ids}, and applies
#' \code{purrr::map_dfr()} over \code{mnk_user_info()}.
#' @seealso \code{\link{mnk_user_info}}, \code{\link{mnk_proj_info}}
#' @examples
#' \dontrun{
#' users <- mnk_proj_user(420)
#' dplyr::glimpse(users)
#' }
#' @export
mnk_proj_user <- function(project_id = NULL) {
  #... function code...
}

mnk_proj_user <- function(project_id = NULL) {

  if (is.null(project_id)) stop("You must provide 'project_id'.", call. = FALSE)
  if (!is.atomic(project_id) || length(project_id)!= 1) {
    stop("'project_id' must be a single character string or number.", call. = FALSE)
  }

  empty_users <- tibble::tibble(
    id = integer(),
    login = character(),
    name = character(),
    created_at = as.POSIXct(character()),
    observations_count = integer(),
    identifications_count = integer(),
    species_count = integer(),
    activity_count = integer(),
    journal_posts_count = integer(),
    orcid = character(),
    icon_url = character(),
    site_id = integer(),
    roles = list(),
    spam = logical(),
    suspended = logical(),
    universal_search_rank = integer()
  )

  resp <- tryCatch(
    httr::GET("https://api.minka-sdg.org", path = "v1/projects",
              query = list(id = as.character(project_id))),
    error = function(e) { message("Network error: ", e$message); NULL }
  )

  if (is.null(resp) || httr::http_error(resp)) {
    if (!is.null(resp)) message("Status: ", httr::status_code(resp))
    return(empty_users)
  }

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(txt) || txt == "null") return(empty_users)

  js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (is.null(js$results) || length(js$results) == 0) return(empty_users)

  ids <- unlist(js$results[[1]]$user_ids %||% list())
  if (length(ids) == 0) return(empty_users)

  # aquí está la magia: por cada id llamamos a tu mnk_user_info
  purrr::map_dfr(as.integer(ids), mnk_user_info)
}
