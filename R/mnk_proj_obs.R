##' @title Download Project Observations by Year
#' @description Downloads observations for a specific Minka project, filtered by
#'   year and optionally by month and day. This is a convenience wrapper around
#'   \code{\link{mnk_obs}}.
#' @param project_id a single numeric project identifier.
#' @param year a single numeric year (e.g., 2024).
#' @param month optional numeric month (1-12). Defaults to NULL for all months.
#' @param day optional numeric day (1-31). Defaults to NULL for all days.
#' @param quiet logical. If TRUE, suppresses console messages. Defaults to FALSE.
#' @param limit_download logical. If TRUE (default), caps download at 10,000
#'   records per query subdivision. If FALSE, attempts to download all records.
#' @return A tibble containing observation data as returned by
#'   \code{\link{mnk_obs}}. See that function for column details.
#' @examples
#' \dontrun{
#' # Download all observations for project 419 for the year 2024
#' proj_data_2024 <- mnk_proj_obs(project_id = 419, year = 2024)
#'
#' # Download observations for project 419 for August 2025, without limit
#' proj_data_aug_2025 <- mnk_proj_obs(
#'   project_id = 419,
#'   year = 2025,
#'   month = 8,
#'   limit_download = FALSE
#' )
#' }
#' @export
mnk_proj_obs <- function(project_id, year, month = NULL, day = NULL,
                         quiet = FALSE, limit_download = TRUE) {

  mnk_obs(
    project_id = project_id,
    year = year,
    month = month,
    day = day,
    quiet = quiet,
    limit_download = limit_download
  )
}
