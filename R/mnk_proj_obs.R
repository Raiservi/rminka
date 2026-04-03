#' Download Project Observations by Year
#'
#' This is a convenience wrapper for \code{\link{mnk_obs}} to simplify downloading
#' observations for a specific project, filtered by year and optionally by month and day.
#'
#' @param project_id The numeric ID of the Minka project.
#' @param year The numeric year for the query (required).
#' @param month (Optional) The numeric month (1-12). Defaults to NULL (all months).
#' @param day (Optional) The numeric day (1-31). Defaults to NULL (all days).
#' @param quiet A logical value. If `TRUE`, all console messages will be suppressed.
#' @param limit_download A logical value. If `TRUE` (default), the download is capped
#'   at 10,000 records per query subdivision. If `FALSE`, it attempts to download all records.
#'
#' @return A `tibble::tibble` containing the downloaded observation data.
#' @export
#' @examples
#' \dontrun{
#' # Download all observations for project 419 for the year 2024
#' # (up to the download limit)
#' proj_data_2024 <- mnk_proj_obs(project_id = 419, year = 2024)
#'
#' # Download all observations for project 419 for August 2025,
#' # attempting to get all records without a limit.
#' proj_data_aug_2025 <- mnk_proj_obs(project_id = 419, year = 2025, month = 8, limit_download = FALSE)
#' }
mnk_proj_obs <- function(project_id, year, month = NULL, day = NULL, quiet = FALSE, limit_download = TRUE) {

    mnk_obs(
    project_id = project_id,
    year = year,
    month = month,
    day = day,
    quiet = quiet,
    limit_download = limit_download
  )

}
