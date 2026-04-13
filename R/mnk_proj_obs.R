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
#' @return A tibble with one row per observation. Returns an empty tibble with
#'   zero rows if no observations match the query. Columns are:
#' \describe{
#'   \item{id}{Observation identifier, integer.}
#'   \item{observed_on}{Observation date, character in YYYY-MM-DD format.}
#'   \item{year}{Year of observation, integer.}
#'   \item{month}{Month of observation, integer.}
#'   \item{week}{ISO week number, integer.}
#'   \item{day}{Day of month, integer.}
#'   \item{hour}{Hour of observation, integer.}
#'   \item{created_at}{Record creation timestamp, character in ISO 8601 format.}
#'   \item{updated_at}{Record update timestamp, character in ISO 8601 format.}
#'   \item{latitude}{Decimal latitude, numeric.}
#'   \item{longitude}{Decimal longitude, numeric.}
#'   \item{positional_accuracy}{Coordinate uncertainty in meters, integer.}
#'   \item{geoprivacy}{Geoprivacy flag, logical.}
#'   \item{obscured}{Obscured coordinates flag, logical.}
#'   \item{uri}{Observation url in Minka, character.}
#'   \item{url_picture}{URL of observation image in Minka, character.}
#'   \item{quality_grade}{Data quality grade, character.}
#'   \item{taxon_id}{Taxon identifier, integer.}
#'   \item{taxon_name}{Scientific name, character.}
#'   \item{taxon_rank}{Taxonomic rank, character.}
#'   \item{taxon_min_ancestry}{Minimal ancestry string, character.}
#'   \item{taxon_endemic}{Endemic flag, logical.}
#'   \item{taxon_threatened}{Threatened flag, logical.}
#'   \item{taxon_introduced}{Introduced flag, logical.}
#'   \item{taxon_native}{Native flag, logical.}
#'   \item{user_id}{Observer identifier, integer.}
#'   \item{user_login}{Observer login, character.}
#' }
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
