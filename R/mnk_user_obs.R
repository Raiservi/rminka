#' @title Download User Observations by Year
#' @description Downloads observations for a specific user, filtered by year
#'   and optionally by month and day. This is a convenience wrapper for
#'   \code{\link{mnk_obs}}.
#' @param user_id A user identifier, either numeric ID or character login.
#' @param year The year to query, as a single integer.
#' @param month Optional month to filter, integer from 1 to 12. Defaults to
#'   `NULL`.
#' @param day Optional day to filter, integer from 1 to 31. Defaults to `NULL`.
#' @param quiet Logical; if `TRUE`, suppresses console messages.
#' @param limit_download Logical; if `TRUE` (default), caps each query
#'   subdivision at 10,000 records. If `FALSE`, attempts to retrieve all
#'   records.
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
#' # Download the first 10,000 observations for user 6 (xasalva) in 2024
#' mnk_user_obs(user_id = 6, year = 2024)
#'
#' # Download all observations for August 2024 without record cap
#' mnk_user_obs(user_id = 6, year = 2024, month = 8, limit_download = FALSE)
#' }
#' @export
mnk_user_obs <- function(user_id, year, month = NULL, day = NULL,
                         quiet = FALSE, limit_download = TRUE) {
  mnk_obs(
    user_id = user_id,
    year = year,
    month = month,
    day = day,
    quiet = quiet,
    limit_download = limit_download
  )
}
