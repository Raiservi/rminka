## ===================================================================
# HELPER FUNCTIONS (NOT EXPORTED)
# Defined in dependency order, from least to most dependent.
# ===================================================================

#' Process Raw Minka API Results
#'
#' Processes raw observation results from the Minka API and returns a clean
#' tibble. Handles missing fields gracefully by inserting NA values.
#'
#' @param all_results a list containing the raw observation data from the API.
#' @return a tibble with structured observation data.
#' @noRd
process_minka_results <- function(all_results) {
  if (length(all_results) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(all_results, function(.x) {
    tibble::tibble(
      id =.x$id %||% NA_integer_,
      observed_on =.x$observed_on %||% NA_character_,
      year = purrr::pluck(.x, "observed_on_details", "year",.default = NA_integer_),
      month = purrr::pluck(.x, "observed_on_details", "month",.default = NA_integer_),
      week = purrr::pluck(.x, "observed_on_details", "week",.default = NA_integer_),
      day = purrr::pluck(.x, "observed_on_details", "day",.default = NA_integer_),
      hour = purrr::pluck(.x, "observed_on_details", "hour",.default = NA_integer_),
      created_at =.x$created_at %||% NA_character_,
      updated_at =.x$updated_at %||% NA_character_,
      # coordenadas - pluck evita [[2]] sobre NULL
      latitude = purrr::pluck(.x, "geojson", "coordinates", 2,.default = NA_real_),
      longitude = purrr::pluck(.x, "geojson", "coordinates", 1,.default = NA_real_),
      positional_accuracy =.x$positional_accuracy %||% NA_integer_,
      geoprivacy =.x$taxon_geoprivacy %||% NA_character_,
      obscured =.x$obscured %||% NA,
      uri =.x$uri %||% NA_character_,
      # AQUÍ ESTABA EL ERROR - ahora devuelve NA si no hay fotos
      url_picture = purrr::pluck(.x, "observation_photos", 1, "photo", "url",.default = NA_character_),
      quality_grade =.x$quality_grade %||% NA_character_,
      # taxon puede ser NULL en observaciones casuales
      taxon_id = purrr::pluck(.x, "taxon", "id",.default = NA_integer_),
      taxon_name = purrr::pluck(.x, "taxon", "name",.default = NA_character_),
      taxon_rank = purrr::pluck(.x, "taxon", "rank",.default = NA_character_),
      taxon_min_ancestry = purrr::pluck(.x, "taxon", "min_species_ancestry",.default = NA_character_),
      taxon_endemic = purrr::pluck(.x, "taxon", "endemic",.default = NA),
      taxon_threatened = purrr::pluck(.x, "taxon", "threatened",.default = NA),
      taxon_introduced = purrr::pluck(.x, "taxon", "introduced",.default = NA),
      taxon_native = purrr::pluck(.x, "taxon", "native",.default = NA),
      user_id = purrr::pluck(.x, "user", "id",.default = NA_integer_),
      user_login = purrr::pluck(.x, "user", "login",.default = NA_character_)
    )
  })
}

#' Download Paginated Data from Minka API
#'
#' Downloads paginated data from the Minka API for a given set of query
#' parameters. Fetches data in chunks up to the user-defined limit.
#'
#' @param params a list of query parameters for the API call.
#' @param total_res optional. The total number of results to expect, to avoid an
#' initial ping.
#' @param quiet a logical value. If TRUE, suppresses all messages.
#' @param numeric_limit the numeric maximum number of records to download for
#' this specific call.
#' @return a list containing `$data` (a tibble) and `$count` (number of rows).
#' @noRd
download_paginated_data <- function(params, total_res = NULL, quiet = FALSE, numeric_limit = 10000) {
  API_MAX_PER_PAGE <- 200

  if (is.null(total_res)) {
    ping_params <- c(params, list(per_page = 1))
    ping_response <- httr::GET("https://api.minka-sdg.org/v1/observations", query = ping_params)
    if (httr::http_error(ping_response)) {
      if (!quiet) message(paste("The PING query failed with code:", httr::status_code(ping_response)))
      return(list(data = tibble::tibble(), count = 0))
    }
    total_res <- httr::content(ping_response, as = "parsed")$total_results
  }

  if (is.null(total_res) || total_res == 0) {
    return(list(data = tibble::tibble(), count = 0))
  }

  max_to_fetch <- min(total_res, numeric_limit)

  if (total_res > numeric_limit && is.finite(numeric_limit) &&!quiet) {
    message(paste0("NOTE: Fetching only the first ", format(max_to_fetch, big.mark = ","), " of ", format(total_res, big.mark = ","), " available records due to limit."))
  }

  all_results <- list()
  if (max_to_fetch > 0) {
    for (i in 1:ceiling(max_to_fetch / API_MAX_PER_PAGE)) {
      if (length(all_results) >= max_to_fetch) break

      page_params <- c(params, list(per_page = API_MAX_PER_PAGE, page = i))
      data_response <- httr::GET("https://api.minka-sdg.org/v1/observations", query = page_params)

      if (httr::http_error(data_response)) {
        if (!quiet) message(paste("Error on page", i, "- skipping."))
        next
      }

      data_content <- httr::content(data_response, as = "parsed")$results
      if (length(data_content) > 0) {
        all_results <- c(all_results, data_content)
      } else {
        break
      }
    }
  }

  if(length(all_results) > max_to_fetch){
    all_results <- all_results[1:max_to_fetch]
  }

  processed_data <- process_minka_results(all_results)
  return(list(data = processed_data, count = nrow(processed_data)))
}

#' Download Data for a Specific Month with Subdivision Logic
#'
#' Downloads data for a given month, respecting the remaining download limit.
#' Subdivides by day if monthly total exceeds the API limit.
#'
#' @param base_params a list of base query parameters.
#' @param year the year to download.
#' @param current_month the month to download.
#' @param quiet a logical value. If TRUE, suppresses messages.
#' @param remaining_limit the maximum number of records to download in this and
#' subsequent calls.
#' @return a list containing `$data` (a tibble) and `$count` (number of rows).
#' @noRd
download_month_data <- function(base_params, year, current_month, quiet = FALSE, remaining_limit) {
  if (remaining_limit <= 0) {
    return(list(data = tibble::tibble(), count = 0))
  }

  month_name <- month.name[current_month]
  if (!quiet) message(paste0("\n--- Evaluating month: ", month_name, " ", year, " ---"))

  monthly_ping_params <- c(base_params, list(year = year, month = current_month, per_page = 1))
  ping_response <- httr::GET("https://api.minka-sdg.org/v1/observations", query = monthly_ping_params)

  if (httr::http_error(ping_response)) {
    if (!quiet) message(paste("The PING query failed for the month of", month_name))
    return(list(data = tibble::tibble(), count = 0))
  }

  monthly_total <- httr::content(ping_response, as = "parsed")$total_results

  if (is.null(monthly_total) || monthly_total == 0) {
    if (!quiet) message(paste("No records were found for", month_name, year))
    return(list(data = tibble::tibble(), count = 0))
  }

  if (!quiet) message(paste("The month of", month_name, "has", monthly_total, "records."))

  if (monthly_total <= 10000) {
    if (!quiet) message(" -> Total <= 10,000. Downloading month in one go...")
    params_for_month <- c(base_params, list(year = year, month = current_month))
    return(download_paginated_data(params = params_for_month, total_res = monthly_total, quiet = quiet, numeric_limit = remaining_limit))
  } else {
    if (!quiet) message(" -> Total > 10,000. Subdividing by DAY to respect API limit...")
    days_in_month <- as.numeric(format(seq(as.Date(paste(year, current_month, 1, sep = "-")), by = "month", length.out = 2)[2] - 1, "%d"))

    all_day_data <- list()
    total_downloaded_this_month <- 0

    for(current_day in 1:days_in_month) {
      day_remaining_limit <- remaining_limit - total_downloaded_this_month
      if (day_remaining_limit <= 0) break

      if (!quiet) message(paste(" - Downloading day:", current_day))
      daily_params <- c(base_params, list(year = year, month = current_month, day = current_day))

      day_result <- download_paginated_data(params = daily_params, quiet = quiet, numeric_limit = day_remaining_limit)

      if (day_result$count > 0) {
        all_day_data[[length(all_day_data) + 1]] <- day_result$data
        total_downloaded_this_month <- total_downloaded_this_month + day_result$count
      }
    }

    combined_data <- dplyr::bind_rows(all_day_data)
    return(list(data = combined_data, count = nrow(combined_data)))
  }
}

## ===================================================================

# MAIN FUNCTION

## ===================================================================

#' Download Minka Observations
#'
#' Downloads observation data from the Minka API. Handles pagination and
#' rate limits automatically by subdividing large queries by month or day.
#'
#' @param query a generic query string for the 'q' parameter.
#' @param taxon_name a character string with the taxon name (common or
#'   scientific).
#' @param taxon_id a numeric ID for the taxon.
#' @param user_id a numeric ID for a specific user.
#' @param project_id a numeric ID for a specific project.
#' @param place_id a numeric ID for a specific place.
#' @param endemic a logical value. Filters for endemic species.
#' @param introduced a logical value. Filters for introduced species.
#' @param threatened a logical value. Filters for threatened species.
#' @param quality a character string. Must be 'casual' or 'research'.
#' @param geo a logical value. If TRUE, filters for observations with coordinates.
#' @param annotation a numeric vector of length 2 (term_id, term_value_id).
#' @param year a numeric value for the year.
#' @param month a numeric value for the month (1-12).
#' @param day a numeric value for the day (1-31).
#' @param bounds a bounding box. Accepts an sf object with CRS EPSG:4326
#'   (WGS84) or a numeric vector c(nelat, nelng, swlat, swlng).
#' @param quiet a logical value. If TRUE, suppresses console messages.
#' @param limit_download a logical value. If TRUE (default), caps download at
#'   10,000 records.
#' @return a tibble with one row per observation and the following columns:
#'\describe{
#'   \item{id}{observation identifier, integer.}
#'   \item{observed_on}{observation date, character.}
#'   \item{year}{year component, integer.}
#'   \item{month}{month component, integer.}
#'   \item{week}{week component, integer.}
#'   \item{day}{day component, integer.}
#'   \item{hour}{hour component, integer.}
#'   \item{created_at}{creation timestamp, character.}
#'   \item{updated_at}{last update timestamp, character.}
#'   \item{latitude}{latitude in WGS84, numeric.}
#'   \item{longitude}{longitude in WGS84, numeric.}
#'   \item{positional_accuracy}{coordinate uncertainty in meters, integer.}
#'   \item{geoprivacy}{geoprivacy setting, character.}
#'   \item{obscured}{flag for obscured coordinates, logical.}
#'   \item{uri}{API resource URI, character.}
#'   \item{url_picture}{URL of first photo, character.}
#'   \item{quality_grade}{quality grade, character.}
#'   \item{taxon_id}{taxon identifier, integer.}
#'   \item{taxon_name}{scientific name, character.}
#'   \item{taxon_rank}{taxonomic rank, character.}
#'   \item{taxon_min_ancestry}{lowest species ancestry, character.}
#'   \item{taxon_endemic}{endemic flag, logical.}
#'   \item{taxon_threatened}{threatened flag, logical.}
#'   \item{taxon_introduced}{introduced flag, logical.}
#'   \item{taxon_native}{native flag, logical.}
#'   \item{user_id}{observer identifier, integer.}
#'   \item{user_login}{observer login, character.}
#' }
#' Returns an empty tibble if no data is found.
#' @export
#' @examples
#' \dontrun{
#' # Download firts 10.000 observations of taxon  from a project in 2025
#' obs <- mnk_obs(project_id = 417, year = 2025,
#' taxon_name = "Diplodus sargus" )
#'
#' # Download all records in 2024 from a user using bounds
#' barcelona <- c(41.5, 2.3, 41.2, 2.0)
#' obs_bc <- mnk_obs( year=2024, user_login = "xasalvador",
#' bounds = barcelona, quiet = TRUE, limit_dowload = FALSE)
#' }
mnk_obs <- function(query = NULL, taxon_name = NULL, taxon_id = NULL,
                    user_id = NULL, project_id = NULL, place_id = NULL,
                    endemic = NULL, introduced = NULL, threatened = NULL,
                    quality = NULL, geo = NULL, annotation = NULL,
                    year = NULL, month = NULL, day = NULL, bounds = NULL,
                    quiet = FALSE, limit_download = TRUE) {

  arg_list <- list(query = query, taxon_name = taxon_name, taxon_id = taxon_id,
                   user_id = user_id, project_id = project_id, place_id = place_id,
                   endemic = endemic, introduced = introduced, threatened = threatened,
                   quality = quality, geo = geo, annotation = annotation,
                   year = year, month = month, day = day, bounds = bounds)

  if (all(sapply(arg_list, is.null))) {
    stop("You must specify at least one search parameter (e.g., taxon_name, year, project_id).")
  }
  if (!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE.")
  if (!is.logical(limit_download)) stop("'limit_download' must be TRUE or FALSE.")

  download_limit <- if (limit_download) 10000 else Inf

  base_params <- list()

  if (!is.null(taxon_name)) base_params$taxon_name <- taxon_name
  if (!is.null(query)) base_params$q <- query
  if (!is.null(quality)) {
    if (!quality %in% c("casual", "research")) {
      stop("The 'quality' parameter must be 'casual' or 'research'.")
    }
    base_params$quality_grade <- quality
  }
  if (!is.null(taxon_id)) base_params$taxon_id <- taxon_id
  if (!is.null(user_id)) base_params$user_id <- user_id
  if (!is.null(project_id)) base_params$project_id <- project_id
  if (!is.null(place_id)) base_params$place_id <- place_id
  if (!is.null(geo) && geo) base_params$`has[]` <- "geo"

  if (!is.null(endemic)) {
    if (!is.logical(endemic)) stop("The 'endemic' parameter must be TRUE or FALSE.")
    base_params$endemic <- tolower(as.character(endemic))
  }
  if (!is.null(introduced)) {
    if (!is.logical(introduced)) stop("The 'introduced' parameter must be TRUE or FALSE.")
    base_params$introduced <- tolower(as.character(introduced))
  }
  if (!is.null(threatened)) {
    if (!is.logical(threatened)) stop("The 'threatened' parameter must be TRUE or FALSE.")
    base_params$threatened <- tolower(as.character(threatened))
  }

  if (!is.null(annotation)) {
    if(length(annotation)!= 2 ||!is.numeric(annotation)){
      stop("The 'annotation' parameter must be a numeric vector of length 2 (term_id, term_value_id).")
    }
    base_params$term_id <- annotation[1]
    base_params$term_value_id <- annotation[2]
  }

  if (!is.null(bounds)) {
    if (inherits(bounds, c("sf", "sfc"))) {
      crs <- sf::st_crs(bounds)
      if (is.na(crs) || crs$epsg!= 4326) {
        stop("bounds must have CRS EPSG:4326 (WGS84). ",
             "Use sf::st_transform(bounds, 4326) first.", call. = FALSE)
      }
      bbox <- sf::st_bbox(bounds)
      processed_bounds <- list(
        swlng = as.numeric(bbox[["xmin"]]),
        swlat = as.numeric(bbox[["ymin"]]),
        nelng = as.numeric(bbox[["xmax"]]),
        nelat = as.numeric(bbox[["ymax"]])
      )
    } else {
      if (!is.numeric(bounds) || length(bounds)!= 4) {
        stop("'bounds' must be a numeric vector of length 4: c(nelat, nelng, swlat, swlng)")
      }
      processed_bounds <- list(
        nelat = bounds[1],
        nelng = bounds[2],
        swlat = bounds[3],
        swlng = bounds[4]
      )
    }
    base_params <- c(base_params, processed_bounds)
  }

  final_data <- NULL

  if (!is.null(year) &&!is.null(month) &&!is.null(day)) {
    if (!quiet) message(paste0("--- STARTING DOWNLOAD FOR DAY: ", year, "-", month, "-", day, " ---"))
    day_params <- c(base_params, list(year = year, month = month, day = day))
    final_data <- download_paginated_data(params = day_params, quiet = quiet, numeric_limit = download_limit)$data

  } else if (!is.null(year) &&!is.null(month)) {
    if (!quiet) message(paste0("--- STARTING DOWNLOAD FOR MONTH: ", month.name[month], " ", year, " ---"))
    final_data <- download_month_data(base_params = base_params, year = year, current_month = month, quiet = quiet, remaining_limit = download_limit)$data

  } else if (!is.null(year)) {
    if (!quiet) message(paste0("--- STARTING ANNUAL DOWNLOAD FOR THE YEAR ", year, " ---"))

    all_year_data <- list()
    remaining_limit <- download_limit

    for(current_month in 1:12) {
      if(remaining_limit <= 0) {
        if(!quiet && is.finite(download_limit)) message("Download limit reached. Stopping.")
        break
      }

      month_result <- download_month_data(
        base_params = base_params,
        year = year,
        current_month = current_month,
        quiet = quiet,
        remaining_limit = remaining_limit
      )

      if(month_result$count > 0){
        all_year_data[[length(all_year_data) + 1]] <- month_result$data
        remaining_limit <- remaining_limit - month_result$count
      }
    }
    final_data <- dplyr::bind_rows(all_year_data)

  } else {
    if (!quiet) message("--- STARTING DOWNLOAD WITH NO DATE FILTER ---")
    final_data <- download_paginated_data(params = base_params, quiet = quiet, numeric_limit = download_limit)$data
  }

  if (!quiet) message("\n--- FINISHING... ---")

  if (is.null(final_data) || nrow(final_data) == 0) {
    if (!quiet) message("No data could be downloaded for the specified criteria.")
    return(tibble::tibble())
  }

  if (is.finite(download_limit) && nrow(final_data) > download_limit) {
    final_data <- final_data[1:download_limit, ]
  }

  if (!quiet) message(paste0("Download complete! A total of ", format(nrow(final_data), big.mark = ","), " records were obtained."))
  return(final_data)
}
