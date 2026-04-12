
## ===================================================================
# HELPER FUNCTIONS (NOT EXPORTED)
# Defined in dependency order, from least to most dependent.
# ===================================================================

# Helper 1
#' @noRd
byday_get_total_results <- function(p) {
  resp <- httr::GET("https://api.minka-sdg.org/v1/observations",
                    query = c(p, list(per_page = 1)))
  if (httr::http_error(resp)) return(0)
  total <- httr::content(resp, as = "parsed")$total_results
  rlang::`%||%`(total, 0)
}

# Helper 2
#' @noRd
byday_process_results <- function(all_results) {
  if (length(all_results) == 0) return(tibble::tibble())

  processed <- purrr::map(all_results, ~tibble::tibble(
    id = rlang::`%||%`(.x$id, NA_integer_),
    observed_on = rlang::`%||%`(.x$observed_on, NA),
    year = rlang::`%||%`(.x$observed_on_details$year, NA_integer_),
    month = rlang::`%||%`(.x$observed_on_details$month, NA_integer_),
    week = rlang::`%||%`(.x$observed_on_details$week, NA_integer_),
    day = rlang::`%||%`(.x$observed_on_details$day, NA_integer_),
    hour = rlang::`%||%`(.x$observed_on_details$hour, NA_integer_),
    created_at = rlang::`%||%`(.x$created_at, NA),
    updated_at = rlang::`%||%`(.x$updated_at, NA),
    latitude = rlang::`%||%`(.x$geojson$coordinates[[2]], NA_real_),
    longitude = rlang::`%||%`(.x$geojson$coordinates[[1]], NA_real_),
    positional_accuracy = rlang::`%||%`(.x$positional_accuracy, NA_integer_),
    geoprivacy = rlang::`%||%`(.x$taxon_geoprivacy, NA),
    obscured = rlang::`%||%`(.x$obscured, NA),
    uri = rlang::`%||%`(.x$uri, NA),
    photo_url_square = rlang::`%||%`(.x$taxon$default_photo$square_url, NA_character_),
    photo_url_medium = rlang::`%||%`(.x$taxon$default_photo$medium_url, NA_character_),
    quality_grade = rlang::`%||%`(.x$quality_grade, NA),
    species_guess = rlang::`%||%`(.x$species_guess, NA),
    taxon_id = rlang::`%||%`(.x$taxon$id, NA_integer_),
    taxon_name = rlang::`%||%`(.x$taxon$name, NA),
    taxon_rank = rlang::`%||%`(.x$taxon$rank, NA),
    taxon_min_ancestry = rlang::`%||%`(.x$taxon$min_species_ancestry, NA),
    taxon_endemic = rlang::`%||%`(.x$taxon$endemic, NA),
    taxon_threatened = rlang::`%||%`(.x$taxon$threatened, NA),
    taxon_introduced = rlang::`%||%`(.x$taxon$introduced, NA),
    taxon_native = rlang::`%||%`(.x$taxon$native, NA),
    user_id = rlang::`%||%`(.x$user$id, NA_integer_),
    user_login = rlang::`%||%`(.x$user$login, NA)
  ))
  dplyr::bind_rows(processed)
}

# Helper 3
#' @noRd
byday_download_chunk <- function(params, total_res, quiet, limit_download) {
  API_MAX_PER_PAGE <- 200
  download_limit <- if (limit_download) 10000 else Inf
  max_to_fetch <- min(total_res, download_limit)

  all_results <- list()
  if (max_to_fetch > 0) {
    pages <- 1:ceiling(max_to_fetch / API_MAX_PER_PAGE)
    for (i in pages) {
      page_params <- c(params, list(per_page = API_MAX_PER_PAGE, page = i))
      data_response <- httr::GET("https://api.minka-sdg.org/v1/observations",
                                 query = page_params)
      if (httr::http_error(data_response)) next
      data_content <- httr::content(data_response, as = "parsed")$results
      if (!is.null(data_content) && length(data_content) > 0) {
        all_results <- c(all_results, data_content)
      } else {
        break
      }
    }
  }
  byday_process_results(all_results)
}

## ===================================================================

# MAIN FUNCTION

## ===================================================================
#' Download Minka Observations by Date Range
#'
#' Downloads observation data from the Minka API for a specified date range.
#' Subdivides requests by month or day automatically to avoid the 10,000
#' record API limit.
#'
#' @param d1 start date in 'yyyy-mm-dd' format.
#' @param d2 end date in 'yyyy-mm-dd' format.
#' @param ... additional arguments passed to the API. See [mnk_obs()] for
#'   details on available parameters.
#' @param quiet a logical value. If TRUE, suppresses console messages.
#' @param limit_download a logical value. If TRUE (default), each subdivided
#'   request is capped at 10,000 records.
#' @inheritDotParams mnk_obs -year -month -day -quiet -limit_download
#' @return a tibble with one row per observation and the same columns
#'   documented in \code{\link{mnk_obs}}. Returns an empty tibble if no
#'   data is found.
#' @export
#' @examples
#' \dontrun{
#' # Download observations between two dates
#' obs <- mnk_obs_byday("2024-03-01", "2024-03-31",
#' taxon_name = "Diplodus sargus")
#'
#' # Use with bounds (must be EPSG:4326)
#' barcelona <- c(41.5, 2.3, 41.2, 2.0)
#' obs_bc <- mnk_obs_byday("2024-01-01", "2024-01-07",
#' bounds = barcelona, quiet = TRUE)
#' }
mnk_obs_byday <- function(d1, d2,..., quiet = FALSE, limit_download = TRUE) {
  date1 <- as.Date(d1, format = "%Y-%m-%d")
  date2 <- as.Date(d2, format = "%Y-%m-%d")
  if (is.na(date1) || is.na(date2)) {
    stop("Dates d1 and d2 must be in 'yyyy-mm-dd' format.")
  }
  if (date1 > date2) {
    stop("The start date (d1) cannot be after the end date (d2).")
  }

  all_params <- list(...)
  base_params <- purrr::compact(all_params)

  if (!is.null(base_params$bounds)) {
    bounds <- base_params$bounds
    if (inherits(bounds, c("sf", "sfc"))) {
      # Validate CRS is WGS84
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
        stop("'bounds' must be a numeric vector of length 4: ",
             "c(nelat, nelng, swlat, swlng)")
      }
      processed_bounds <- list(
        nelat = bounds[1], nelng = bounds[2],
        swlat = bounds[3], swlng = bounds[4]
      )
    }
    base_params$bounds <- NULL
    base_params <- c(base_params, processed_bounds)
  }

  if (!is.null(base_params$annotation)) {
    annotation <- base_params$annotation
    if (!is.numeric(annotation) || length(annotation)!= 2) {
      stop("The 'annotation' parameter must be a numeric vector of length 2: ",
           "c(term_id, term_value_id)")
    }
    processed_annotation <- list(
      term_id = annotation[1],
      term_value_id = annotation[2]
    )
    base_params$annotation <- NULL
    base_params <- c(base_params, processed_annotation)
  }

  total_results <- byday_get_total_results(c(base_params, list(d1 = d1, d2 = d2)))

  if (!quiet) {
    if (total_results > 0) {
      message("Found a total of ", format(total_results, big.mark = ","),
              " records between ", d1, " and ", d2, ".")
    } else {
      message("No records found for the specified criteria.")
    }
  }
  if (total_results == 0) return(tibble::tibble())

  if (total_results <= 10000) {
    params <- c(base_params, list(d1 = d1, d2 = d2))
    return(byday_download_chunk(params, total_results, quiet, limit_download))
  }

  if (!quiet) message(" -> Total > 10,000. Subdividing request...")
  all_results_list <- list()

  years_in_range <- unique(format(seq.Date(date1, date2, by = "day"), "%Y"))

  for (year_val in years_in_range) {
    year_start_date <- max(date1, as.Date(paste0(year_val, "-01-01")))
    year_end_date <- min(date2, as.Date(paste0(year_val, "-12-31")))
    months_in_range <- unique(format(seq.Date(year_start_date,
                                              year_end_date, by = "day"), "%Y-%m"))

    for (month_str in months_in_range) {
      m_date <- as.Date(paste0(month_str, "-01"))
      m <- as.numeric(format(m_date, "%m"))
      if (!quiet) message("\n--- Processing month: ", month.name[m],
                          " ", year_val, " ---")

      start_day <- as.numeric(format(max(year_start_date, m_date), "%d"))
      end_day <- as.numeric(format(min(year_end_date,
                                       as.Date(paste0(month_str, "-", lubridate::days_in_month(m_date)))), "%d"))
      is_full_month <- start_day == 1 && end_day == lubridate::days_in_month(m_date)

      if (is_full_month) {
        monthly_total <- byday_get_total_results(
          c(base_params, list(year = year_val, month = m))
        )
        if (!quiet && monthly_total > 0) {
          message(" -> Month has ", format(monthly_total, big.mark = ","),
                  " records.")
        }

        if (monthly_total > 0 && monthly_total <= 10000) {
          if (!quiet) message(" -> Downloading month in one go...")
          params <- c(base_params, list(year = year_val, month = m))
          all_results_list[[length(all_results_list) + 1]] <-
            byday_download_chunk(params, monthly_total, TRUE, limit_download)
        } else if (monthly_total > 10000) {
          if (!quiet) message(" -> Month > 10,000. Downloading day by day...")
          for (d in start_day:end_day) {
            params <- c(base_params, list(year = year_val, month = m, day = d))
            day_total <- byday_get_total_results(params)
            if (day_total > 0) {
              if (!quiet) message(" - Day: ", d, " has ", day_total, " records.")
              all_results_list[[length(all_results_list) + 1]] <-
                byday_download_chunk(params, day_total, TRUE, limit_download)
            }
          }
        }
      } else {
        for (d in start_day:end_day) {
          params <- c(base_params, list(year = year_val, month = m, day = d))
          day_total <- byday_get_total_results(params)
          if (day_total > 0) {
            if (!quiet) message(" - Day: ", d, " has ", day_total, " records.")
            all_results_list[[length(all_results_list) + 1]] <-
              byday_download_chunk(params, day_total, TRUE, limit_download)
          }
        }
      }
    }
  }

  final_data <- dplyr::bind_rows(all_results_list)
  if (nrow(final_data) > 0) {
    final_data <- final_data[!duplicated(final_data$id), ]
  }

  if (!quiet) {
    message("\nOverall process complete! A total of ",
            format(nrow(final_data), big.mark = ","),
            " unique records were obtained.")
  }
  return(final_data)
}
