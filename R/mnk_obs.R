#' Infix Operator for Null Coalescing
#'
#' Returns the right-hand side `b` if the left-hand side `a` is NULL.
#'
#' @param a The value to check for NULL.
#' @param b The default value to return if `a` is NULL.
#' @return The first non-NULL value.
#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# ===================================================================
# FUNCIONES AUXILIARES (NO EXPORTADAS)
# Se definen en orden de dependencia, de menos a más dependientes.
# ===================================================================

#' Process Raw Minka API Results
#'
#' This internal function takes the raw list of observation results from the API
#' and transforms it into a clean `tibble::tibble`, handling missing fields
#'  gracefully.
#' @param all_results A list containing the raw observation data from the API.
#' @return A `tibble::tibble` with structured observation data.
#' @noRd
process_minka_results <- function(all_results) {
  if (length(all_results) == 0) {
    return(tibble::tibble())
  }
  processed_list <- purrr::map(all_results, ~tibble::tibble(
    id =.x$id %||% NA_integer_,
    observed_on =.x$observed_on %||% NA,
    year =.x$observed_on_details$year %||% NA_integer_,
    month =.x$observed_on_details$month %||% NA_integer_,
    week =.x$observed_on_details$week %||% NA_integer_,
    day =.x$observed_on_details$day %||% NA_integer_,
    hour =.x$observed_on_details$hour %||% NA_integer_,
    created_at =.x$created_at %||% NA,
    updated_at =.x$updated_at %||% NA,
    latitude =.x$geojson$coordinates[[2]] %||% NA_real_,
    longitude =.x$geojson$coordinates[[1]] %||% NA_real_,
    positional_accuracy =.x$positional_accuracy %||% NA_integer_,
    geoprivacy =.x$taxon_geoprivacy %||% NA,
    obscured =.x$obscured %||% NA,
    uri =.x$uri %||% NA,
    photo_url_square =.x$taxon$default_photo$square_url %||% NA_character_,
    photo_url_medium =.x$taxon$default_photo$medium_url %||% NA_character_,
    quality_grade =.x$quality_grade %||% NA,
    species_guess =.x$species_guess %||% NA,
    taxon_id =.x$taxon$id %||% NA_integer_,
    taxon_name =.x$taxon$name %||% NA,
    taxon_rank =.x$taxon$rank %||% NA,
    taxon_min_ancestry =.x$taxon$min_species_ancestry %||% NA,
    taxon_endemic =.x$taxon$endemic %||% NA,
    taxon_threatened =.x$taxon$threatened %||% NA,
    taxon_introduced =.x$taxon$introduced %||% NA,
    taxon_native =.x$taxon$native %||% NA,
    user_id =.x$user$id %||% NA_integer_,
    user_login =.x$user$login %||% NA
  ))
  dplyr::bind_rows(processed_list)
}

#' Download Paginated Data from Minka API
#'
#' This helper function handles the pagination logic for a given set of query
#' parameters. It fetches data in chunks up to the user-defined limit.
#' @param params A list of query parameters for the API call.
#' @param total_res Optional. The total number of results to expect, to avoid an
#' initial ping.
#' @param quiet A logical value. If TRUE, suppresses all messages.
#' @param numeric_limit The numeric maximum number of records to download for
#' this specific call.
#' @return A list containing `$data` (a tibble) and `$count` (number of rows).
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
#' This function downloads data for a given month, respecting the remaining
#' download limit.
#' @param base_params A list of base query parameters.
#' @param year The year to download.
#' @param current_month The month to download.
#' @param quiet A logical value. If TRUE, suppresses messages.
#' @param remaining_limit The maximum number of records to download in this and
#' subsequent calls.
#' @return A list containing `$data` (a tibble) and `$count` (number of rows).
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

# ===================================================================
# MAIN FUNCTION

#' Download Observations from the Minka-SDG API
#'
#' This function provides a high-level interface to download observation data from
#' the Minka-SDG API. It automatically handles pagination and rate limits by
#' subdividing large queries by month or day.
#'
#' @param query A generic query string for the 'q' parameter in the API. The
#' exact behavior is not specified by the API documentation.
#' @param taxon_name A character string for the taxon name (common or
#' scientific).
#' @param taxon_id A numeric ID for the taxon.
#' @param user_id A numeric ID for a specific user.
#' @param project_id A numeric ID for a specific project.
#' @param place_id A numeric ID for a specific place.
#' @param endemic A logical value (`TRUE`/`FALSE`). Filter for endemic species.
#' @param introduced A logical value (`TRUE`/`FALSE`). Filter for introduced
#' species.
#' @param threatened A logical value (`TRUE`/`FALSE`). Filter for threatened
#' species.
#' @param quality A character string. Must be either "casual" or "research".
#' @param geo A logical value. If `TRUE`, filters for observations with
#' geographic coordinates.
#' @param annotation A numeric vector of length 2 (`c(term_id, term_value_id)`).
#' The exact IDs for terms and values are not specified.
#' @param year A numeric value for the year.
#' @param month A numeric value for the month (1-12).
#' @param day A numeric value for the day (1-31).
#' @param bounds An object representing a bounding box. Can be an `sf` object or
#' a numeric vector of 4 elements in the order: `c(nelat, nelng, swlat, swlng)`.
#' @param quiet A logical value. If `TRUE`, all console messages during download
#' will be suppressed. Defaults to `FALSE`.
#' @param limit_download A logical value. If `TRUE` (default), the download is
#' capped at 10,000 records. If `FALSE`, it attempts to download all matching
#' records.
#' @return A `tibble::tibble` containing the downloaded observation data, or an
#' empty tibble if no data is found or an error occurs.
#' @importFrom sf st_bbox
#' @export
#' @examples
#' \dontrun{
#' # Download up to 10,000 observations of "Diplodus sargus" for August 2025
#' (default limit)
#' d_sargus <- mnk_obs(taxon_name = "Diplodus sargus", year = 2025, month = 8)
#'
#' # Attempt to download ALL observations from a project for the entire year
#' 2024
#' project_data <- mnk_obs(project_id = 419, year = 2024,limit_download = FALSE)
#'
#' # Download threatened species quietly (up to 10,000)
#' threatened_data <- mnk_obs(threatened = TRUE, quiet = TRUE)
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
