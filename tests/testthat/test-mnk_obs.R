
# tests/testthat/test-mnk_obs.R

skip_if_not_installed("mockery")
library(mockery)
library(testthat)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

# --- Operador y Mocks (sin cambios) ---
`%||%` <- function(a, b) if (is.null(a)) b else a
mock_json_single_observation_record_content <- '{
  "id": 12345, "observed_on": "2025-01-15",
  "observed_on_details": {"year": 2025, "month": 1, "week":3, "day":15, "hour":10},
  "created_at": "2025-01-15T10:00:00Z", "updated_at": "2025-01-15T10:00:00Z",
  "geojson": {"coordinates": [2.0, 41.0]}, "positional_accuracy": 5,
  "taxon_geoprivacy": "obscured", "obscured": true, "uri": "http://minka/obs/12345",
  "taxon": {"default_photo": {"square_url": "sq.jpg", "medium_url": "med.jpg"}, "id":100, "name":"Sp", "rank":"species", "min_species_ancestry":"anc", "endemic":true, "threatened":false, "introduced":false, "native":true},
  "quality_grade": "research", "species_guess": "Guess", "user": {"id":1, "login":"user1"}
}'

# --- TESTS ---

test_that("mnk_obs handles invalid input", {
  expect_error(mnk_obs(), "You must specify at least one search parameter")
})

test_that("mnk_obs handles day-specific download", {
  mock_dpd <- function(...) { return(list(data = tibble(id = 1:50), count = 50)) }
  with_mocked_bindings(download_paginated_data = mock_dpd, {
    result <- suppressMessages(mnk_obs(taxon_name="test", year=2025, month=1, day=15))
    expect_s3_class(result, "tbl_df"); expect_equal(nrow(result), 50)
  })
})

test_that("mnk_obs handles month-specific download (<10k)", {
  mock_dmd <- function(...) { return(list(data = tibble(id = 1:500), count = 500)) }
  with_mocked_bindings(download_month_data = mock_dmd, {
    result <- suppressMessages(mnk_obs(taxon_name="Sp_Monthly_500", year=2025, month=1))
    expect_s3_class(result, "tbl_df"); expect_equal(nrow(result), 500)
  })
})

test_that("mnk_obs annual mode correctly calls monthly downloads", {
  call_log <- list()
  mock_dmd_annual <- function(base_params, year, current_month, quiet, remaining_limit) {
    call_log[[length(call_log) + 1]] <<- current_month
    return(list(data = tibble(id = 1), count = 1))
  }
  with_mocked_bindings(download_month_data = mock_dmd_annual, {
    suppressMessages(mnk_obs(taxon_name="Sp_Annual_Generic", year=2025, limit_download=FALSE))
    expect_equal(length(call_log), 12)
  })
})

test_that("download_month_data subdivides by day when monthly_total > 10000", {
  download_month_data_local <- function(base_params, year, current_month, quiet = FALSE, remaining_limit = 10000) {
    monthly_params <- c(base_params, list(year = year, month = current_month, per_page = 1))
    ping <- httr::GET(url = "https://api.minambiente.gov.co", path = "/v1/observations", query = monthly_params)
    total_monthly <- jsonlite::fromJSON(httr::content(ping, "text", encoding = "UTF-8"))$total_results %||% 0

    if (total_monthly > 10000) {
      # **LA CORRECCIÓN ESTÁ AQUÍ**
      # Forma robusta de calcular los días del mes: ir al mes siguiente y restar un día.
      first_day_next_month <- as.Date(paste(year, current_month + 1, "01", sep = "-"), format = "%Y-%m-%d")
      last_day_current_month <- first_day_next_month - 1
      days_in_month <- as.integer(format(last_day_current_month, "%d"))

      daily_results <- purrr::map(1:days_in_month, function(day) {
        day_params <- c(base_params, list(year = year, month = current_month, day = day))
        download_paginated_data(day_params, quiet = quiet, numeric_limit = remaining_limit)
      })
      all_data <- dplyr::bind_rows(purrr::map(daily_results, "data"))
      return(list(data = all_data, count = nrow(all_data)))
    } else {
      # Este bloque no se ejecuta en este test, pero se mantiene por completitud
      month_params <- c(base_params, list(year = year, month = current_month))
      return(download_paginated_data(month_params, total_res = total_monthly, quiet = quiet, numeric_limit = remaining_limit))
    }
  }

  daily_calls <- 0
  mock_dpd_daily <- function(...) {
    daily_calls <<- daily_calls + 1
    return(list(data = tibble(id = 1:100), count = 100))
  }
  stub(download_month_data_local, 'download_paginated_data', mock_dpd_daily)
  stub(download_month_data_local, 'httr::GET', function(...) {
    structure(list(status_code = 200L, content = charToRaw('{"total_results": 12000}')), class = "response")
  })

  res <- suppressMessages(download_month_data_local(base_params=list(), year=2025, current_month=2, quiet=TRUE, remaining_limit=Inf))

  expect_equal(daily_calls, 28)
  expect_equal(res$count, 2800)
})

test_that("process_minka_results correctly transforms list with missing data", {
  process_minka_results_local <- function(all_results) {
    if (length(all_results) == 0) return(tibble::tibble())
    processed_list <- purrr::map(all_results, ~tibble::tibble(
      id =.x$id %||% NA_integer_,
      photo_url_square =.x$taxon$default_photo$square_url %||% NA_character_
    ))
    dplyr::bind_rows(processed_list)
  }
  mock_data_list <- list(
    jsonlite::fromJSON(mock_json_single_observation_record_content, simplifyVector = FALSE),
    list(id = 2, taxon = list(default_photo = NULL))
  )
  result <- process_minka_results_local(mock_data_list)
  expect_s3_class(result, "tbl_df")
  expect_true("photo_url_square" %in% names(result))
  expect_true(is.na(result$photo_url_square[2]))
})

