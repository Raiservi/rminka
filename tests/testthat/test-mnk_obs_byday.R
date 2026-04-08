# TEST 1
test_that("mnk_obs_byday handles small date ranges in one go", {
  httptest::with_mock_api({
    obs <- mnk_obs_byday(d1 = "2024-05-20", d2 = "2024-05-20", quiet = TRUE)
    expect_s3_class(obs, "tbl_df")
    expect_equal(nrow(obs), 1)
    expect_equal(obs$id, 101)
  })
})

# TEST 2 - NUEVO: subdivisión cuando total > 10.000
test_that("mnk_obs_byday subdivides requests when total results are large", {
  # Simulamos las respuestas de la API sin tocar internet
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      # 1) llamada inicial para todo el rango
      if (!is.null(p$d1) && !is.null(p$d2)) return(15000L)
      # 2) llamada para cada día
      if (!is.null(p$day)) return(1L)
      0L
    },
    byday_download_chunk = function(params, total_res, quiet, limit_download) {
      # devolvemos un id = 200 + día, para poder comprobarlo
      tibble::tibble(
        id = 200L + params$day,
        observed_on = sprintf("2024-04-%02d", params$day)
      )
    },
    .package = "rminka"
  )

  obs <- mnk_obs_byday(d1 = "2024-04-01", d2 = "2024-04-02", quiet = TRUE)

  expect_s3_class(obs, "tbl_df")
  expect_equal(nrow(obs), 2)
  expect_equal(obs$id, c(201, 202))
})

# TEST 3
test_that("mnk_obs_byday downloads a full month as a single chunk", {
  httptest::with_mock_api({
    obs <- mnk_obs_byday(d1 = "2024-03-01", d2 = "2024-04-30", quiet = TRUE)
    expect_s3_class(obs, "tbl_df")
    expect_equal(nrow(obs), 2)
    expect_equal(obs$id, c(301, 302))
  })
})

# TEST 4 - NUEVO: mes parcial (empieza y acaba a mitad de mes)
test_that("mnk_obs_byday handles partial months day by day", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if (!is.null(p$d1)) return(12000L) # fuerza subdivisión
      if (!is.null(p$day)) return(2L)    # cada día tiene 2
      0L
    },
    byday_download_chunk = function(params, ...) {
      tibble::tibble(id = params$day * 10 + 1:2)
    },
    .package = "rminka"
  )

  obs <- mnk_obs_byday("2024-05-10", "2024-05-12", quiet = TRUE)
  # 3 días x 2 registros = 6 filas
  expect_equal(nrow(obs), 6)
  expect_equal(obs$id, c(101,102,111,112,121,122))
})

# TEST 5 - NUEVO: conversión de bounds numérico
test_that("mnk_obs_byday converts numeric bounds to API params", {
  captured <- list()
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      captured <<- p
      return(1L)
    },
    byday_download_chunk = function(...) tibble::tibble(id=1),
    .package = "rminka"
  )

  mnk_obs_byday("2024-01-01","2024-01-01", bounds = c(42.2, 2.2, 38.2, 0.6), quiet = TRUE)

  expect_equal(captured$nelat, 42.2)
  expect_equal(captured$nelng, 2.2)
  expect_equal(captured$swlat, 38.2)
  expect_equal(captured$swlng, 0.6)
  expect_null(captured$bounds)
})

# TEST 6 - NUEVO: conversión de annotation
test_that("mnk_obs_byday converts annotation vector", {
  captured <- list()
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) { captured <<- p; 1L },
    byday_download_chunk = function(...) tibble::tibble(id=1),
    .package = "rminka"
  )

  mnk_obs_byday("2024-01-01","2024-01-01", annotation = c(12, 34), quiet = TRUE)

  expect_equal(captured$term_id, 12)
  expect_equal(captured$term_value_id, 34)
})

# TEST 7 - NUEVO: deduplicación
test_that("mnk_obs_byday removes duplicate ids across days", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) if(!is.null(p$d1)) 20000L else 1L,
    byday_download_chunk = function(params, ...) {
      # día 1 y día 2 devuelven el mismo id 999
      tibble::tibble(id = 999L)
    },
    .package = "rminka"
  )

  obs <- mnk_obs_byday("2024-06-01","2024-06-02", quiet = TRUE)
  expect_equal(nrow(obs), 1) # duplicado eliminado
  expect_equal(obs$id, 999)
})


test_that("byday_get_total_results devuelve 0 si hay http_error", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class="response"),
    http_error = function(...) TRUE,
    .package = "httr"
  )
  expect_equal(rminka:::byday_get_total_results(list()), 0)
})

test_that("byday_download_chunk salta página con error", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class="response"),
    http_error = function(...) TRUE,
    content = function(...) stop("no debería llamarse"),
    .package = "httr"
  )
  out <- rminka:::byday_download_chunk(list(), total_res = 200, quiet = TRUE, limit_download = TRUE)
  expect_equal(nrow(out), 0) # línea 58 cubierta
})


test_that("byday_process_results devuelve tibble vacío", {
  expect_equal(nrow(rminka:::byday_process_results(list())), 0) # línea 24
})

test_that("byday_download_chunk para cuando API devuelve results vacío", {
  calls <- 0
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class="response"),
    http_error = function(...) FALSE,
    content = function(...) { calls <<- calls + 1; list(results = list()) },
    .package = "httr"
  )
  out <- rminka:::byday_download_chunk(list(), 400, TRUE, TRUE)
  expect_equal(calls, 1) # entra en el else {break} línea 62
  expect_equal(nrow(out), 0)
})

test_that("mnk_obs_byday valida fechas", {
  expect_error(mnk_obs_byday("2024-13-01","2024-01-02"), "must be in 'yyyy-mm-dd'") #83
  expect_error(mnk_obs_byday("2024-02-01","2024-01-01"), "cannot be after") #84
})

test_that("mnk_obs_byday valida bounds y annotation", {
  expect_error(mnk_obs_byday("2024-01-01","2024-01-01", bounds = 1:3), "must be a numeric vector of length 4") #100
  expect_error(mnk_obs_byday("2024-01-01","2024-01-01", annotation = 1), "must be a numeric vector of length 2") #111
})

test_that("mnk_obs_byday muestra mensajes con quiet=FALSE", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) 0L,
    .package = "rminka"
  )
  expect_message(mnk_obs_byday("2024-01-01","2024-01-01", quiet=FALSE), "No records found") #125-126 y 128
})

test_that("mnk_obs_byday imprime progreso de subdivisión", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) if(!is.null(p$d1)) 15000L else 1L,
    byday_download_chunk = function(...) tibble::tibble(id=1),
    .package = "rminka"
  )
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "Total > 10,000") #135
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "Processing month") #148
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "has 1 records") #178
  expect_message(mnk_obs_byday("2024-04-01","2024-04-02", quiet=FALSE), "Overall process complete") #191
})

test_that("mnk_obs_byday descarga mes completo <=10000", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if(!is.null(p$d1)) return(15000L) # fuerza subdivisión
      if(!is.null(p$month) && is.null(p$day)) return(5000L) # línea 155-158
      0L
    },
    byday_download_chunk = function(...) tibble::tibble(id=301),
    .package = "rminka"
  )
  obs <- mnk_obs_byday("2024-03-01","2024-03-31", quiet=TRUE)
  expect_equal(obs$id, 301) # cubre 159-161
})

test_that("mnk_obs_byday descarga mes completo >10000 día a día", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if(!is.null(p$d1)) return(20000L)
      if(!is.null(p$month) && is.null(p$day)) return(15000L) # línea 162
      if(!is.null(p$day)) return(1L) # 166-169
      0L
    },
    byday_download_chunk = function(params, ...) tibble::tibble(id = params$day),
    .package = "rminka"
  )
  obs <- mnk_obs_byday("2024-03-01","2024-03-03", quiet=TRUE)
  expect_equal(nrow(obs), 3) # cubre bucle 164-169
})

test_that("mnk_obs_byday convierte bounds sf", {
  skip_if_not_installed("sf")

  # crea un sfc y lo convierte a sf (clase que tu función sí reconoce)
  poly_sfc <- sf::st_as_sfc("POLYGON((0 38, 2 38, 2 42, 0 42, 0 38))", crs = 4326)
  poly_sf <- sf::st_sf(geometry = poly_sfc)

  cap <- NULL
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p){ cap <<- p; 0L },
    .package = "rminka"
  )

  mnk_obs_byday("2024-01-01", "2024-01-01", bounds = poly_sf, quiet = TRUE)

  expect_equal(cap$swlng, 0)
  expect_equal(cap$swlat, 38)
  expect_equal(cap$nelng, 2)
  expect_equal(cap$nelat, 42)
})

# TEST – cubre líneas 161 y 164 (mes completo <=10k con mensajes)
test_that("mnk_obs_byday muestra mensajes para mes completo pequeño", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if (!is.null(p$d1)) return(15000L)      # fuerza subdivisión
      if (!is.null(p$month) && is.null(p$day)) return(5000L) # línea 160
      0L
    },
    byday_download_chunk = function(...) tibble::tibble(id = 1),
    .package = "rminka"
  )
  expect_message(
    mnk_obs_byday("2024-03-01", "2024-03-31", quiet = FALSE),
    "Month has 5,000"  # línea 161
  )
  expect_message(
    mnk_obs_byday("2024-03-01", "2024-03-31", quiet = FALSE),
    "Downloading month in one go"  # línea 164
  )
})

test_that("mnk_obs_byday muestra mensajes para mes completo grande", {
  testthat::local_mocked_bindings(
    byday_get_total_results = function(p) {
      if (!is.null(p$d1)) return(20000L)
      if (!is.null(p$month) && is.null(p$day)) return(15000L) # fuerza >10k
      if (!is.null(p$day)) return(1L)
      0L
    },
    byday_download_chunk = function(params, ...) tibble::tibble(id = params$day),
    .package = "rminka"
  )

  # USA MES COMPLETO, no 3 días
  msgs <- testthat::capture_messages(
    mnk_obs_byday("2024-03-01", "2024-03-31", quiet = FALSE)
  )

  expect_true(any(grepl("Month > 10,000", msgs)))          # ahora sí, línea 168
  expect_true(any(grepl("Day: 1 has 1 records", msgs)))    # líneas 173-174
  expect_true(any(grepl("Day: 3 has 1 records", msgs)))
})
