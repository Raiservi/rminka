# Cargar las bibliotecas necesarias para los tests
library(testthat)
library(tibble)

# Configuración de skips iniciales [1, 2]
skip_if_not_installed("mockery")
skip_if_not_installed("httptest")

# === Tests SIN conexión a internet (usando simulación/mocking) ===

test_that("get_wrm_tax handles API HTTP errors (e.g., 404, 500)", {
  # Definimos una respuesta de error más robusta
  mock_httr_GET_error <- function(url) {
    structure(list(
      status_code = 404L,
      content = charToRaw("Not Found"),
      url = "https://www.marinespecies.org/rest/mock-error", # CAMBIO: Añadida URL
      headers = list("Content-Type" = "text/plain")        # CAMBIO: Añadidos headers
    ), class = "response")
  }

  with_mocked_bindings(
    GET = mock_httr_GET_error,
    .package = "httr",
    {
      expect_message(
        result <- get_wrm_tax("any name"),
        regexp = "WoRMS API request failed. Status code: 404"
      )
      expect_null(result)
    }
  )
})

test_that("get_wrm_tax handles API returning empty JSON", {
  mock_httr_GET_empty <- function(url) {
    structure(list(
      status_code = 200L,
      content = charToRaw("[]"),
      url = "https://www.marinespecies.org/rest/mock-empty", # CAMBIO: Añadida URL
      headers = list("Content-Type" = "application/json")   # CAMBIO: Añadidos headers
    ), class = "response")
  }

  with_mocked_bindings(
    GET = mock_httr_GET_empty,
    .package = "httr",
    {
      expect_message(
        result <- get_wrm_tax("a name that returns empty"),
        regexp = "No taxon found for the scientific name: 'a name that returns empty'."
      )
      expect_null(result)
    }
  )
})

test_that("get_wrm_tax parses a valid JSON response correctly (offline)", {
  valid_json <- '[{
    "AphiaID": 159782, "valid_AphiaID": 159782, "valid_name": "Diplodus sargus",
    "rank": "Species", "kingdom": "Animalia", "phylum": "Chordata",
    "class": "Actinopteri", "order": "Spariformes", "family": "Sparidae",
    "genus": "Diplodus", "isMarine": 1, "isBrackish": 1,
    "isFreshwater": 0, "isTerrestrial": 0, "isExtinct": 0
  }]'

  mock_httr_GET_success <- function(url) {
    structure(list(
      status_code = 200L,
      content = charToRaw(valid_json),
      url = "https://www.marinespecies.org/rest/mock-success", # CAMBIO: Añadida URL
      headers = list("Content-Type" = "application/json")      # CAMBIO: Añadidos headers
    ), class = "response")
  }

  with_mocked_bindings(
    GET = mock_httr_GET_success,
    .package = "httr",
    {
      result <- get_wrm_tax("Diplodus sargus")

      # Verificamos estructura y contenido [3, 4]
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_equal(result$valid_name, "Diplodus sargus")
      expect_equal(result$rank, "Species")
      # El test asume que tu función convierte 1/0 a TRUE/FALSE
      expect_true(as.logical(result$isMarine))
    }
  )
})

# === Tests para la validación de la entrada (no dependen de la API) ===

test_that("get_wrm_tax throws error for invalid input", {
  # Validamos que se disparen los errores correctos [4, 5]
  err_msg <- "'scientific_name' must be a single non-empty character string."
  expect_error(get_wrm_tax(NULL), regexp = err_msg, fixed = TRUE)
  expect_error(get_wrm_tax(12345), regexp = err_msg, fixed = TRUE)
  expect_error(get_wrm_tax(c("a", "b")), regexp = err_msg, fixed = TRUE)
})

# === Test con conexión real (solo si hay internet y no es CRAN) ===

test_that("get_wrm_tax works for a valid species name (live API call)", {
  skip_if_offline(host = "www.marinespecies.org") # [6]
  skip_on_cran() # [2]

  result <- get_wrm_tax("Diplodus sargus")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$valid_name, "Diplodus sargus")
})
