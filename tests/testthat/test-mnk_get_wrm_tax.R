# Cargar las bibliotecas necesarias para los tests
skip_if_not_installed("mockery")
skip_if_not_installed("httptest")


# Cargar las bibliotecas necesarias para los tests
library(testthat)
library(tibble)

# (Descomenta si la función no está cargada automáticamente por devtools::test())
# source("../../R/mnk_get_wrm_tax.R")

# === Tests SIN conexión a internet (usando simulación/mocking) ===

test_that("get_wrm_tax handles API HTTP errors (e.g., 404, 500)", {
  # Este test ya funcionaba correctamente, no necesita cambios.
  # La función get_wrm_tax no llega a llamar a httr::content() si hay un error HTTP,
  # por lo que la estructura interna de la simulación no es tan crítica aquí.
  mock_httr_GET_error <- function(url) {
    structure(list(
      status_code = 404L,
      body = charToRaw("Not Found") # Usar 'body' o 'content' aquí es indiferente para este test
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
  # Función de simulación que devuelve una respuesta 200 OK con '[]'
  mock_httr_GET_empty <- function(url) {
    structure(list(
      status_code = 200L,
      # --- CORRECCIÓN ---
      # Cambiamos 'body' por 'content' para que httr::content() lo encuentre.
      content = charToRaw("[]")
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
  # JSON válido, imitando una respuesta real de la API
  valid_json <- '[{
    "AphiaID": 159782, "valid_AphiaID": 159782, "valid_name": "Diplodus sargus",
    "rank": "Species", "kingdom": "Animalia", "phylum": "Chordata",
    "class": "Actinopteri", "order": "Spariformes", "family": "Sparidae",
    "genus": "Diplodus", "isMarine": 1, "isBrackish": 1,
    "isFreshwater": 0, "isTerrestrial": 0, "isExtinct": 0
  }]'

  # Función de simulación que devuelve el JSON válido
  mock_httr_GET_success <- function(url) {
    structure(list(
      status_code = 200L,
      # --- CORRECCIÓN ---
      # Cambiamos 'body' por 'content' para que httr::content() lo encuentre.
      content = charToRaw(valid_json)
    ), class = "response")
  }

  with_mocked_bindings(
    GET = mock_httr_GET_success,
    .package = "httr",
    {
      result <- get_wrm_tax("Diplodus sargus")

      # Comprobamos que el parsing fue correcto
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_equal(result$valid_name, "Diplodus sargus")
      expect_equal(result$rank, "Species")
      expect_true(result$isMarine)
    }
  )
})

# === Tests para la validación de la entrada (no dependen de la API) ===

test_that("get_wrm_tax throws error for invalid input", {
  expect_error(get_wrm_tax(NULL), "'scientific_name' must be a single non-empty character string.")
  expect_error(get_wrm_tax(12345), "'scientific_name' must be a single non-empty character string.")
  expect_error(get_wrm_tax(c("a", "b")), "'scientific_name' must be a single non-empty character string.")
})



# === Tests que SÍ dependen de una conexión real a internet (Opcional) ===

test_that("get_wrm_tax works for a valid species name (live API call)", {
  skip_if_offline(host = "www.marinespecies.org")
  skip_on_cran()

  result <- get_wrm_tax("Diplodus sargus")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$valid_name, "Diplodus sargus")
})
