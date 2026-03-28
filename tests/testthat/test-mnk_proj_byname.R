

library(testthat)
library(httr)
library(jsonlite)
library(stringr)
library(tibble)
library(purrr)


test_that("mnk_proj_byname throws error for invalid query", {
  expect_error(mnk_proj_byname(NULL), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  expect_error(mnk_proj_byname(NA_character_), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  expect_error(mnk_proj_byname(""), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  expect_error(mnk_proj_byname("   "), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  expect_error(mnk_proj_byname(c("query1", "query2")), "You must provide a single query string. Only one query is accepted.")
})


test_that("mnk_proj_byname handles API HTTP errors", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=error_query", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 500L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('{"error": "Internal server error"}')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de error HTTP: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_proj_byname("error_query"), regexp = "Minka API request failed for query 'error_query'. Status code: 500")
      expect_null(result)
    }
  )
})


test_that("mnk_proj_byname handles empty or null API response", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=empty_response", full_url)) {
      # Simula una respuesta con cuerpo vacío
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else if (grepl("q=null_response", full_url)) {
      # Simula una respuesta cuyo cuerpo es el JSON 'null'
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('null')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado URL test respuesta vacía/nula: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_proj_byname("empty_response"), regexp = "API returned an empty or null response for query 'empty_response'.")
      expect_null(result)

      expect_message(result <- mnk_proj_byname("null_response"), regexp = "API returned an empty or null response for query 'null_response'.")
      expect_null(result)
    }
  )
})


test_that("mnk_proj_byname handles JSON with no projects found", {
  # Simula una respuesta con una lista de resultados vacía
  mock_response_json <- '{"results": []}'

  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=no_projects", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(mock_response_json)
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado esta URL test de no proyectos: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_proj_byname("no_projects"), regexp = "No projects found for query 'no_projects'.")
      expect_s3_class(result, "data.frame") # Devuelve un tibble/data.frame vacío
      expect_equal(nrow(result), 0)
    }
  )
})


# --- Test para API devuelve datos válidos (VERSIÓN CORREGIDA PARA TU FUNCIÓN) ---
test_that("mnk_proj_byname returns a tibble with specific columns for a valid query", {
  # El JSON de prueba ahora usa las columnas que la función busca ('title', 'slug', etc.)
  # También añadimos una columna extra para asegurarnos de que la función la ignora.
  # El segundo objeto JSON no tiene todos los campos para probar la asignación de NA.
  mock_response_json <- '{
    "results": [
      {
        "id": 101,
        "title": "Proyecto Test A",
        "place_id": 901,
        "slug": "proyecto-test-a",
        "created_at": "2023-01-01T12:00:00Z",
        "updated_at": "2023-01-02T12:00:00Z",
        "project_type": "collection",
        "description": "Descripción del Proyecto A",
        "columna_extra_a_ignorar": "valor extra"
      },
      {
        "id": 102,
        "title": "Proyecto Test B",
        "slug": "proyecto-test-b"
      }
    ]
  }'

  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=Proyecto%20Test", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(mock_response_json)
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de exito: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      result <- mnk_proj_byname("Proyecto Test")

      # 1. Comprobamos que el resultado es un tibble con 2 filas
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 2)

      # 2. Comprobamos que el resultado TIENE EXACTAMENTE las columnas deseadas
      columnas_esperadas <- c("id", "title", "place_id", "slug", "created_at",
                              "updated_at", "project_type", "description")
      expect_equal(sort(names(result)), sort(columnas_esperadas))

      # 3. Comprobamos los valores, incluyendo los NA para campos ausentes
      expect_equal(result$title, c("Proyecto Test A", "Proyecto Test B"))
      expect_equal(result$id, c(101, 102))

      # El segundo proyecto no tenía 'place_id', así que debe ser NA_integer_
      expect_equal(result$place_id, c(901, NA_integer_))

      # El segundo proyecto no tenía 'description', así que debe ser NA_character_
      expect_equal(result$description, c("Descripción del Proyecto A", NA_character_))
    }
  )
})

