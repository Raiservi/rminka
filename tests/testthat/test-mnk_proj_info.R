
skip_if_not_installed("mockery")
library(testthat)

# También las librerías que la función usa
library(httr)
library(jsonlite)
library(tibble)

# --- INICIO DE LOS TESTS ---

# Test 1: Comprobación de argumentos inválidos (sin cambios, estaba correcto)
test_that("mnk_proj_info handles invalid input", {
  expect_error(mnk_proj_info(project_id = NULL, grpid = NULL), "You must provide either 'project_id' or 'grpid'")
  expect_error(mnk_proj_info(project_id = c("1", "2")), "'project_id' must be a single character string or number.")
  expect_error(mnk_proj_info(grpid = c("group1", "group2")), "'grpid' must be a single character string or number.")
})

# Test 2: Errores de red y HTTP (MODIFICADO para añadir URL al mock de error)
test_that("mnk_proj_info handles network and HTTP errors", {
  # --- Sub-test para Error de Red ---
  mock_GET_network_error <- function(...) { stop("Failed to connect") }

  local_mocked_bindings(GET = mock_GET_network_error, .package = "httr")

  expect_message(
    result_net <- mnk_proj_info(project_id = 123),
    "Network error: Minka API is unavailable."
  )
  expect_null(result_net)

  # --- Sub-test para Error HTTP ---
  # Simulamos una respuesta con código de error (ej. 404)
  mock_GET_http_error <- function(url, ...) {
    # CORRECCIÓN: Añadido 'url' al objeto de respuesta
    structure(
      list(
        url = url, # <- AÑADIDO
        status_code = 404L,
        content = charToRaw('{}')
      ),
      class = c("response", "handle")
    )
  }

  local_mocked_bindings(GET = mock_GET_http_error, .package = "httr")

  expect_message(
    result_http <- mnk_proj_info(project_id = "not_found"),
    "Minka API request failed. Status code: 404"
  )
  expect_null(result_http)
})

# Test 3: Respuestas de la API vacías o sin resultados (MODIFICADO para añadir URL)
test_that("mnk_proj_info handles empty, null, or no-result API responses", {
  mock_GET_various_empty <- function(url, query, ...) {
    content <- switch(query$id,
                      "empty_string" = "",
                      "null_string" = "null",
                      "no_results" = '{"results": []}',
                      '{"other_field": 1}' # Sin campo 'results'
    )
    # CORRECCIÓN: Añadido 'url' al objeto de respuesta
    structure(
      list(
        url = url, # <- AÑADIDO
        status_code = 200L,
        content = charToRaw(content)
      ),
      class = c("response", "handle")
    )
  }
  local_mocked_bindings(GET = mock_GET_various_empty, .package = "httr")

  expect_message(mnk_proj_info(project_id = "empty_string"), "API returned an empty or null response")
  expect_message(mnk_proj_info(project_id = "null_string"), "API returned an empty or null response")
  expect_message(mnk_proj_info(project_id = "no_results"), "No project details found")
  expect_message(mnk_proj_info(project_id = "other"), "No project details found")
})


# --- TESTS PARA LA LÓGICA DE 'users' (MODIFICADOS para añadir URL) ---

# Test 4: Comprueba el comportamiento por defecto (users = FALSE)
test_that("mnk_proj_info with users=FALSE returns project info list", {
  mock_response_content <- '{
    "results": [ { "id": 420, "title": "Test Project", "description": "A desc.", "slug": "test-proj", "created_at": "2023-01-01", "place_id": 101, "user_ids": [10, 20, 30] } ]
  }'
  mock_GET_success <- function(url, ...) {
    structure(
      list(url = url, status_code = 200L, content = charToRaw(mock_response_content)),
      class = c("response", "handle")
    )
  }
  local_mocked_bindings(GET = mock_GET_success, .package = "httr")

  result <- mnk_proj_info(project_id = 420, users = FALSE)

  expect_type(result, "list")
  expect_named(result, c("id", "title", "created_at", "subscrib_users", "place_id", "slug", "description"))
  expect_equal(result$id, 420)
})

# Test 5: Comprueba el comportamiento con users = TRUE
test_that("mnk_proj_info with users=TRUE returns a tibble of user IDs", {
  mock_response_content <- '{
    "results": [ { "user_ids": [10, 20, 30] } ]
  }'
  mock_GET_success <- function(url, ...) {
    structure(
      list(url = url, status_code = 200L, content = charToRaw(mock_response_content)),
      class = c("response", "handle")
    )
  }
  local_mocked_bindings(GET = mock_GET_success, .package = "httr")

  result <- mnk_proj_info(project_id = 420, users = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "id_users")
  expect_equal(nrow(result), 3)
})

# Test 6: Comprueba el caso de 'users = TRUE' pero no hay usuarios
test_that("mnk_proj_info with users=TRUE handles no users", {
  mock_response_no_users <- '{ "results": [ { "id": 421, "user_ids": [] } ] }'
  mock_GET_no_users <- function(url, ...) {
    structure(
      list(url = url, status_code = 200L, content = charToRaw(mock_response_no_users)),
      class = c("response", "handle")
    )
  }
  local_mocked_bindings(GET = mock_GET_no_users, .package = "httr")

  result <- mnk_proj_info(project_id = 421, users = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "id_users")
  expect_equal(nrow(result), 0)
})

# Test 7: Comprueba el manejo de campos nulos con users=FALSE
test_that("mnk_proj_info handles missing fields correctly when users=FALSE", {
  mock_response_missing_fields <- '{ "results": [ { "id": 777, "title": "Missing" } ] }'
  mock_GET_missing <- function(url, ...) {
    structure(
      list(url = url, status_code = 200L, content = charToRaw(mock_response_missing_fields)),
      class = c("response", "handle")
    )
  }
  local_mocked_bindings(GET = mock_GET_missing, .package = "httr")

  result <- mnk_proj_info(project_id = 777)

  expect_true(is.na(result$created_at))
  expect_true(is.na(result$place_id))
  expect_equal(result$subscrib_users, 0)
})

