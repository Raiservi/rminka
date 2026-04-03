# tests/testthat/test-mnk_obs.R


skip_if_not_installed("httptest")
library(httptest)
# tests/testthat/test-mnk_obs.R



test_that("mnk_obs se detiene si no hay parámetros", {
  expect_error(mnk_obs(), "You must specify at least one search parameter")
})

test_that("mnk_obs valida los parámetros lógicos", {
  expect_error(mnk_obs(taxon_name = "test", quiet = "no"), "'quiet' must be TRUE or FALSE")
  expect_error(mnk_obs(taxon_name = "test", limit_download = "yes"), "'limit_download' must be TRUE or FALSE")
})

test_that("mnk_obs valida el parámetro 'quality'", {
  expect_error(mnk_obs(quality = "malo"), "must be 'casual' or 'research'")
})

# --- NUEVO TEST AÑADIDO ---
test_that("process_minka_results maneja una lista vacía", {
  # Objetivo: Cubrir la línea 28

  # Llamamos a la función interna con una lista vacía
  result <- rminka:::process_minka_results(list())

  # Comprobamos que devuelve un tibble vacío, como se espera
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
# ======================================================
# Tus tests originales que ya funcionaban
# ======================================================
test_that("mnk_obs se detiene si no hay parámetros", {
  expect_error(rminka::mnk_obs(), "You must specify at least one search parameter")
})

test_that("mnk_obs valida los parámetros lógicos", {
  expect_error(rminka::mnk_obs(taxon_name = "test", quiet = "no"), "'quiet' must be TRUE or FALSE")
  expect_error(rminka::mnk_obs(taxon_name = "test", limit_download = "yes"), "'limit_download' must be TRUE or FALSE")
})

test_that("mnk_obs valida el parámetro 'quality'", {
  expect_error(rminka::mnk_obs(quality = "malo"), "must be 'casual' or 'research'")
})

test_that("process_minka_results maneja una lista vacía", {
  result <- rminka:::process_minka_results(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# ======================================================
# EL NUEVO TEST QUE VAMOS A HACER FUNCIONAR
# ======================================================

test_that("download_paginated_data maneja una respuesta de 0 resultados", {
  # Objetivo: Cubrir líneas 87-89

  with_mock_api({
    # Al ejecutar el test, esto hará una llamada real a la API
    # Y httptest guardará la respuesta en un archivo .json
    result <- rminka:::download_paginated_data(params = list(q = "cero_resultados_test"))

    expect_equal(result$count, 0)
    expect_s3_class(result$data, "tbl_df")
  })
})

#------------------------------------------

test_that("download_paginated_data descarga una página de resultados", {
  # Objetivo: Cubrir el bucle while y el procesamiento de resultados (líneas 92-108)

  with_mock_api({
    # Usamos un parámetro 'q' diferente para que busque un archivo.json diferente
    result <- rminka:::download_paginated_data(params = list(q = "una_pagina_test"))

    # Comprobamos que ha procesado el resultado que pondremos en el.json
    expect_equal(result$count, 1)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 1)
    expect_equal(result$data$id[1], 12345) # Verificamos un dato del.json
  })
})

###-----------------------------------------------------

# tests/testthat/test-mnk_obs.R
#... (todos los tests anteriores)...

test_that("download_paginated_data maneja múltiples páginas de resultados", {
  # Objetivo: Forzar al bucle 'while' a ejecutarse más de una vez.
  # Para ello, simularemos una respuesta con 201 resultados.

  with_mock_api({
    # Usamos un nuevo parámetro 'q' para que busque un nuevo juego de archivos
    result <- rminka:::download_paginated_data(params = list(q = "multi_pagina_test"))

    # Comprobamos que el resultado final es correcto
    expect_equal(result$count, 201)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 201)
    # Verificamos el id del último registro para confirmar que la página 2 se añadió
    expect_equal(result$data$id[201], 99999)
  })
})
test_that("download_paginated_data maneja múltiples páginas de resultados", {
  with_mock_api({
    result <- rminka:::download_paginated_data(params = list(q = "multi_pagina_test"))
    expect_equal(result$count, 201)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 201)
    expect_equal(result$data$id[201], 99999)
  })
})

# Cargamos httptest, la herramienta correcta para este trabajo.
skip_if_not_installed("httptest")
library(httptest)
# tests/testthat/test-mnk_obs.R


test_that("mnk_obs se detiene si no hay parámetros", {
  expect_error(mnk_obs(), "You must specify at least one search parameter")
})

test_that("mnk_obs valida los parámetros lógicos", {
  expect_error(mnk_obs(taxon_name = "test", quiet = "no"), "'quiet' must be TRUE or FALSE")
  expect_error(mnk_obs(taxon_name = "test", limit_download = "yes"), "'limit_download' must be TRUE or FALSE")
})

test_that("mnk_obs valida el parámetro 'quality'", {
  expect_error(mnk_obs(quality = "malo"), "must be 'casual' or 'research'")
})

# --- NUEVO TEST AÑADIDO ---
test_that("process_minka_results maneja una lista vacía", {
  # Objetivo: Cubrir la línea 28

  # Llamamos a la función interna con una lista vacía
  result <- rminka:::process_minka_results(list())

  # Comprobamos que devuelve un tibble vacío, como se espera
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
# ======================================================
# Tus tests originales que ya funcionaban
# ======================================================
test_that("mnk_obs se detiene si no hay parámetros", {
  expect_error(rminka::mnk_obs(), "You must specify at least one search parameter")
})

test_that("mnk_obs valida los parámetros lógicos", {
  expect_error(rminka::mnk_obs(taxon_name = "test", quiet = "no"), "'quiet' must be TRUE or FALSE")
  expect_error(rminka::mnk_obs(taxon_name = "test", limit_download = "yes"), "'limit_download' must be TRUE or FALSE")
})

test_that("mnk_obs valida el parámetro 'quality'", {
  expect_error(rminka::mnk_obs(quality = "malo"), "must be 'casual' or 'research'")
})

test_that("process_minka_results maneja una lista vacía", {
  result <- rminka:::process_minka_results(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# ======================================================
# EL NUEVO TEST QUE VAMOS A HACER FUNCIONAR
# ======================================================

test_that("download_paginated_data maneja una respuesta de 0 resultados", {
  # Objetivo: Cubrir líneas 87-89

  with_mock_api({
    # Al ejecutar el test, esto hará una llamada real a la API
    # Y httptest guardará la respuesta en un archivo .json
    result <- rminka:::download_paginated_data(params = list(q = "cero_resultados_test"))

    expect_equal(result$count, 0)
    expect_s3_class(result$data, "tbl_df")
  })
})

#------------------------------------------

test_that("download_paginated_data descarga una página de resultados", {
  # Objetivo: Cubrir el bucle while y el procesamiento de resultados (líneas 92-108)

  with_mock_api({
    # Usamos un parámetro 'q' diferente para que busque un archivo.json diferente
    result <- rminka:::download_paginated_data(params = list(q = "una_pagina_test"))

    # Comprobamos que ha procesado el resultado que pondremos en el.json
    expect_equal(result$count, 1)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 1)
    expect_equal(result$data$id[1], 12345) # Verificamos un dato del.json
  })
})

###-----------------------------------------------------

# tests/testthat/test-mnk_obs.R
#... (todos los tests anteriores)...

test_that("download_paginated_data maneja múltiples páginas de resultados", {
  # Objetivo: Forzar al bucle 'while' a ejecutarse más de una vez.
  # Para ello, simularemos una respuesta con 201 resultados.

  with_mock_api({
    # Usamos un nuevo parámetro 'q' para que busque un nuevo juego de archivos
    result <- rminka:::download_paginated_data(params = list(q = "multi_pagina_test"))

    # Comprobamos que el resultado final es correcto
    expect_equal(result$count, 201)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 201)
    # Verificamos el id del último registro para confirmar que la página 2 se añadió
    expect_equal(result$data$id[201], 99999)
  })
})
test_that("download_paginated_data maneja múltiples páginas de resultados", {
  with_mock_api({
    result <- rminka:::download_paginated_data(params = list(q = "multi_pagina_test"))
    expect_equal(result$count, 201)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 201)
    expect_equal(result$data$id[201], 99999)
  })
})

