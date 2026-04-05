# Cargamos las librerías necesarias para los tests
skip_if_not_installed("httptest")
skip_if_not_installed("tibble")
skip_if_not_installed("sf")
library(httptest)
library(tibble)
library(sf)

# ======================================================
# Tests de validación de parámetros de mnk_obs()
# ======================================================

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

# ======================================================
# Tests para funciones auxiliares (internas)
# ======================================================

test_that("process_minka_results maneja una lista vacía", {
  # Objetivo: Cubrir la línea del if (length(all_results) == 0)
  result <- rminka:::process_minka_results(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("download_paginated_data maneja una respuesta de API con 0 resultados", {
  # Objetivo: Cubrir el caso donde la API devuelve total_results = 0
  with_mock_api({
    result <- rminka:::download_paginated_data(params = list(q = "cero_resultados_test"))

    expect_equal(result$count, 0)
    expect_s3_class(result$data, "tbl_df")
  })
})

test_that("download_paginated_data descarga una página de resultados", {
  # Objetivo: Cubrir el bucle de paginación con una sola ejecución
  with_mock_api({
    result <- rminka:::download_paginated_data(params = list(q = "una_pagina_test"))

    expect_equal(result$count, 1)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 1)
    expect_equal(result$data$id[1], 12345) # Dato del mock JSON
  })
})

test_that("download_paginated_data maneja múltiples páginas de resultados", {
  # Objetivo: Forzar al bucle de paginación a ejecutarse más de una vez
  with_mock_api({
    result <- rminka:::download_paginated_data(params = list(q = "multi_pagina_test"))

    expect_equal(result$count, 201)
    expect_s3_class(result$data, "tbl_df")
    expect_equal(nrow(result$data), 201)
    # Verificamos el ID del último registro para confirmar que la página 2 se añadió
    expect_equal(result$data$id[201], 99999)
  })
})

test_that("mnk_obs valida el formato del parámetro 'annotation'", {
  # Debe ser un vector numérico
  expect_error(
    mnk_obs(annotation = c("a", "b")),
    "The 'annotation' parameter must be a numeric vector of length 2"
  )
  # Debe tener longitud 2
  expect_error(
    mnk_obs(annotation = c(1, 2, 3)),
    "The 'annotation' parameter must be a numeric vector of length 2"
  )
})

test_that("mnk_obs valida el formato del parámetro 'bounds'", {
  # Si no es un objeto 'sf', debe ser un vector numérico de longitud 4
  expect_error(
    mnk_obs(bounds = c(1, 2, 3)), # Longitud incorrecta
    "'bounds' must be a numeric vector of length 4"
  )
  expect_error(
    mnk_obs(bounds = c("a", "b", "c", "d")), # Tipo de dato incorrecto
    "'bounds' must be a numeric vector of length 4"
  )
})

test_that("mnk_obs descarga datos para un día específico", {
  # Objetivo: Cubrir la rama if (!is.null(year) && !is.null(month) && !is.null(day))
  with_mock_api({
    # Usamos un taxon_name único para que httptest cree un mock específico para este test.
    datos_dia <- mnk_obs(taxon_name = "test_taxon_dia", year = 2025, month = 8, day = 15)

    # Comprobaciones basadas en el mock que vamos a crear
    expect_s3_class(datos_dia, "tbl_df")
    expect_equal(nrow(datos_dia), 1)
    expect_equal(datos_dia$id[1], 815) # ID de ejemplo para el día 15/8
  })
})

test_that("mnk_obs descarga un mes completo (menos de 10k resultados)", {
  # Objetivo: Cubrir la rama de descarga por mes, sin subdivisión.
  with_mock_api({
    # Usamos un taxon_name nuevo para no mezclar mocks
    datos_mes <- mnk_obs(taxon_name = "test_taxon_mes", year = 2025, month = 9)

    expect_s3_class(datos_mes, "tbl_df")
    expect_equal(nrow(datos_mes), 2)
    # Comprobamos el ID del segundo registro para confirmar que se leyeron ambos
    expect_equal(datos_mes$id[2], 902)
  })
})

test_that("mnk_obs descarga un año completo, iterando por meses", {
  # Objetivo: Cubrir la rama de descarga anual y el bucle de 12 meses.
  with_mock_api({
    # quiet = TRUE es útil para no llenar la consola de salida durante el test.
    datos_anuales <- mnk_obs(taxon_name = "test_taxon_anual", year = 2024, quiet = TRUE)

    expect_s3_class(datos_anuales, "tbl_df")
    expect_equal(nrow(datos_anuales), 3)
    # Comprobamos que un ID del último mes con datos está presente
    expect_true(202 %in% datos_anuales$id)
  })
})

test_that("mnk_obs descarga datos sin filtro de fecha", {
  # Objetivo: Cubrir la rama 'else' final, cuando no hay parámetros de fecha.
  with_mock_api({
    # Usamos un parámetro que no sea de fecha, como `project_id`.
    datos_sin_fecha <- mnk_obs(project_id = 999, quiet = TRUE)

    expect_s3_class(datos_sin_fecha, "tbl_df")
    expect_equal(nrow(datos_sin_fecha), 1)
    expect_equal(datos_sin_fecha$id[1], 99901)
  })
})

test_that("mnk_obs subdivide la descarga por días si un mes tiene >10k resultados", {
  # Objetivo: Cubrir la lógica de subdivisión en `download_month_data`.
  with_mock_api({
    # Usamos taxon_name y mes únicos.
    datos_subdivididos <- mnk_obs(taxon_name = "test_taxon_subdivision", year = 2024, month = 4, quiet = TRUE)

    expect_s3_class(datos_subdivididos, "tbl_df")
    expect_equal(nrow(datos_subdivididos), 1)
    expect_equal(datos_subdivididos$id[1], 40101)
  })
})

test_that("mnk_obs valida correctamente todos los tipos de parámetros", {

  # -----------------------------------------------------------------
  # Objetivo: Cubrir las ramas `stop()` de validación de parámetros
  # Estas pruebas no requieren mock de API porque fallan antes.
  # -----------------------------------------------------------------

  # Validar 'endemic'
  expect_error(
    mnk_obs(endemic = "no"),
    "The 'endemic' parameter must be TRUE or FALSE."
  )

  # Validar 'introduced'
  expect_error(
    mnk_obs(introduced = "yes"),
    "The 'introduced' parameter must be TRUE or FALSE."
  )

  # Validar 'threatened'
  expect_error(
    mnk_obs(threatened = 1),
    "The 'threatened' parameter must be TRUE or FALSE."
  )

  # Validar 'annotation' (formato incorrecto)
  expect_error(
    mnk_obs(annotation = c(1, 2, 3)),
    "The 'annotation' parameter must be a numeric vector of length 2"
  )
  expect_error(
    mnk_obs(annotation = c("a", "b")),
    "The 'annotation' parameter must be a numeric vector of length 2"
  )

  # Validar 'bounds' como vector numérico (formato incorrecto)
  expect_error(
    mnk_obs(bounds = c(1, 2, 3)),
    "'bounds' must be a numeric vector of length 4"
  )
  expect_error(
    mnk_obs(bounds = c("a", "b", "c", "d")),
    "'bounds' must be a numeric vector of length 4"
  )

})

test_that("download_paginated_data devuelve un tibble vacío si la API falla", {
  # Objetivo: Verificar el comportamiento del manejo de errores INTERNO de la función.
  with_mock_api({
    # Simulamos la llamada que sabemos que tiene un mock de error 500
    resultado <- rminka:::download_paginated_data(
      params = list(q = "test_api_error_500"),
      quiet = TRUE # Usamos quiet=TRUE para no llenar la consola
    )

    # La prueba de fuego: ¿Qué devuelve la función tras capturar el error?
    # Asumimos que devuelve la estructura esperada pero sin datos.
    expect_s3_class(resultado$data, "tbl_df")
    expect_equal(nrow(resultado$data), 0)
    expect_equal(resultado$count, 0)
  })
})


test_that("download_paginated_data muestra mensaje al superar el límite de descarga", {
  # Objetivo: Cubrir el `if (total_res > numeric_limit ...)` que imprime una nota
  with_mock_api({
    # Esperamos el mensaje específico sobre el límite
    expect_message(
      # Ejecutamos la función pero no nos importa el resultado, solo el mensaje
      void <- rminka:::download_paginated_data(
        params = list(q = "test_limit_message"),
        numeric_limit = 50,
        quiet = FALSE
      ),
      "NOTE: Fetching only the first 50 of 101 available records"
    )
  })
})

test_that("mnk_obs maneja correctamente una respuesta sin resultados", {
  # Este test se asegura de que la función devuelve una tabla vacía
  # y (si no está en modo 'quiet') muestra un mensaje.

  # 1. Probamos que devuelve una tabla (tibble) vacía
  httptest::with_mock_api({
    # --- CAMBIO AQUÍ ---
    no_results <- mnk_obs(query = "no_existe_nada_con_este_nombre", quiet = TRUE)

    expect_s3_class(no_results, "data.frame")
    expect_equal(nrow(no_results), 0)
  })

  # 2. Probamos que muestra el mensaje correcto cuando quiet = FALSE
  expect_message(
    httptest::with_mock_api({
      # --- Y CAMBIO AQUÍ ---
      mnk_obs(query = "no_existe_nada_con_este_nombre", quiet = FALSE)
    }),
    "No data could be downloaded"
  )
})

test_that("mnk_obs procesa correctamente múltiples parámetros a la vez", {

  httptest::with_mock_api({

    many_params_result <- mnk_obs(
      query = "test_muchos_parametros",
      taxon_id = 5,
      user_id = 10,
      place_id = 15,
      project = "mi-proyecto-test",
      geo = TRUE,
      endemic = TRUE,
      threatened = TRUE,
      introduced = FALSE,
      quality = "research",
      quiet = TRUE
    )

    expect_s3_class(many_params_result, "data.frame")

  })
})

test_that("El parámetro 'limit_download' funciona correctamente", {

  httptest::with_mock_api({

    results_paginated <- mnk_obs(month = "September", quiet = TRUE)

    expect_equal(nrow(results_paginated), 2)
  })

  httptest::with_mock_api({

    results_one_page <- mnk_obs(month = "September", limit_download = FALSE, quiet = TRUE)

    expect_equal(nrow(results_one_page), 2)
  })

})

test_that("mnk_obs procesa bounds y annotation correctamente", {

  # Usaremos un mock específico para esta combinación de parámetros
  httptest::with_mock_api({

    # Definimos los parámetros que queremos probar
    barcelona_bounds <- c(41.3, 2.1, 41.4, 2.2) # Formato: sw_lat, sw_lng, ne_lat, ne_lng
    life_stage_annotation <- c(1, 2)          # Ejemplo: Adulto y Teno

    # Hacemos la llamada con los nuevos parámetros
    # (La combinamos con 'month' para hacer la URL más específica)
    obs_bounds_annot <- mnk_obs(
      month = "September",
      bounds = barcelona_bounds,
      annotation = life_stage_annotation,
      quiet = TRUE
    )

    # De momento, solo comprobamos que devuelve un data.frame.
    # Cuando creemos el mock, ajustaremos el número de filas.
    expect_s3_class(obs_bounds_annot, "data.frame")

  })
})
