context("mnk_proj_obs")

test_that("mnk_proj_obs calls mnk_obs with the correct basic parameters", {
  # 1. Creamos un "espía" (mock) que reemplazará a mnk_obs.
  #    Este espía no hace nada, solo registra con qué argumentos fue llamado.
  m <- mockery::mock()

  # 2. Usamos mockery::stub para reemplazar temporalmente la verdadera función
  #    mnk_obs por nuestro espía 'm', solo dentro del cuerpo de mnk_proj_obs.
  mockery::stub(where = mnk_proj_obs, what = "mnk_obs", how = m)

  # 3. Ejecutamos la función que queremos testear.
  mnk_proj_obs(project_id = 123, year = 2024)

  # 4. Verificamos que nuestro espía 'm' fue llamado una sola vez.
  mockery::expect_called(m, 1)

  # 5. Verificamos que fue llamado EXACTAMENTE con los argumentos que esperamos.
  #    Los argumentos no especificados (month, day, etc.) deben estar en sus valores por defecto.
  mockery::expect_args(m, 1,
                       project_id = 123,
                       year = 2024,
                       month = NULL,
                       day = NULL,
                       quiet = FALSE,
                       limit_download = TRUE
  )
})

test_that("mnk_proj_obs passes all optional parameters correctly", {
  m <- mockery::mock()
  mockery::stub(where = mnk_proj_obs, what = "mnk_obs", how = m)

  # Ejecutamos la función con todos los parámetros posibles
  mnk_proj_obs(
    project_id = 999,
    year = 2025,
    month = 8,
    day = 15,
    quiet = TRUE,
    limit_download = FALSE
  )

  mockery::expect_called(m, 1)

  # Verificamos que TODOS los valores, incluyendo los no-default, se pasaron correctamente.
  mockery::expect_args(m, 1,
                       project_id = 999,
                       year = 2025,
                       month = 8,
                       day = 15,
                       quiet = TRUE,
                       limit_download = FALSE
  )
})

test_that("mnk_proj_obs requires project_id and year", {
  # No hace falta simular nada aquí, solo comprobamos que R lanza el error esperado
  # si faltan los argumentos obligatorios.

  expect_error(
    mnk_proj_obs(year = 2024),
    "argument \"project_id\" is missing, with no default"
  )

  expect_error(
    mnk_proj_obs(project_id = 123),
    "argument \"year\" is missing, with no default"
  )
})
