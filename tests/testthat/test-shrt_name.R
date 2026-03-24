# tests/testthat/test-shrt_name.R

# Primero, probamos que la función se carga.
# El helper-*.R es un buen sitio si necesitas funciones auxiliares para tus tests.
# En este caso, devtools::load_all() es suficiente.

test_that("Function produces correct abbreviations for valid names", {

  # Caso 1: Nombre estándar de 2 palabras
  expect_equal(shrt_name("Diplodus sargus"), "dip.sar")

  # Caso 2: Nombre de 3 palabras
  expect_equal(shrt_name("Diplodus sargus sargus"), "dip.sar.sar")

  # Caso 3: Nombre de 1 palabra
  expect_equal(shrt_name("Diplodus"), "dip")

  # Caso 4: Manejo de mayúsculas inconsistentes
  expect_equal(shrt_name("DIPLODUS SarGUS"), "dip.sar")

  # Caso 5: Manejo de espacios en blanco extra al principio o al final
  expect_equal(shrt_name("  Diplodus sargus  "), "dip.sar")
})

test_that("Function handles vectors of names correctly", {

  # Vector con varios nombres válidos
  input_vector <- c("Diplodus sargus", "Diplodus cervinus",
                    "Diplodus sargus sargus", "Diplodus")
  expected_output <- c("dip.sar", "dip.cer", "dip.sar.sar","dip")

  expect_equal(shrt_name(input_vector), expected_output)
})

test_that("Function stops with an error for invalid input types", {

  # Entrada numérica
  expect_error(
    shrt_name(123),
    "Input cannot be a number. Please provide a character string."
  )

  # Entrada NULL
  expect_error(
    shrt_name(NULL),
    "Input must be a non-empty character string or vector."
  )


  expect_error(
    shrt_name(TRUE),
    "Input must be a non-empty character string or vector."
  )
})

test_that("Function stops with an error for empty, NA, or blank inputs", {

  # Entrada NA
  expect_error(
    shrt_name(NA),
    "Input cannot contain NA or empty strings."
  )

  expect_error(
    shrt_name(""),
    "Input cannot contain NA or empty strings."
  )

  expect_error(
    shrt_name("   "),
    "Input cannot contain NA or empty strings."
  )

  # Vector que contiene un NA
  expect_error(
    shrt_name(c("Homo sapiens", NA)),
    "Input cannot contain NA or empty strings."
  )
})

test_that("Function stops with an error for incorrect number of words", {

  # Demasiadas palabras (4)
  expect_error(
    shrt_name("one two trhee four"),
    "Each scientific name must contain between 1 and 3 words."
  )

  # También probamos con un vector donde uno de los nombres es demasiado largo
  expect_error(
    shrt_name(c("Diplodus sargus", "one two trhee four")),
    "Each scientific name must contain between 1 and 3 words."
  )
})
