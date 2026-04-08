library(testthat)
library(rminka)

# Grabar mocks si no existen
# options(httptest.mock.fail = FALSE)

# FORZAR un user-agent estándar para evitar bloqueos de la API
# httr::set_config(
#   httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36")
# )

test_check("rminka")
