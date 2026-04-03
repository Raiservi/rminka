

create_fake_response <- function(payload) {


  raw_content <- charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))

  response <- list(
    status_code = 200,
    headers = list('Content-Type' = 'application/json'),
    content = raw_content # <-- El contenido debe estar en formato 'raw'
  )

  class(response) <- "response"
  return(response)
}
