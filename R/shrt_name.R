#' Generate a Short Name from a Scientific Name
#'
#' From a full scientific name, this function creates a
#' standard abbreviation by taking the first three letters of each word, in
#' lowercase, and joining them with a period.
#'
#' @param scientific_name A character string or vector of character strings with
#' the scientific names.
#' @return A character string or vector of character strings with the
#' abbreviations.

#' @examples\dontrun{
#' shrt_name("Diplodus sargus")
#' shrt_name("Diplodus sargus sargus")
#' shrt_name(c("Diplodus cervinus", "Diplodus vulgaris","Diplodus sargus"))
#' }
#' @export
#'

shrt_name <- function(scientific_name) {



  if (is.numeric(scientific_name)) {
    stop("Input cannot be a number. Please provide a character string.", call. = FALSE)
  }


  if (any(is.na(scientific_name) | stringr::str_trim(scientific_name) == "")) {
    stop("Input cannot contain NA or empty strings.", call. = FALSE)
  }

  if (is.null(scientific_name) || !is.character(scientific_name) || length(scientific_name) == 0) {
    stop("Input must be a non-empty character string or vector.", call. = FALSE)
  }

  word_counts <- stringr::str_count(stringr::str_trim(scientific_name), " ") + 1
  if (any(word_counts < 1 | word_counts > 3)) {
    stop("Each scientific name must contain between 1 and 3 words.", call. = FALSE)
  }


  result <- stringr::str_to_lower(scientific_name) %>%
    stringr::str_trim() %>%
    stringr::str_split(" ") %>%
    purrr::map_chr(~ paste(stringr::str_sub(.x, 1, 3), collapse = "."))

  return(result)
}
