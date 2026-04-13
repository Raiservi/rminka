#' @title Generate Short Name from Scientific Name
#' @description Creates a standardized abbreviation from a scientific name by
#'   taking the first three letters of each word, converting to lowercase, and
#'   joining with periods. Accepts names with one to three words.
#' @param scientific_name A character vector of scientific names. Each element
#'   must contain one to three words. Cannot contain `NA` or empty strings.
#' @return A character vector of the same length as `scientific_name`. Each
#'   element contains the abbreviation formed by the first three lowercase
#'   letters of each word, separated by periods. For example, "Diplodus sargus"
#'   returns "dip.sar" and "Diplodus sargus sargus" returns "dip.sar.sar".
#'   Signals an error if input is not character, is empty, contains `NA`, or
#'   has more than three words.
#' @examples
#' shrt_name("Diplodus sargus")
#' shrt_name("Diplodus sargus sargus")
#' shrt_name(c("Diplodus cervinus", "Diplodus vulgaris", "Diplodus sargus"))
#' @export
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
