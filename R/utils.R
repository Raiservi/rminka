# utils.R

# Declaración de variables globales para evitar notas de R CMD check
utils::globalVariables(c(
  "geojson_string",
  "sf_geometry",
  ".data",                   # Para las expresiones de dplyr/purrr
  "id",                      # Columna 'id' del proyecto
  "title",                   # Columna 'title' del proyecto
  "description",             # Columna 'description' del proyecto
  "slug",                    # Columna 'slug' del proyecto
  "place_id",                # Columna 'place_id' del proyecto
  "icon",                    # Columna 'icon' del proyecto
  "header_image_url"         # Columna 'header_image_url' del proyecto
))

#' Null-coalescing operator
#'
#' See \code{rlang::\link[rlang:op-null-default]{\%||\%}} for details.
#'
#' @name null-default
#' @aliases %||%
#' @rdname null-default
#' @keywords internal
#' @importFrom rlang %||%
NULL

