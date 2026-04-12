mock_response_json_success_point <- '{
  "total_results": 1,
  "results": [{
    "id": 265,
    "name": "Area marina Sant Feliu",
    "display_name": "Area marina Sant Feliu",
    "slug": "area-marina-sant-feliu",
    "uuid": "ee1ecbbf-2ae7-4551-b053-35eb5bb9f31e",
    "place_type": null,
    "admin_level": null,
    "bbox_area": 0.0053,
    "location": "41.7597741479,3.0226481505",
    "geometry_geojson": {"type":"Point","coordinates":[2.923,41.777]}
  }]
}'

mock_response_json_success_polygon <- '{
  "total_results": 1,
  "results": [{
    "id": 456,
    "name": "Parque Natural XYZ",
    "display_name": "Parque Natural XYZ",
    "slug": "parque-natural-xyz",
    "uuid": "some-uuid",
    "place_type": null,
    "admin_level": null,
    "bbox_area": 0.0053,
    "location": "10.0,10.0",
    "geometry_geojson": {"type":"Polygon","coordinates":[[[0,0],[0,1],[1,1],[1,0],[0,0]]]}
  }]
}'

mock_response_json_empty_results <- '{"total_results":0,"results":[]}'
mock_response_json_no_results_key <- '{"some_other_key":"value"}'
mock_response_json_results_no_geom <- '{"total_results":1,"results":[{"id":888,"name":"Place without geometry","slug":"place-no-geom","location":"10.0,10.0"}]}'
mock_response_malformed_geojson_coords <- '{"total_results":1,"results":[{"id":666,"name":"Malformed","location":"1.0,1.0","geometry_geojson":{"type":"Point","coordinates":["invalid","data"]}}]}'
mock_response_completely_malformed_json <- '{"total_results":1,"results":[{"id":777,"geometry_geojson":{"type":"Point","coordinates":[0,0]}'

test_that("mnk_place_sf handles invalid input 'place_id'", {
  expect_error(mnk_place_sf(NULL), "single non-empty numerical 'place_id'")
  expect_error(mnk_place_sf("abc"), "single non-empty numerical 'place_id'")
  expect_error(mnk_place_sf(c(1,2)), "single non-empty numerical 'place_id'")
  expect_error(mnk_place_sf(NA_real_), "single non-empty numerical 'place_id'")
})

test_that("mnk_place_sf returns sf with metadata", {
  skip_if_not_installed("httr"); skip_if_not_installed("sf"); skip_if_not_installed("stringr")

  mock_GET <- function(url=NULL,...,path=NULL){
    id <- as.numeric(stringr::str_extract(path, "[0-9]+$"))
    cnt <- switch(as.character(id),
                  "265"=mock_response_json_success_point,
                  "456"=mock_response_json_success_polygon,
                  '{"error":"Not Found"}')
    status <- if(id %in% c(265,456)) 200L else 404L
    structure(list(status_code=status, content=charToRaw(cnt)), class=c("response","handle"))
  }

  with_mocked_bindings(GET=mock_GET, .package="httr", {
    res <- mnk_place_sf(265)


    expect_s3_class(res, "sf")
    expect_equal(nrow(res), 1)
    expect_named(res, c("place_id","name","display_name","slug","uuid",
                        "place_type","admin_level","bbox_area","location","geometry"))


    expect_equal(res$place_id, 265L)
    expect_equal(res$name, "Area marina Sant Feliu")
    expect_equal(res$display_name, "Area marina Sant Feliu")
    expect_equal(res$slug, "area-marina-sant-feliu")
    expect_equal(res$uuid, "ee1ecbbf-2ae7-4551-b053-35eb5bb9f31e")
    expect_true(is.na(res$place_type))
    expect_true(is.na(res$admin_level))
    expect_equal(res$bbox_area, 0.0053)
    expect_equal(res$location, "41.7597741479,3.0226481505")


    expect_equal(sf::st_crs(res)$epsg, 4326)
    expect_true(sf::st_is(res, "POINT"))
    expect_equal(as.numeric(sf::st_coordinates(res)), c(2.923,41.777), tolerance=1e-3)


    res_poly <- mnk_place_sf(456)
    expect_true(sf::st_is(res_poly, "POLYGON"))
    expect_equal(res_poly$place_id, 456L)
  })
})

test_that("mnk_place_sf respects crs argument", {
  skip_if_not_installed("httr"); skip_if_not_installed("sf")

  mock_GET <- function(...) structure(list(status_code=200L, content=charToRaw(mock_response_json_success_point)), class=c("response","handle"))

  with_mocked_bindings(GET=mock_GET, .package="httr", {
    res_25831 <- mnk_place_sf(265, crs = 25831)
    expect_equal(sf::st_crs(res_25831)$epsg, 25831)

    expect_equal(res_25831$name, "Area marina Sant Feliu")

    res_null <- mnk_place_sf(265, crs = NULL)
    expect_equal(sf::st_crs(res_null)$epsg, 4326)
  })
})

test_that("mnk_place_sf handles empty and missing geometry", {
  skip_if_not_installed("httr"); skip_if_not_installed("sf")

  mock_GET <- function(url=NULL,...,path=NULL){
    id <- as.numeric(stringr::str_extract(path, "[0-9]+$"))
    cnt <- switch(as.character(id),
                  "999"="",
                  "888"=mock_response_json_empty_results,
                  "777"=mock_response_json_no_results_key,
                  "666"=mock_response_json_results_no_geom)
    structure(list(status_code=200L, content=charToRaw(cnt)), class=c("response","handle"))
  }

  with_mocked_bindings(GET=mock_GET, .package="httr", {
    expect_message(mnk_place_sf(999), "API returned an empty response")
    expect_message(mnk_place_sf(888), "No places found")
    expect_message(mnk_place_sf(777), "No places found")

    res_nogeom <- mnk_place_sf(666)
    expect_s3_class(res_nogeom, "sf")
    expect_equal(res_nogeom$place_id, 888L)
    expect_equal(res_nogeom$name, "Place without geometry")
    expect_true(sf::st_is_empty(res_nogeom))
  })
})

test_that("mnk_place_sf handles HTTP errors", {
  skip_if_not_installed("httr")
  mock_GET <- function(...) structure(list(status_code=500L, content=charToRaw('{}')), class=c("response","handle"))
  with_mocked_bindings(GET=mock_GET, .package="httr", {
    expect_message(mnk_place_sf(500), "Status code: 500")
  })
})

test_that("mnk_place_sf handles malformed JSON", {
  skip_if_not_installed("httr"); skip_if_not_installed("sf")
  mock_GET <- function(url=NULL,...,path=NULL){
    id <- as.numeric(stringr::str_extract(path, "[0-9]+$"))
    cnt <- if(id==666) mock_response_malformed_geojson_coords else mock_response_completely_malformed_json
    structure(list(status_code=200L, content=charToRaw(cnt)), class=c("response","handle"))
  }
  with_mocked_bindings(GET=mock_GET, .package="httr", {
    res_bad <- suppressWarnings(mnk_place_sf(666))
    expect_true(sf::st_is_empty(res_bad))
    expect_equal(res_bad$place_id, 666L)

    expect_error(mnk_place_sf(777), regexp="lexical|parse|unexpected|EOF")
  })
})
