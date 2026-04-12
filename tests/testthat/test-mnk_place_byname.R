mock_response_json_success_multiple <- '{
  "results": [
    {"id":"123", "slug":"area-marina-sant-feliu", "name":"Area marina Sant Feliu", "display_name":"Area marina Sant Feliu de Guíxols", "location":"41.7597741479,3.0226481505", "bbox_area":0.005301438053870476},
    {"id":"456", "slug":"otra-area", "name":"Otra Area", "display_name":"Otra Area en alguna parte", "location":"40.00,2.00", "bbox_area":0.01}
  ]
}'
mock_response_json_success_single <- '{
  "results": [
    {"id":"100", "slug":"single-place", "name":"Single Place", "display_name":"Just one place here", "location":"30.00,1.00", "bbox_area":0.0075}
  ]
}'
mock_response_json_empty_results <- '{"results": []}'
mock_response_json_no_results_key <- '{"some_other_key": "some_value"}'

test_that("mnk_place_byname throws error for invalid query", {
  expect_error(mnk_place_byname(NULL), "You must provide a non-empty 'query' string.")
  expect_error(mnk_place_byname(""), "You must provide a non-empty 'query' string.")
  expect_error(mnk_place_byname(123), "You must provide a non-empty 'query' string.")
  expect_error(mnk_place_byname(" "), "You must provide a non-empty 'query' string.")
})

test_that("mnk_place_byname returns a tibble for a valid query", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param <- utils::URLdecode(sub(".*q=", "", path))
    response_content <- if (grepl("Area marina Sant Feliu", query_param, ignore.case = TRUE)) {
      mock_response_json_success_multiple
    } else {
      mock_response_json_success_single
    }
    structure(list(status_code = 200L, headers = list(`Content-Type`="application/json"),
                   content = charToRaw(response_content)), class = c("response","handle"))
  }
  with_mocked_bindings(GET = mock_httr_GET,.package = "httr", {
    result_multiple <- mnk_place_byname("Area marina Sant Feliu")
    expect_s3_class(result_multiple, "tbl_df")
    expect_equal(nrow(result_multiple), 2)
    expect_equal(result_multiple$place_id, c(123L, 456L))

    result_single <- mnk_place_byname("Single Place")
    expect_equal(nrow(result_single), 1)
  })
})

test_that("mnk_place_byname handles no results from API", {
  mock_httr_GET <- function(url=NULL,...,path=NULL,as) {
    structure(list(status_code=200L, headers=list(`Content-Type`="application/json"),
                   content=charToRaw(mock_response_json_empty_results)), class=c("response","handle"))
  }
  with_mocked_bindings(GET = mock_httr_GET,.package="httr", {
    expect_message(result <- mnk_place_byname("No existente"), "No places found for your query.")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

test_that("mnk_place_byname handles API HTTP error", {
  mock_httr_GET <- function(...) {
    structure(list(status_code=500L, content=charToRaw('{}')), class=c("response","handle"))
  }
  with_mocked_bindings(GET = mock_httr_GET,.package="httr", {
    expect_message(result_500 <- mnk_place_byname("error_query_500"), "Minka API request failed. Status code: 500")
    expect_null(result_500)
  })
})

test_that("mnk_place_byname handles malformed JSON or missing 'results' key", {
  mock_httr_GET <- function(url=NULL,...,path=NULL,as) {
    query <- utils::URLdecode(sub(".*q=", "", path))
    content <- switch(query,
                      "No results key" = mock_response_json_no_results_key,
                      "Malformed location" = '{"results":[{"location":"invalid"}]}',
                      "Location no numeric" = '{"results":[{"location":"41.78,abc"}]}',
                      mock_response_json_empty_results
    )
    structure(list(status_code=200L, headers=list(`Content-Type`="application/json"),
                   content=charToRaw(content)), class=c("response","handle"))
  }
  with_mocked_bindings(GET = mock_httr_GET,.package="httr", {
    expect_message(res <- mnk_place_byname("No results key"), "No places found")
    expect_equal(nrow(res), 0)

    # ahora no hace stop, devuelve NA
    res_bad <- mnk_place_byname("Malformed location")
    expect_true(is.na(res_bad$location_latitud))

    res_nonum <- mnk_place_byname("Location no numeric")
    expect_true(is.na(res_nonum$location_longitud))
  })
})
