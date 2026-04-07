    # TEST 1
    test_that("mnk_obs_byday handles small date ranges in one go", {
      httptest::with_mock_api({
        obs <- mnk_obs_byday(d1 = "2024-05-20", d2 = "2024-05-20", quiet = TRUE)
        expect_s3_class(obs, "tbl_df")
        expect_equal(nrow(obs), 1)
        expect_equal(obs$id, 101)
      })

    })
    #
    # # TEST 2
    # test_that("mnk_obs_byday subdivides requests when total results are large", {
    #   .mockPaths("mocks_byday/test2") # <--- Apunta a la nueva carpeta
    #   with_mock_api({
    #     obs <- mnk_obs_byday(d1 = "2024-04-01", d2 = "2024-04-02", quiet = TRUE)
    #     expect_s3_class(obs, "tbl_df")
    #     expect_equal(nrow(obs), 2)
    #     expect_equal(obs$id, c(201, 202))
    #   })
    #   .mockPaths(NULL) # Resetea
    # })

    # TEST 3
    test_that("mnk_obs_byday downloads a full month as a single chunk", {
      # Este test sigue usando la carpeta principal de mocks, donde están sus 100+ archivos
      httptest::with_mock_api({
        obs <- mnk_obs_byday(d1 = "2024-03-01", d2 = "2024-04-30", quiet = TRUE)
        expect_s3_class(obs, "tbl_df")
        expect_equal(nrow(obs), 2)
        expect_equal(obs$id, c(301, 302))
      })
    })
