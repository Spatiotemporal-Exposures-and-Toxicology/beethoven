# Tests for covariate generation functions

testthat::test_that("covar_geos with hourly data.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  buffers <- c(0, 1000)
  daily_sum <- c(TRUE, FALSE)
  sites <- readRDS("../testdata/geos/sites_sample.RDS")
  # expect function
  expect_true(
    is.function(covar_geos)
  )
  for (d in seq_along(daily_sum)) {
    daily <- daily_sum[d]
    for (c in seq_along(collections)) {
      collection <- collections[c]
      for (b in seq_along(buffers)) {
        geos <-
          import_geos(
            date_start = "2018-01-01",
            date_end = "2018-01-01",
            collection = collection,
            variable = "O3",
            daily = daily,
            daily_fun = "mean",
            directory_with_data =
              paste0("../testdata/geos/", collection)
          )
        covar <-
          covar_geos(
            data = geos,
            sites = sites_sample,
            identifier = "site_id",
            crs = 4326,
            buffer = buffers[b],
            fun = "mean"
          )
        # expect output is data.frame
        expect_true(
          class(covar) == "data.frame"
        )
        # expect 4 columns
        expect_true(
          ncol(covar) == 4
        )
        # expect numeric value
        expect_true(
          class(covar[,4]) == "numeric"
        )
        # expect date and time column
        if (daily == TRUE) {
          expect_true(
            "Date" %in% class(covar$date)
          )
        } else if (daily == FALSE) {
          expect_true(
            "POSIXt" %in% class(covar$date)
          )
        }
      }
    }
  }
})
