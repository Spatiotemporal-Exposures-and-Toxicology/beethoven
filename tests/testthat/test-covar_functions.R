# Tests for covariate generation functions

testthat::test_that("covar_narr returns expected.", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  buffers <- c(0, 1000)
  sites <- readRDS("../testdata/sites_sample.RDS")
  # expect function
  expect_true(
    is.function(covar_narr)
  )
  for (v in seq_along(variables)) {
    variable <- variables[v]
    for (b in seq_along(buffers)) {
      narr <-
        import_narr(
          date_start = "2018-01-01",
          date_end = "2018-01-01",
          variable = variable,
          directory_with_data =
            paste0(
              "../testdata/narr/",
              variables[v]
            )
        )
      narr_covar <-
        covar_narr(
          data = narr,
          sites = sites,
          identifier = "site_id",
          buffer = buffers[b],
          fun = "mean"
        )
      # expect output is data.frame
      expect_true(
        class(narr_covar) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(narr_covar) == 4
      )
      # expect numeric value
      expect_true(
        class(narr_covar[, 4]) == "numeric"
      )
      # expect date column
      expect_true(
        class(narr_covar[, 2]) == "Date"
      )
    }
  }
})

testthat::test_that("covar_geos returns as expected.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  buffers <- c(0, 1000)
  daily_sum <- c(TRUE, FALSE)
  sites <- data.frame(readRDS("../testdata/sites_sample.RDS"))
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
              paste0(
                "../testdata/geos/",
                collection)
          )
        geos_covar <-
          covar_geos(
            data = geos,
            sites = sites,
            identifier = "site_id",
            buffer = buffers[b],
            fun = "mean"
          )
        # expect output is data.frame
        expect_true(
          class(geos_covar) == "data.frame"
        )
        # expect 4 columns
        expect_true(
          ncol(geos_covar) == 4
        )
        # expect numeric value
        expect_true(
          class(geos_covar[, 4]) == "numeric"
        )
        # expect date and time column
        if (daily == TRUE) {
          expect_true(
            "Date" %in% class(geos_covar$date)
          )
        } else if (daily == FALSE) {
          expect_true(
            "POSIXt" %in% class(geos_covar$date)
          )
        }
      }
    }
  }
})

testthat::test_that("sites_vector identifies data type and missing columns.", {
  withr::local_package("terra")
  sites <- readRDS("../testdata/sites_sample.RDS")
  narr <-
    import_narr(
      date_start = "2018-01-01",
      date_end = "2018-01-01",
      variable = "weasd",
      directory_with_data = "../testdata/narr/weasd/"
    )
  # expect error when missing `lat` or `lon`
  expect_error(
    covar_narr(
      data = narr,
      sites = subset(
        sites,
        select = "lon"
      ),
      identifier = "site_id"
    )
  )
  # expect error when sites are SpatVector
  expect_error(
    covar_narr(
      data = narr,
      sites = terra::vect(
        sites,
        geom = c("lon", "lat")
      ),
      identifier = "site_id"
    )
  )
})
