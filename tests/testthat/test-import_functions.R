# Tests for data import functions

testthat::test_that("import_geos returns expected.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  daily <- c(TRUE, FALSE)
  # expect function
  expect_true(
    is.function(import_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    for (d in seq_along(daily)) {
      geos <-
        import_geos(
          date_start = "2018-01-01",
          date_end = "2018-01-01",
          collection = collection,
          variable = "O3",
          daily = daily[d],
          daily_fun = "mean",
          directory_with_data =
            paste0("NEEDS_RELATIVE_PATH_WITH_TEST_DATA",
                   collection)
        )
      # expect output is SpatRaster
      expect_true(
        class(geos)[1] == "SpatRaster"
      )
      # expect values
      expect_true(
        terra::hasValues(geos)
      )
      # expect non-null coordinate reference system
      expect_false(
        is.null(terra::crs(geos))
      )
      # expect lon and lat dimensions to be > 1
      expect_false(
        any(c(0, 1) %in% dim(geos)[1:2])
      )
      # expect non-numeric and non-empty time
      expect_false(
        any(c("", 0) %in% terra::time(geos))
      )
      # expect time dimension according to levels and daily
      if (daily[d] == TRUE) {
        expect_true(
          "Date" %in% class(terra::time(geos))
        )
        expect_true(
          "days" %in% terra::timeInfo(geos)
        )
        if (collection == "aqc_tavg_1hr_g1440x721_v1") {
          expect_true(
            dim(geos)[3] == 1
          )
        } else if (collection == "chm_inst_1hr_g1440x721_p23") {
          expect_true(
            dim(geos)[3] == 23
          )
        }
      } else {
        expect_true(
          "POSIXt" %in% class(terra::time(geos))
        )
        expect_true(
          "seconds" %in% terra::timeInfo(geos)
        )
        if (collection == "aqc_tavg_1hr_g1440x721_v1") {
          expect_true(
            dim(geos)[3] == 24
          )
        } else if (collection == "chm_inst_1hr_g1440x721_p23") {
          expect_true(
            dim(geos)[3] == 23*24
          )
        }
      }
    }
  }
})
