# Tests for data import functions

testthat::test_that("import_narr returns expected.", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  # expect function
  expect_true(
    is.function(import_narr)
  )
  for (v in seq_along(variables)) {
    narr <-
      import_narr(
        date_start = "2018-01-01",
        date_end = "2018-01-01",
        variable = variables[v],
        directory_with_data =
          paste0(
            "../testdata/narr/",
            variables[v]
          )
      )
    # expect output is SpatRaster
    expect_true(
      class(narr)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(narr)
    )
    # expect non-null coordinate reference system
    expect_false(
      is.null(terra::crs(narr))
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(narr)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(narr))
    )
    # expect dimensions according to levels
    if (variables[v] == "weasd") {
      expect_true(
        dim(narr)[3] == 1
      )
    } else if (variables[v] == "omega") {
      expect_true(
        dim(narr)[3] == 29
      )
    }
  }
})

testthat::test_that("import_geos returns expected.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  # expect function
  expect_true(
    is.function(import_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    geos <-
      import_geos(
        date_start = "2018-01-01",
        date_end = "2018-01-01",
        variable = "O3",
        directory_with_data = paste0(
          "../testdata/geos/",
          collection
        )
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
    # expect time dimension is POSIXt for hourly
    expect_true(
      "POSIXt" %in% class(terra::time(geos))
    )
    # expect seconds in time information
    expect_true(
      "seconds" %in% terra::timeInfo(geos)
    )
    # expect dimensions according to collectoin
    if (collection == "aqc_tavg_1hr_g1440x721_v1") {
      expect_true(
        dim(geos)[3] == 24
      )
    } else if (collection == "chm_inst_1hr_g1440x721_p23") {
      expect_true(
        dim(geos)[3] == 23 * 24
      )
    }
  }
})

testthat::test_that("import_geos expected errors.", {
  # expect error without variable
  expect_error(
    import_geos()
  )
  # expect error on directory without data
  expect_error(
    import_geos(
      variable = "O3",
      directory_with_data = "../../"
    )
  )
})

testthat::test_that("import support functions return expected.", {
  path <- list.files(
    "../testdata/geos/aqc_tavg_1hr_g1440x721_v1/",
    full.names = TRUE
    )
  expect_error(
    geos_strsplit(
      path = path,
      collection = TRUE,
      date = TRUE,
      datetime = TRUE
    )
  )
  path_split_d <- geos_strsplit(
    path = path,
    date = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_d)) == 8
  )
  path_split_dt <- geos_strsplit(
    path = path,
    datetime = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_dt)) == 12
  )
})



























