#' @description
#' Import and clean NOAA NCEP North American Regional Reanalysis (NARR) data
#' downloaded with `download_narr` or `download_data(dataset_name = "NARR")`.
#' Function returns a SpatRast object containing the user-defined variable
#' of interest. Layer names indicate the variable, pressure level, and date
#' (YYYYMMDD).
#' @param date_start character(1). length of 10. Format "YYYY-MM-DD".
#' @param date_end character(1). length of 10. Format "YYYY-MM-DD".
#' @param variable character(1). NARR variable name(s).
#' @param directory_with_data character(1). Directory with downloaded GEOS-CF
#' netCDF files.
#' @author Mitchell Manware
#' @return a SpatRaster object
#' @importFrom terra rast
#' @export
import_narr <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    variable = NULL,
    directory_with_data = "../../data/covariates/narr/") {
  #### directory setup
  directory_with_data <- download_sanitize_path(directory_with_data)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### identify file paths
  data_paths <- list.files(
    directory_with_data,
    pattern = variable,
    full.names = TRUE
  )
  data_paths <- data_paths[grep(
    ".nc",
    data_paths
  )]
  #### define date sequence
  date_sequence <- generate_date_sequence(
    date_start,
    date_end,
    sub_hyphen = TRUE
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p])
    cat(paste0(
      "Cleaning data for year ",
      substr(
        gsub(
          "-",
          "",
          terra::time(data_year)[1]
        ),
        1,
        4
      ),
      "...\n"
    ))
    #### check for mono or pressure levels
    if (grepl("level", names(data_year)[1])) {
      #### pressure levels data
      names(data_year) <- paste0(
        variable,
        "_",
        sapply(
          strsplit(
            names(data_year),
            "_"
          ),
          function(x) x[2]
        ),
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    } else {
      #### mono level data
      names(data_year) <- paste0(
        variable,
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    }
    data_full <- c(
      data_full,
      data_year,
      warn = FALSE
    )
  }
  #### subset years to dates of interest
  data_return <- terra::subset(
    data_full,
    which(
      substr(
        names(data_full),
        nchar(names(data_full)) - 7,
        nchar(names(data_full))
      ) %in% date_sequence
    )
  )
  cat(paste0(
    "Returning daily ",
    variable,
    " data from ",
    date_sequence[1],
    " to ",
    date_sequence[length(date_sequence)],
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}

#' @description
#' Import and clean GEOS-CF data downloaded with
#' `download_geos_cf_data` or `download_data(dataset_name = "geos")`. Function
#' returns a SpatRast object containing the user-defined variables of interest.
#' Layer names indicate the variable, pressure level, date (YYYYMMDD), and, if
#' applicable, the hour (HHMMSS).
#' @param date_start character(1). length of 10. Format "YYYY-MM-DD".
#' @param date_end character(1). length of 10. Format "YYYY-MM-DD".
#' @param collection character(1). GEOS-CF data collection file name.
#' @param variable character(1). GEOS-CF variable name(s).
#' @param daily logical(1). Calculate daily value from hourly GEOS-CF
#' observations.
#' @param daily_fun character(1). Function used to summarize hourly or three
#' hourly observations into daily statistic. (Default = `mean`).
#' @param directory_with_data character(1). Directory with downloaded GEOS-CF
#' netCDF files.
#' @author Mitchell Manware
#' @return a SpatRaster object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra timeInfo
#' @importFrom terra hasValues
#' @importFrom terra subset
#' @export
import_geos <-
  function(date_start = "2023-09-01",
           date_end = "2023-09-01",
           collection =
           c(
             "htf_inst_15mn_g1440x721_x1", "aqc_tavg_1hr_g1440x721_v1",
             "chm_tavg_1hr_g1440x721_v1", "met_tavg_1hr_g1440x721_x1",
             "xgc_tavg_1hr_g1440x721_x1", "chm_inst_1hr_g1440x721_p23",
             "met_inst_1hr_g1440x721_p23"
           ),
           variable = NULL,
           daily = TRUE,
           daily_fun = "mean",
           directory_with_data = "../../data/covariates/geos_cf/") {
    #### directory setup
    directory_with_data <- download_sanitize_path(directory_with_data)
    #### check for variable
    check_for_null_parameters(mget(ls()))
    #### match user collection
    collection <- match.arg(collection)
    #### define date sequence
    date_sequence <- generate_date_sequence(
      date_start,
      date_end,
      sub_hyphen = TRUE
    )
    #### define time sequence
    collection_end <- substr(collection, nchar(collection), nchar(collection))
    if (collection_end == "1") {
      time_sequence <- seq(from = 30, to = 2330, by = 100)
    } else if (collection_end == "3") {
      time_sequence <- seq(from = 0, to = 2300, by = 100)
    }
    time_sequence <- sprintf("%04d", time_sequence)
    #### initiate for loop
    data_return <- terra::rast()
    for (d in seq_along(date_sequence)) {
      date <- date_sequence[d]
      data_date <- terra::rast()
      cat(paste0(
        "Cleaning data for date ",
        date,
        "...\n"
      ))
      for (t in seq_along(time_sequence)) {
        #### define path to hourly data
        path <- paste0(
          directory_with_data,
          "/GEOS-CF.v01.rpl.",
          collection,
          ".",
          date,
          "_",
          time_sequence[t],
          "z.nc4"
        )
        #### import .nc4 data
        data_raw <- terra::rast(path)
        #### subset to user-selected variable
        data_variable <- terra::subset(
          data_raw,
          subset = grep(
            variable,
            names(data_raw)
          )
        )
        #### define variable time
        terra::time(data_variable) <- rep(
          ISOdate(
            year = substr(date, 1, 4),
            month = substr(date, 5, 6),
            day = substr(date, 7, 8),
            hour = substr(
              time_sequence[t],
              1,
              2
            ),
            min = substr(
              time_sequence[t],
              3,
              4
            ),
            sec = 00,
            tz = "UTC"
          ),
          terra::nlyr(data_variable)
        )
        #### define variable name based on date and time
        names(data_variable) <- paste0(
          names(data_variable),
          "_",
          gsub(
            ":", "",
            gsub(
              "-", "",
              gsub(" ", "_", terra::time(data_variable))
            )
          )
        )
        if (t == 1 && collection_end == "3") {
          names(data_variable) <- paste0(
            names(data_variable),
            "_",
            time_sequence[t],
            "00"
          )
        }
        #### combine data with same date
        data_date <- c(
          data_date,
          data_variable,
          warn = FALSE
        )
      }
      if (daily == TRUE) {
        cat(paste0(
          "Returning daily ",
          daily_fun,
          " ",
          variable,
          " data for date ",
          date,
          ".\n"
        ))
        if (collection_end == "1") {
          #### summarize by day
          data_fun <- do.call(
            get(
              daily_fun,
              asNamespace("terra")
            ),
            list(
              data_date,
              na.rm = TRUE
            )
          )
          #### set variable names
          terra::varnames(data_fun) <- paste0(variable)
          #### set layer names
          names(data_fun) <- paste0(
            variable,
            "_",
            date
          )
          #### set time
          terra::time(data_fun) <- as.Date(
            date,
            format = "%Y%m%d"
          )
          #### set coordinate reference system
          terra::crs(data_fun) <- "EPSG:4326"
          #### combine data in temporal range
          data_return <- c(
            data_return,
            data_fun,
            warn = FALSE
          )
        } else if (collection_end == "3") {
          #### identify pressure levels
          levels_split <- unlist(
            strsplit(
              names(data_date),
              "_2"
            )
          )
          levels_unique <- unique(
            levels_split[
              which(
                startsWith(levels_split, variable)
              )
            ]
          )
          data_fun <- terra::rast()
          for (l in seq_along(levels_unique)) {
            #### subset to single pressure level
            data_level <- terra::subset(
              data_date,
              subset = grep(
                levels_unique[l],
                names(data_date)
              )
            )
            #### calculate summary statistic
            data_level_fun <- do.call(
              get(
                daily_fun,
                asNamespace("terra")
              ),
              list(
                data_level,
                na.rm = TRUE
              )
            )
            #### set variable names
            terra::varnames(data_level_fun) <- paste0(variable)
            #### set layer names
            names(data_level_fun) <- paste0(
              levels_unique[l],
              "_",
              date
            )
            #### set time
            terra::time(data_level_fun) <- as.Date(
              date,
              format = "%Y%m%d"
            )
            #### set coordinate reference system
            terra::crs(data_level_fun) <- "EPSG:4326"
            #### combine data in temporal range
            data_return <- c(
              data_return,
              data_level_fun,
              warn = FALSE
            )
          }
        }
      } else if (daily == FALSE) {
        cat(paste0(
          "Returning hourly ",
          variable,
          " data for date ",
          date,
          ".\n"
        ))
        #### set coordinate reference system
        terra::crs(data_date) <- "EPSG:4326"
        #### combine data in temporal range
        data_return <- c(
          data_return,
          data_date,
          warn = FALSE
        )
      }
    }
    #### return SpatRaster
    return(data_return)
  }
