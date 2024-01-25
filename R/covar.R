#' @description
#' Extract Global Multi-resolution Terrain Elevation Data (GMTED2010) data at
#' point locations using SpatRaster object from `import_gmted`. Function returns
#' a data frame containing GEOS-CF variable values at user-defined sites. Unique
#' column reflect statistic, resolution, and circular buffer.
#' @param data SpatRaster(1). Cleaned SpatRaster object that has been returned
#' from `import_gmted` containing Global Multi-resolution Terrain Elevation Data
#' (GMTED2010) data.
#' @param sites data.frame, characater to file path, SpatVector, or sf object.
#' @param identifier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param buffer integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @author Mitchell Manware
#' @return a data.frame object;
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
covar_gmted <- function(
    data,
    sites,
    identifier = NULL,
    buffer = 0,
    fun = "mean") {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- sites_vector(
    sites,
    terra::crs(data),
    buffer
  )
  #### site identifiers only
  sites_id <- subset(
    terra::as.data.frame(sites_e),
    select = identifier
  )
  #### layer name
  layer_name <- names(data)
  cat(paste0(
    "Calculating ",
    gmted_codes(
      substr(
        layer_name,
        1,
        2
      ),
      statistic = TRUE,
      invert = TRUE
    ),
    " covariates with ",
    gmted_codes(
      substr(
        layer_name,
        3,
        4
      ),
      resolution = TRUE,
      invert = TRUE
    ),
    " resolution data.\n"
  ))
  #### extract layer data at sites
  sites_extracted <- terra::extract(
    data,
    sites_e,
    fun = fun,
    method = "simple",
    ID = FALSE,
    bind = FALSE
  )
  #### merge with site_id and date (year)
  sites_extracted <- cbind(
    sites_id,
    sites_extracted
  )
  #### convert integer to numeric
  sites_extracted[, 2] <- as.numeric(sites_extracted[, 2])
  #### define column names
  colnames(sites_extracted) <- c(
    identifier,
    paste0(
      tolower(
        gsub(
          " ",
          "_",
          gmted_codes(
            substr(
              layer_name,
              1,
              2
            ),
            statistic = TRUE,
            invert = TRUE
          )
        )
      ),
      "_",
      substr(
        layer_name,
        3,
        4
      ),
      "_",
      buffer
    )
  )
  return(sites_extracted)
}

#' @description
#' Extract NOAA NCEP North American Regional Reanalysis (NARR) data at point
#' locations using SpatRaster object from `import_narr`. Function returns a data
#' frame containing GEOS-CF variable values at user-defined sites. Unique column
#' names reflect variable name, circular buffer, and vertical pressure level
#' (if applicable).
#' @param data SpatRaster(1). Cleaned SpatRaster object that has been returned
#' from `import_narr` containing NOAA NCEP North American Regional Reanalysis
#' variable data.
#' @param sites data.frame, characater to file path, SpatVector, or sf object.
#' @param identifier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param buffer integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @author Mitchell Manware
#' @return a data.frame object;
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
covar_narr <- function(
    data,
    sites,
    identifier = NULL,
    buffer = 0,
    fun = "mean") {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- sites_vector(
    sites,
    terra::crs(data),
    buffer
  )
  #### site identifiers only
  sites_id <- subset(
    terra::as.data.frame(sites_e),
    select = identifier
  )
  #### empty location data.frame
  sites_extracted <- NULL
  for (l in seq_len(terra::nlyr(data))) {
    #### select data layer
    data_layer <- data[[l]]
    #### extract layer names for variable, date, and pressure level
    data_name <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### monolevel data
    if (length(data_name) == 2) {
      layer_level <- "monolevel"
      layer_date <- as.Date(
        data_name[2],
        format = "%Y%m%d"
      )
      cat(paste0(
        "Calculating daily ",
        data_name[1],
        " covariates at ",
        layer_level,
        " for date ",
        layer_date,
        "...\n"
      ))
    } else if (length(data_name) == 3) {
      layer_level <- data_name[2]
      layer_date <- as.Date(
        data_name[3],
        format = "%Y%m%d"
      )
      cat(paste0(
        "Calculating daily ",
        data_name[1],
        " covariates at ",
        layer_level,
        " for date ",
        layer_date,
        "...\n"
      ))
    }
    #### extract layer data at sites
    sites_extracted_layer <- terra::extract(
      data_layer,
      sites_e,
      fun = fun,
      method = "simple",
      ID = FALSE,
      bind = FALSE
    )
    #### merge with site_id, datetime, pressure level
    sites_extracted_layer <- cbind(
      sites_id,
      layer_date,
      layer_level,
      sites_extracted_layer
    )
    #### define column names
    colnames(sites_extracted_layer) <- c(
      identifier,
      "date",
      "level",
      paste0(
        data_name[1],
        "_",
        buffer
      )
    )
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
    if (l == terra::nlyr(data)) {
      cat(paste0(
        "Returning ",
        data_name[1],
        " covariates.\n"
      ))
    }
  }
  #### return data.frame
  return(sites_extracted)
}

#' @description
#' Extract GEOS-CF data at point locations using SpatRaster object from
#' `import_geos`. Function returns a data frame containing GEOS-CF variable
#' values at user-defined sites. Unique column names reflect variable name,
#' circular buffer, and vertical pressure level (if applicable).
#'
#' @param data SpatRaster(1). Cleaned SpatRaster object that has been returned
#' from `import_geos` containing GEOS-CF variable data.
#' @param sites data.frame, characater to file path, SpatVector, or sf object.
#' @param identifier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param buffer integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @author Mitchell Manware
#' @return a data.frame object;
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
covar_geos <- function(
    data,
    sites,
    identifier = NULL,
    buffer = 0,
    fun = "mean") {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- sites_vector(
    sites,
    terra::crs(data),
    buffer
  )
  #### site identifiers
  sites_id <- subset(
    sites,
    select = identifier
  )
  #### empty location data.frame
  sites_extracted <- NULL
  for (l in seq_len(terra::nlyr(data))) {
    #### select data layer
    data_layer <- data[[l]]
    #### extract layer names for variable and datetime sequence
    data_name <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### set datetime based on selections in import_geos
    #### (2 = variable + date; 3 = variable + pressure level + date;
    ####  4 = variable + pressure level + date + time)
    if (length(data_name) == 2) {
      layer_datetime <- as.Date(
        data_name[2],
        format = "%Y%m%d"
      )
      layer_level <- "monolevel"
      cat(paste0(
        "Calculating daily ",
        data_name[1],
        " covariates for date ",
        layer_datetime,
        "...\n"
      ))
    } else if (length(data_name) == 3) {
      layer_datetime <- as.Date(
        data_name[3],
        format = "%Y%m%d"
      )
      layer_level <- data_name[2]
      cat(paste0(
        "Calculating daily ",
        data_name[1],
        " covariates at ",
        layer_level,
        " for date ",
        layer_datetime,
        "...\n"
      ))
    } else if (length(data_name) == 4) {
      layer_datetime <- ISOdate(
        year = substr(data_name[3], 1, 4),
        month = substr(data_name[3], 5, 6),
        day = substr(data_name[3], 7, 8),
        hour = substr(data_name[4], 1, 2),
        min = substr(data_name[4], 3, 4),
        sec = substr(data_name[4], 5, 6),
        tz = "UTC"
      )
      layer_level <- data_name[2]
      cat(paste0(
        "Calculating hourly ",
        data_name[1],
        " covariates at ",
        layer_level,
        " for date ",
        layer_datetime,
        "...\n"
      ))
    }
    #### extract layer data at sites
    sites_extracted_layer <- terra::extract(
      data_layer,
      sites_e,
      fun = fun,
      method = "simple",
      ID = FALSE,
      bind = FALSE
    )
    #### merge with site_id, datetime, pressure level
    sites_extracted_layer <- cbind(
      sites_id,
      layer_datetime,
      layer_level,
      sites_extracted_layer
    )
    #### define column names
    colnames(sites_extracted_layer) <- c(
      identifier,
      "date",
      "level",
      paste0(
        data_name[1],
        "_",
        buffer
      )
    )
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
    if (l == terra::nlyr(data)) {
      cat(paste0(
        "Returning ",
        data_name[1],
        " covariates.\n"
      ))
    }
  }
  #### return data.frame
  return(sites_extracted)
}
