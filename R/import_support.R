#' Identify GEOS-CF collection based on user-defined file paths
#' @param path character(1). File path to GEOS-CF data file.
#' @param collection logical(1). Identifies and returns GEOS-CF collection
#' name(s) based on provided file path(s).
#' @param date logical(1). Identifies and returns date sequence (YYYYMMDD) based
#' on provided file path(s).
#' @param datetime logical(1). Identifies and returns date time sequence
#' (YYYYMoMoDDHHMiMi) based on provided file path(s).
#' @return character
#' @export
geos_strsplit <-
  function(
    path,
    collection = FALSE,
    date = FALSE,
    datetime = FALSE
  ) {
    #### check for more than one true
    parameters <- c(collection, date, datetime)
    if (length(parameters[parameters == TRUE]) > 1) {
      stop(
        paste0(
          "Select one of 'collection', 'date', or 'datetime'.\n"
        )
      )
    }
    #### split full file path based on unique GEOS-CF character
    split_geos <- unlist(
      strsplit(
        path,
        "GEOS-CF.v01.rpl."
      )
    )
    #### split file path into collection, datetime, and "nc4"
    split_period <- unlist(
      strsplit(
        split_geos[
          which(
            endsWith(split_geos, ".nc4")
          )
        ],
        "\\."
      )
    )
    #### remove "nc4"
    split_wo_nc4 <- split_period[!split_period == "nc4"]
    #### create data frame
    split_df <- data.frame(
      split_wo_nc4[
        which(
          !(endsWith(
            split_wo_nc4,
            "z"
          ))
        )
      ],
      split_wo_nc4[
        which(
          endsWith(
            split_wo_nc4,
            "z"
          )
        )
      ]
    )
    #### colnames
    colnames(split_df) <- c("collection", "datetime")
    #### return only collection name
    if (collection == TRUE) {
      return(split_df$collection)
    }
    #### return date sequence
    if (date == TRUE) {
      split_dates <- substr(
        split_df$datetime,
        1,
        8
      )
      return(split_dates)
    }
    #### return datetime sequence
    if (datetime == TRUE) {
      split_datetime <- gsub(
        "_",
        "",
        gsub(
          "z",
          "",
          split_df$datetime
        )
      )
      return(split_datetime)
    }
  }
