#' Identify GEOS-CF collection based on user-defined file paths
#' @param path character(1). File path to GEOS-CF data file.
#' @return character
#' @export
geos_collection <-
  function(
    path
  ) {
    split_geos <- unlist(
      strsplit(
        path,
        "GEOS-CF.v01.rpl."
      )
    )[2]
    split_period <- unlist(
      strsplit(
        split_geos,
        "\\."
      )
    )[1]
    return(split_period)
  }

#' Generate time sequence based on GEOS-CF data collection.
#' @param collection character(1). GEOS-CF data collection
#' @return vector
#' @export
generate_time_sequence <-
  function(
    collection
  ) {
    collection_end <- substr(collection, nchar(collection), nchar(collection))
    if (collection_end == "1") {
      ts <- seq(from = 30, to = 2330, by = 100)
    } else if (collection_end == "3") {
      ts <- seq(from = 0, to = 2300, by = 100)
    }
    time_sequence <- sprintf("%04d", ts)
    return(time_sequence)
  }