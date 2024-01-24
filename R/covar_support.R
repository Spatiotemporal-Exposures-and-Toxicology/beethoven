#' Create circular buffer around site points.
#' @param sites SpatVector(1). SpatVector object with point geometry
#' @param buffer integer(1). Circular buffer size (meters).
#' @description Creates a circular buffer around points if `buffer` is > 0.
#' Returns points if `buffer` is 0.
#' @returns SpatVector.
#' @importFrom terra buffer
#' @export
sites_buffer <-
  function(
    sites,
    buffer
  ) {
    cat(paste0(
      "Utilizing ",
      buffer,
      " meter buffer for covariate calculations.\n"
    ))
    if (buffer == 0) {
      return(sites)
    } else if (buffer > 0) {
      sites_buffer <- terra::buffer(
        sites,
        buffer
      )
      return(sites_buffer)
    }
  }