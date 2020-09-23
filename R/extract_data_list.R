#' Title
#'
#' @param file
#' @param dname
#' @param n
#' @param years
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
extract_data_list <- function(file, dname, n = 52, years, ...){
  climexncdf <- ncdf4::nc_open(file)
  lon <- ncdf4::ncvar_get(climexncdf, "Longitude")
  lat <- ncdf4::ncvar_get(climexncdf, "Latitude")
  t <- ncdf4::ncvar_get(climexncdf, "Step")
  nlon <- dim(lon)
  tmp_array <- ncdf4::ncvar_get(climexncdf, dname)

  tmp_stack <- vector("list", length(t))  # Create a temporary stack
  rm(climexncdf) # Remove from memory large ncdf file

  # Populate temporary stack
  for (i in seq_along(t)) {
    tmp_stack[[i]] <- tmp_array[, , i]
  }
  # Reorder values in temp stack
  for (i in seq_along(t)) {
    tmp_stack[[i]] <- apply(t(tmp_stack[[i]][, order(lat)]), 2, rev)
  }
  if (missing(years)){
    years <- seq_len(length(t) / n)
  }
  result <- create_data_list(tmp_stack, n = n, years = years)

  metadata <- list(lon = lon, lat = lat)
  attr(result, "metadata") <- metadata
  attr(result, "dname") <- dname

  return(result)
}
