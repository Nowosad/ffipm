#' Extract Data List
#'
#' @param file An ncdf file
#' @param dname A variable name. One of "Weekly Growth Index", "Hot Stress", "Dry Stress", "Wet Stress", "Cold Stress", "Ecoclimatic Index", "Moisture Index", "Temperature Index"
#' @param n A number of layers for each year. The default is 52
#' @param years A vector indicating years in the input file
#' @param ... Not used
#'
#' @return A nested list with spatio-thematic metadata.
#'  Each nested list contains a data for each year
#'  in a form of a set of matrices.
#' @export
#'
#' @examples
#' \dontrun{
#'   Path <- "C:/Google Drive/Corvus Geostat - Jakub Nowosad/"
#'   Path <- "../corvus_dynamic_outputs/"
#'   input_file <- paste0(Path,"NetCDF/DxResults_Cc_2010-2015.nc")
#'   AllYears <- extract_data_list(input_file, "Weekly Growth Index",
#'                               years = 2000:2014)
#' }
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

  metadata <- list(lon = lon, lat = lat, dname = dname)
  attr(result, "metadata") <- metadata

  return(result)
}
