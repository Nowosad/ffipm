#' Title
#'
#' @param rasters
#' @param x
#' @param y
#' @param years
#' @param start_week
#' @param end_week
#'
#' @return
#' @export
#'
#' @examples
#' # improve time handling
# dname <- "Weekly Growth Index"  # GI
# dname <- "Hot Stress"
# dname <- "Dry Stress"
# dname <- "Wet Stress"
# dname <- "Cold Stress"
# dname <- "Ecoclimatic Index"    # EI
# dname <- "Moisture Index"
# dname <- "Temperature Index"
# Path <- "C:/Google Drive/Corvus Geostat - Jakub Nowosad/"
# Path <- "./"
# input_file <- paste0(Path,"NetCDF/DxResults_Cc_2010-2015.nc")
# AllYears <- extract_data_list(input_file, "Weekly Growth Index",
#                               years = 2000:2014)
#
# rasters <- create_raster_stack(AllYears, years = 2000:2014)
#
# cts <- create_time_series(rasters,
#                           years = 2000:2014,
#                           x = 3.57, y = 50.35)

# library(ggplot2)
# ggplot(cts, aes(date, V1)) + geom_line()
create_time_series <- function(rasters,
                               x = 3.57, y = 50.35,
                               # n = 52,
                               years,
                               start_week = 1, end_week = 52){

  sel_layers <- which(as.numeric(substr(names(rasters), 2, 5)) %in% years)
  rasters <- raster::subset(rasters, sel_layers)

  rr <- raster::extract(rasters, matrix(c(x, y), ncol = 2),
                        na.rm = FALSE)
  rr <- as.data.frame(t(rr))

  result <- names_to_dates(names(rasters))
  result <- cbind(result, rr)
  return(result)
}

names_to_dates <- function(x){
  year <- as.numeric(substr(x, 2, 5))
  week <- as.numeric(substr(x, 6, 7))
  wdate <- paste0(year, "-W", sprintf("%02d", week), "-3")
  data.frame(date = ISOweek::ISOweek2date(wdate))
}
