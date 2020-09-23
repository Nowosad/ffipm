#' Create Time Series
#'
#' @param rasters An output from [create_raster_stack()]
#' @param x An x coordinate (longitude)
#' @param y A y coordinate (latitude)
#' @param years A vector stating for which years the time series will be extracted
#' @param start_week A number of first week in the output RasterStack.
#'   The default is 1
#' @param end_week A number of last week in the output RasterStack.
#'   The default is 52
#'
#' @return A data.frame with three columns: time, value, variable.
#' @export
#'
#' @examples
#' \dontrun{
#'   Path <- "C:/Google Drive/Corvus Geostat - Jakub Nowosad/"
#'   Path <- "../corvus_dynamic_outputs/"
#'   input_file <- paste0(Path,"NetCDF/DxResults_Cc_2010-2015.nc")
#'   AllYears <- extract_data_list(input_file, "Weekly Growth Index",
#'                               years = 2000:2014)
#'   rasters <- create_raster_stack(AllYears, years = 2000:2014)
#'
#'   cts <- create_time_series(rasters,
#'                           years = 2000:2014,
#'                           x = 3.57, y = 50.35)
#'
#'   library(ggplot2)
#'   ggplot(cts, aes(time, value)) + geom_line()
#' }
create_time_series <- function(rasters,
                               x = 3.57, y = 50.35,
                               # n = 52,
                               years,
                               start_week = 1, end_week = 52){
  dname <- attr(rasters, "dname")

  sel_layers <- which(as.numeric(substr(names(rasters), 2, 5)) %in% years)
  rasters <- raster::subset(rasters, sel_layers)

  rr <- raster::extract(rasters, matrix(c(x, y), ncol = 2),
                        na.rm = FALSE)
  rr <- as.data.frame(t(rr))

  result <- names_to_dates(names(rasters))
  result <- cbind(result, rr)
  names(result) <- c("time", "value")
  result$variable <- dname
  return(result)
}

names_to_dates <- function(x){
  year <- as.numeric(substr(x, 2, 5))
  week <- as.numeric(substr(x, 6, 7))
  wdate <- paste0(year, "-W", sprintf("%02d", week), "-3")
  data.frame(date = ISOweek::ISOweek2date(wdate))
}
