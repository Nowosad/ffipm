#' Title
#'
#' @param x
#' @param rasters
#'
#' @return
#' @export
#'
#' @examples
trap_model_plot <- function(x, rasters){
  x$type <- attr(x, "ylab")
  all_years <- sort(unique(lubridate::year(x$time)))
  x_lat <- unique(x$lat)
  x_lon <- unique(x$lon)

  x_model <- create_time_series(rasters,
                                years = all_years,
                                x = x_lon, y = x_lat)

  x_model <- dplyr::filter(x_model,
                           date >= min(x$time),
                           date <= max(x$time))
  names(x_model) <- c("time", "value" )
  x_model$lat <- x_lat
  x_model$lon <- x_lon
  x_model$type <- attr(rasters, "dname")

  all_x = rbind(x, x_model)

  ggplot2::ggplot(all_x) +
    ggplot2::geom_line(ggplot2::aes(time, value)) +
    ggplot2::facet_wrap(~type, scales = "free_y") +
    ggplot2::labs(title = attr(x, "location")) +
    theme_cg()
}

Path <- "./"
input_file <- paste0(Path,"NetCDF/DxResults_Cc_2010-2015.nc")
AllYears <- extract_data_list(input_file, "Weekly Growth Index",
                              years = 2000:2014)

rasters <- create_raster_stack(AllYears, years = 2000:2014)

# filename <- "2002_Medfly_"; species <- "Cc"
filename <- "Bjelis_2007_1"; species <- "Cc"
x <- read_pd(filename, species)

trap_model_plot(read_pd(5), rasters)
