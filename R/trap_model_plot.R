#' Trap Model Plot
#'
#' @param x An output from [read_pd()]
#' @param rasters An output from [create_raster_stack()]
#'
#' @return A ggplot object
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
#'   maindir <- "../corvus_dynamic_outputs/Digitising FF trapping plots/"
#'   filename <- "Bjelis_2007_1"; species <- "Cc"
#'   x <- read_pd(filename, species, maindir)
#'
#'   trap_model_plot(x, rasters)
#' }
trap_model_plot <- function(x, rasters){
  x$variable <- attr(x, "ylab")
  # if (length(attr(x, "years")) > 0){
    # all_years <- attr(x, "years")
  # } else {
    all_years <- sort(unique(lubridate::year(x$time)))
  # }
  x_lat <- unique(x$lat)
  x_lon <- unique(x$lon)
  x <- x[c(1, 2, 5, 3, 4)]

  x_model <- create_time_series(rasters,
                                years = all_years,
                                x = x_lon, y = x_lat)

  # if (length(attr(x, "years")) > 0){
  #   x_model = x_model %>%
  #     dplyr::group_by(doy = lubridate::yday(time), variable) %>%
  #     dplyr::summarize(value = mean(value)) %>%
  #     dplyr::mutate(time = as.Date(doy - 1, origin = paste0(all_years[[1]], "-01-01"))) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(time, value, variable)
  # } else {
    x_model <- dplyr::filter(x_model,
                             .data$time >= min(x$time),
                             .data$time <= max(x$time))
  # }

  x_model$lat <- x_lat
  x_model$lon <- x_lon

  all_x = rbind(x, x_model)

  ggplot2::ggplot(all_x) +
    ggplot2::geom_line(ggplot2::aes(.data$time, .data$value)) +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::labs(title = attr(x, "location")) +
    theme_cg()
}

