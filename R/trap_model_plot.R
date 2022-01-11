#' Trap Model Plot
#'
#' @param x An output from [read_pd()]
#' @param rasters An output from [create_raster_stack()]
#' @param full_year Should we use observation for the whole year (TRUE) or just for the dates from x (FALSE). The default is FALSE.
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
trap_model_plot <- function(x, rasters, full_year = FALSE){
  location <- attr(x, "location")
  x$variable <- attr(x, "ylab")
  if (length(attr(x, "years")) > 0){
  all_years <- attr(x, "years")
  } else {
    all_years <- sort(unique(lubridate::year(x$time)))
  }
  x_lat <- unique(x$lat)
  x_lon <- unique(x$lon)

  x_model <- create_time_series(rasters,
                                years = all_years,
                                x = x_lon, y = x_lat)

  if (length(attr(x, "years")) > 0){
    x_model = x_model %>%
      dplyr::group_by(week = lubridate::isoweek(lubridate::ymd(.data$time)), .data$variable) %>%
      dplyr::summarize(value = mean(.data$value)) %>%
      dplyr::mutate(time = as.Date(strptime(paste0("2018", stringr::str_sub(paste0("0", .data$week), start= -2), "1"), "%Y%W%u"))) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$time, .data$value, .data$variable)

    x_model_2 <- zoo::na.locf(merge(xts::xts(x_model$value, x_model$time), xts::xts(, seq(from = (as.Date("2018-01-01")), (as.Date("2018-12-31")), "days"))))
    x_model.df <- data.frame(time=zoo::index(x_model_2), zoo::coredata(x_model_2), variable = x_model$variable[1])
    colnames(x_model.df) <- c("time", "value", "variable")
    x_model <- x_model.df %>%
      dplyr::group_by(month = lubridate::month(.data$time), .data$variable) %>%
      dplyr::summarize(value = mean(.data$value)) %>%
      dplyr::mutate(time = as.character(as.Date(paste(attr(x, "years")[[1]], .data$month, "15", sep = "-")))) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$time, .data$value, .data$variable)

    date_format <- "%b"
  } else {
  if (!full_year) {
    x_model <- dplyr::filter(x_model,
                             .data$time >= min(x$time),
                             .data$time <= max(x$time))
  }
    date_format <- "%b %Y"
  }

  x <- x[c(1, 2, 5, 3, 4)]

  x_model$lat <- x_lat
  x_model$lon <- x_lon

  all_x = rbind(x, x_model)

  ggplot2::ggplot(all_x) +
    ggplot2::geom_line(ggplot2::aes(.data$time, .data$value)) +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::labs(title = location) +
    ggplot2::scale_x_date(labels=scales::date_format(date_format)) +
    theme_cg()
}

