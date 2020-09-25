#' Trap Plot
#'
#' @param df The output of the [read_pd()] function
#' @param type A type of the output plot. Either "line" or "bar"
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'   maindir <- "../corvus_dynamic_outputs/Digitising FF trapping plots/"
#'   filename <- "Bjelis_2007_1"; species <- "Cc"
#'   x <- read_pd(filename, species, maindir)
#'   trap_plot(x)
#'   trap_plot(x, "bar")
#'   trap_plot(x) + ggplot2::labs(title = "Title", x = "X", y = "Y")
#' }
trap_plot <- function(df, type = "line"){
  if (type == "line"){
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$time, .data$value)) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  } else if (type == "bar") {
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$time, .data$value)) +
      ggplot2::geom_col()
  }
  date_breaks_style <- which_x_scale(attr(df, "xlab"))
  p <- p +
    theme_cg() +
    ggplot2::labs(x = attr(df, "xlab"),
                  y = attr(df, "ylab"),
                  title = attr(df, "location")) +
    ggplot2::scale_x_date(date_labels = date_breaks_style)
  return(p)
}

# plot Plot Digitizer -----------------------------------------------------
which_x_scale = function(type){
  # https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
  if (type == "date"){
    "%m-%Y"
  } else if (type == "day") {
    "%m-%Y"
  } else if (type == "week"){
    "%m-%Y"
  } else if (type == "month"){
    "%m-%Y" #"%b"
  } else if (type == "year"){
    "%Y"
  } else if (type == "month_avg"){
    "%m"
  } else if (type == "fortnight"){
    "%m-%Y"
  }
}

