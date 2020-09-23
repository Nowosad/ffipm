#' Title
#'
#' @param df
#' @param type
#'
#' @return
#' @export
#'
#' @examples
trap_plot <- function(df, type = "line"){
  source("R/theme_cg.R")
  # x <- read_pd(filename, species)
  if (type == "line"){
    p <- ggplot2::ggplot(df, ggplot2::aes(time, value)) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  } else if (type == "bar") {
    p <- ggplot2::ggplot(df, ggplot2::aes(time, value)) +
      ggplot2::geom_col()
  }
  date_breaks_style <- which_x_scale(attr(df, "xlab"))
  p <- p +
    theme_cg() +
    ggplot2::labs(x = attr(df, "xlab"),
                  y = attr(df, "ylab")) +
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
  } else if (type == "fortnight"){
    "%m-%Y"
  }
}

# type - "line" or "bar"


# setup ------------------------------------------------------------------
# filename <- "20_1b_1"; species <- "Bd"
# filename <- "Bjelis_2007_1"; species <- "Cc"
# x <- read_pd(filename, species)
#
# trap_plot(x)
# trap_plot(x, "bar")
# trap_plot(x) + ggplot2::labs(title = "Tytuł", x = "X", y = "Y")
