#' Read PD
#'
#' @param filename Name of the file without extension as specified in the `File_name` column of the Diagrams_sumary - HG_zmiany.ods file
#' @param species "Cc", "Bd", or "Bz"
#' @param maindir A main directory containing subdirectories for each species
#' @param id Number of a row from the Diagrams_sumary - HG_zmiany.ods file.
#' Can be used instead of `filename` and `species`
#' @param ... Additional arguments for [utils::read.csv2()]
#'
#' @return A data.frame with four columns: time, value, lat, and lon.
#' It additionally contains several hidden attributes: xlab, ylab, and location.
#' @export
#'
#' @examples
#' \dontrun{
#'   maindir <- "../corvus_dynamic_outputs/Digitising FF trapping plots/"
#'   filename <- "Bjelis_2007_1"; species <- "Cc"
#'   x <- read_pd(filename, species, maindir)
#'   y <- read_pd(maindir = maindir, id = 3)
#' }
#'
read_pd <- function(filename, species, maindir = "Digitising FF trapping plots/",
                    id, ...){
  param_df <- readODS::read_ods(system.file("table/data_summary.ods", package = "ffipm"))

  if (!missing(id)){
    param_df <- param_df %>%
      dplyr::slice(id)

  } else {
    param_df <- param_df %>%
      dplyr::select(.data$Record_ID:.data$Data_type) %>%
      dplyr::filter(.data$File_name == filename,
                    .data$FF_species == species)
  }

  folder_name <- switch(param_df$FF_species,
                        Cc = "C. capitata - pliki csv",
                        Bd = "B. dorsalis - pliki csv",
                        Bz = "B. zonata - pliki csv")

  file_sel <- paste0(maindir,
                     folder_name, "/",
                     param_df$File_name,
                     ".csv")
  message(file_sel)
  df <- read_pd_internal(file_sel, ...)
  df$lat <- param_df$Latitude
  df$lon <- param_df$Longitude
  start_x <- param_df$start_x
  end_x <- param_df$end_x
  # toFix for more than one year
  year <- (as.numeric(unlist(stringr::str_split(param_df$Data_year, ","))))
  df$time <- prepare_time(df$time, year = unique(year),
                          type = param_df$Unit_x_axis,
                          start_x = param_df$start_x,
                          end_x = param_df$end_x)
  attr(df, "xlab") <- param_df$Unit_x_axis
  attr(df, "ylab") <- param_df$Unit_y_axis
  attr(df, "location") <- param_df$Location
  return(df)
}

# read Plot Digitizer -----------------------------------------------------
read_pd_internal <- function(file, ...){
  x <- utils::read.csv2(file, header = TRUE, row.names = NULL, skip = 5, ...)
  # names(x) <- names(x)[c(2, 3, 1)]
  x <- as.data.frame(apply(x, 2, as.numeric))
  x[[2]] <- ifelse(x[[2]] < 0, 0, x[[2]])
  names(x) <- c("time", "value")
  return(x)
}

# x_path <- "Digitising FF trapping plots/B. dorsalis/11_1.csv"
# x <- read_pd_internal(x_path)
# x

# prepare_date ------------------------------------------------------------
prep_date <- function(time, year, format){
  is_leap_year <- lubridate::leap_year(year)
  if (format == "month"){
    st_time <- time / 12
    if (is_leap_year){
      day_of_year <- st_time * 366
    } else {
      day_of_year <- st_time * 365
    }
  } else if (format == "day"){
    day_of_year <- time
  } else if (format == "date"){
    stop("Not yet implemented", call. = FALSE)
  }

  origin <- as.Date(paste0(year, "-01-01"), tz = "UTC") - lubridate::days(1)
  result <- as.Date(day_of_year, origin = origin, tz = "UTC")
  return(result)
}

rescale_month <- function(x, start_x, end_x){
  if (start_x > end_x){
    c(start_x:12, 1:end_x)
  } else {
    start_x:end_x
  }
}

prepare_time <- function(time, year, type, start_x, end_x){
  origin <- as.Date(paste0(year, "-01-01"), tz = "UTC") - lubridate::days(1)
  if (type == "date"){
    # filename <- "Segura_2004_Medfly_Argentina_1_1"; species <- "Cc"
    result <- seq(as.Date(start_x),
                  as.Date(end_x), length.out = length(time))
  } else if (type == "day") {
    # filename <- "Bjelis_2007_1"; species <- "Cc"
    # filename <- "2002_Medfly_1"; species <- "Cc"
    day_of_year <- time # option 1
    # day_of_year <- scales::rescale(time,
    #                                to = c(as.Date(start_x),
    #                                       as.Date(end_x))) # option 2
    result <- as.Date(day_of_year, origin = origin, tz = "UTC")
    # result <- seq(as.Date(start_x),
    #               as.Date(end_x), length.out = length(time))
  } else if (type == "week"){
    # filename <- "26_1a_1"; species <- "Bz"
    result <- seq(as.Date(start_x), as.Date(end_x), by = "week")
  } else if (type == "month"){
    # filename <- "Segura_2004_Medfly_Argentina_1_1"; species <- "Cc"
    # filename <- "Escudero-Colomar_2008_1"; species <- "Cc"
    # filename <- "6_1powiekszone_1"; species <- "Bz"
    # filename <- "6_2powiekszenie(2)_a_1"; species <- "Bz"
    result <- seq(as.Date(start_x), as.Date(end_x), by = "month")
  } else if (type == "year"){
    # filename <- "20_1b_1"; species <- "Bd"
    result <- seq(as.numeric(start_x), as.numeric(end_x), by = 1)
    result <- as.Date(paste0(result, "-01-01"))
  } else if (type == "fortnight"){
    # filename <- "35_1powiekszone_1"; species <- "Bz"
    result <- seq(as.Date(start_x), as.Date(end_x), by = "2 week")
  }
  return(result)
}

# improved read -----------------------------------------------------------


# trap_plot(read_pd(4))
# filename <- "Segura_2004_Medfly_Argentina_1_1"
# species <- "Cc"
# x <- read_pd(filename, species)
