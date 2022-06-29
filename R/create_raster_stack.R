#' Create Raster Stack
#'
#' @param allyears An output from [extract_data_list()]
#' @param years A vector stating for which years the RasterStack should be created
#' @param start_week A number of first week in the output RasterStack.
#'   The default is 1
#' @param end_week A number of last week in the output RasterStack.
#'   The default is 52
#' @param step "Step" for weekly data or "Year" for yearly data
#'
#' @return A RasterStack with some metadata
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
#' }
create_raster_stack <- function(allyears,
                                years,
                                start_week = 1, end_week = 52,
                                step = "Step"){
  metadata <- attr(allyears, "metadata")

  # select years
  years_range <- years
  allyears <- allyears[which(names(allyears) %in% as.character(years_range))]

  if (length(allyears) < 1) stop("Selected years do not exist", call. = FALSE)

  if(step == "Step"){
    layer_names <- paste0("Y",
                          rep(sprintf("%04d", as.numeric(names(allyears))),
                              each = length(start_week:end_week)),
                          rep.int(sprintf("%02d", as.numeric(start_week:end_week)),
                                  times = length(allyears)))

    # select weeks
    allyears <- lapply(allyears, function(x) x[start_week:end_week])

    # create one-level list
    allyears <- unlist(allyears, recursive = FALSE)
  }

  # create raster list
  allyears <- lapply(allyears, create_raster_single, metadata = metadata)
  allyears <- raster::stack(allyears)

  if(step == "Step"){
    names(allyears) <- layer_names
  } else if(step == "Year"){
    names(allyears) <- years
  }

  attr(allyears, "dname") <- metadata$dname
  return(allyears)
}

create_data_list <- function(input_stack, n = 52, years){
  len_is <- length(input_stack)
  num_of_elements <- len_is / n
  if (missing(years)){
    years <- seq_len(num_of_elements)
  }
  result <- vector(mode = "list", length = num_of_elements)
  start_n <- 1
  for (i in seq_len(num_of_elements)){
    result[[i]] <- input_stack[start_n:(n * i)]
    start_n <- start_n + n
  }
  names(result) <- years
  return(result)
}
create_template_raster <- function(input, metadata){
  if (missing(metadata)){
    metadata <- attr(input, "metadata")
  }

  lat2 <- seq(min(metadata$lat), max(metadata$lat), 0.5)
  lon2 <- seq(min(metadata$lon), max(metadata$lon), 0.5)

  # Create extent for the raster image
  e <- raster::extent(min(lon2) - 0.25, max(lon2) + 0.25,
                      min(lat2) - 0.25, max(lat2) + 0.25)

  r <- raster::raster(nrow = length(lat2), ncol = length(lon2),
                      ext = e, crs = "+proj=longlat +datum=WGS84 +no_defs")

  return(r)
}

# allyears, year = 1, week = 1
create_raster_single <- function(input, metadata){
  if (missing(metadata)){
    metadata <- attr(input, "metadata")
  }

  template_raster <- create_template_raster(input, metadata)

  # vv <- matdata
  # vv <- allyears[[year]][[week]]

  m <- matrix(NA,
              nrow = nrow(template_raster),
              ncol = ncol(template_raster))
  # get column numbers, from raster based on y coordinate(s)
  b <- raster::colFromX(template_raster, as.vector(metadata$lon))
  m[, b] <- input # insert values to matrix, based on indexes b
  # column numbers with missing coordinates omitted: 1   2   3   6   7   8   9  10  11  12  13 ...
  # And assign m to the RasterLayer
  raster::values(template_raster) <- m
  return(template_raster)
}
