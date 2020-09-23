#' Title
#'
#' @param rasterstack
#' @param filename
#' @param bbox
#' @param outdir
#' @param framerate
#' @param width
#' @param height
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
# source("R/01-extract_ncdf.R")
#
# Path <- "C:/Google Drive/Corvus Geostat - Jakub Nowosad/"
# Path <- "./"
# input_file <- paste0(Path,"NetCDF/DxResults_Cc_2010-2015.nc")
# AllYears <- extract_data_list(input_file, "Weekly Growth Index",
#                               years = seq(2000, 2014, 1))
# rasters <- create_raster_stack(AllYears,
#                                start_year = 2000, end_year = 2014)
#
# rm(AllYears); gc()
#
# # xmin, ymin, xmax, ymax
# world_bbox <- c(-180, -90, 180, 90)
# africa_bbox <- c(-17.8, -34.9, 51.2, 37.4)
# europe_bbox <- c(-32, 34, 70, 56)
# #
#
# rasters2 = raster::subset(rasters, 1:6)
# # zabiera to ok. 8 minut dla 780 klatek
# raster_animation(rasterstack = rasters2,
#                  filename = "europe6.mp4",
#                  bbox = europe_bbox)
#
# raster_animation(rasterstack = rasters,
#                  filename = "africa.mp4",
#                  bbox = africa_bbox)
#
# raster_animation(rasterstack = rasters,
#                  filename = "world.mp4",
#                  bbox = world_bbox)
raster_animation <- function(rasterstack, filename,
                             bbox, outdir,
                             framerate = 4,
                             width = 7, height = 7, dpi = 300){

  if (missing(outdir)){
    d <- paste(tempdir(), "/tmap_plots", sep = "/")
    if (dir.exists(d)) unlink(d, recursive = TRUE)
  } else {
    d <- outdir
  }

  dir.create(d, showWarnings = FALSE)

  n <- raster::nlayers(rasterstack)
  l <- 1
  step <- ifelse(n < 20, n, 20)

  for (i in seq(1, n, by = step)){

    r1 <- raster::subset(rasterstack, i:(i + (step - 1)))

    r1_names <- names(r1)
    year <- as.numeric(substr(r1_names, 2, 5))
    week <- as.numeric(substr(r1_names, 6, 7))
    panel_title <- paste0("Year: " , year, " Week: ", week)

    tm <- tmap::tm_shape(r1, bbox = bbox) +
      tmap::tm_graticules() +
      tmap::tm_raster(style = "cont", palette = "viridis", title = "Value:") +
      tmap::tm_facets(ncol = 1, nrow = 1) +
      tmap::tm_layout(legend.position = c("left", "bottom"),
                      panel.labels = panel_title)
    suppressMessages(tmap::tmap_save(
      tm,
      filename = paste(d,
                       paste0("plot", sprintf("%03d", l), "%03d.png"),
                       sep = "/"),
      width = width,
      height = height,
      dpi = dpi
    ))
    l <- l + 1
  }
  # files <- list.files(path = d, pattern = "^plot[0-9]{6}\\.png$")
  files <- list.files(path = d,
                      pattern = "^plot[0-9]{6}\\.png$",
                      full.names = TRUE)
  k <- length(files)

  cat("All frames were generated. Now it is a time for video rendering!\n")
  av::av_encode_video(input = files, output = filename, framerate = framerate)
  cat(filename, "was created!\n")
}


