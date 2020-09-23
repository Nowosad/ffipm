#' Raster Animation
#'
#' @param rasters Any RasterStack
#' @param filename The output filename with a video extension, such as mp4, mkv, mov, or flv. For more information visit [av::av_encode_video()]
#' @param bbox A vector with four values corresponding to xmin, ymin, xmax, ymax
#' @param outdir Output directory for storing video frames (graphics). The default is a temporary folder
#' @param framerate A number of frames per seconds. For more information visit [av::av_encode_video()]
#' @param width The width the plot. The default is 7 (inches). For more information visit [tmap::tmap_save()]
#' @param height The height of the plot. The default value is calculated automatically based on the RasterStack dimensions
#' @param dpi The default is 100. For more information visit [tmap::tmap_save()]
#'
#' @return A video file
#' @export
#'
#' @examples
#'  \dontrun{
#'   Path <- "C:/Google Drive/Corvus Geostat - Jakub Nowosad/"
#'   Path <- "../corvus_dynamic_outputs/"
#'   input_file <- paste0(Path,"NetCDF/DxResults_Cc_2010-2015.nc")
#'   AllYears <- extract_data_list(input_file, "Weekly Growth Index",
#'                               years = 2000:2014)
#'   rasters <- create_raster_stack(AllYears, years = 2000:2014)
#'
#'   # xmin, ymin, xmax, ymax
#'   world_bbox <- c(-180, -90, 180, 90)
#'   africa_bbox <- c(-17.8, -34.9, 51.2, 37.4)
#'   europe_bbox <- c(-32, 34, 70, 56)
#'   #
#'
#'   # it takes about 8 minut for 780 frames
#'   # the below examples use only 60 frames to show an example
#'   rasters2 = raster::subset(rasters, 1:60)
#'   raster_animation(rasters = rasters2,
#'                    filename = "europe6.mp4",
#'                    bbox = europe_bbox)
#'
#'   raster_animation(rasters = rasters2,
#'                    filename = "africa.mp4",
#'                    bbox = africa_bbox)
#'
#'   raster_animation(rasters = rasters2,
#'                    filename = "world.mp4",
#'                    bbox = world_bbox)
#' }
raster_animation <- function(rasters, filename,
                             bbox, outdir,
                             framerate = 4,
                             width = 7, height, dpi = 100){

  if (missing(height)){
    ra <- raster::crop(rasters[[1]], raster::extent(bbox[c(1, 3, 2, 4)]))
    dim_props <- nrow(ra) / ncol(ra)
    height <- width * (dim_props * 1.05)
  }

  if (missing(outdir)){
    d <- paste(tempdir(), "/tmap_plots", sep = "/")
    if (dir.exists(d)) unlink(d, recursive = TRUE)
  } else {
    d <- outdir
  }

  dir.create(d, showWarnings = FALSE)

  dname <- attr(rasters, "dname")

  n <- raster::nlayers(rasters)
  l <- 1
  step <- ifelse(n < 20, n, 20)

  for (i in seq(1, n, by = step)){

    r1 <- raster::subset(rasters, i:(i + (step - 1)))

    r1_names <- names(r1)
    year <- as.numeric(substr(r1_names, 2, 5))
    week <- as.numeric(substr(r1_names, 6, 7))
    panel_title <- paste0("Year: " , year, " Week: ", week)

    tm <- tmap::tm_shape(r1, bbox = bbox) +
      tmap::tm_graticules() +
      tmap::tm_raster(style = "cont", palette = "viridis",
                      title = paste0(dname, " :")) +
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
  av::av_encode_video(input = files, output = filename, framerate = framerate,
                      vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")
  cat(filename, "was created!\n")
}


