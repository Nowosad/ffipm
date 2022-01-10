#' Create Composite raster
#'
#'file1, file2, mask, years_input_file, years_raster_stack, dname
#' @param file1 file An ncdf file for mask 0 values
#' @param file2 file An ncdf file for mask 1 values
#' @param mask Irrigation mask raster with values 0,1
#' @param years_input_file A vector indicating years in the input file
#' @param years_raster_stack A vector stating for which years the RasterStack should be created
#' @param dname A variable name. One of "Weekly Growth Index", "Hot Stress", "Dry Stress", "Wet Stress", "Cold Stress", "Ecoclimatic Index", "Moisture Index", "Temperature Index"
#'
#' @return A Composite RasterStack with some metadata
#' @export
#'
#' @examples
#' \dontrun{
#' composite_raster_dorsalis <- fun1(file1 = "WeeklyGI_B,dorsalis_2018-2019_World.nc",
#'                                   file2 = "WeeklyGI_B,dorsalis_2018-2019_World_irrigation_set.nc",
#'                                   mask = "CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif",
#'                                   years = 2018:2019,
#'                                   dname = "Weekly Growth Index")
#'
#' }
create_composite_raster <- function(file1, file2, mask, years_input_file, years_raster_stack, dname) {
  mask <- raster::raster(mask) # read mask

  # extracts selected variable from the model (.nc) into a list of matrices
  AllYears1 <- extract_data_list(file1, dname,
                                 years = years_input_file)
  # creates a RasterStack based on the result from extract_data_list()
  rasters1 <- create_raster_stack(AllYears1, years = years_raster_stack)

  #remove AllYears1 to not run out of memory
  rm(AllYears1)
  gc() # free unused memory

  raster_extent <- raster::extent(rasters1)
  mask_cropped <- raster::crop(mask, raster_extent)

  rm(mask)
  gc() # free unused memory

  # Create a new raster object that has the same values as x, except for the cells that are equal to maskvalue
  new_file1 <- raster::mask(rasters1, mask = mask_cropped,
                            maskvalue=1, updatevalue=NA)

  rm(rasters1)
  gc() # free unused memory
  # extracts selected variable from the model (.nc) into a list of matrices
  AllYears2 <- extract_data_list(file2, dname,
                                 years = years_input_file)
  # creates a RasterStack based on the result from extract_data_list()
  rasters2 <- create_raster_stack(AllYears2, years = years_raster_stack)


  rm(AllYears2)
  gc() # free unused memory

  new_file2 <- raster::mask(rasters2, mask = mask_cropped,
                            maskvalue=0, updatevalue=NA)
  # final_raster <- raster::cover(new_file1, new_file2)
  # final_raster_stack <- raster::stack(final_raster)

  rm(rasters2)
  rm(mask_cropped)
  gc() # free unused memory

  #first method
  final_raster <- raster::cover(new_file1, new_file2)
  names(final_raster) <- names(new_file2)

  # second method
  # insert values from file2 to file1 where are nas
  # raster::values(new_file1) <- ifelse(is.na(raster::values(new_file1)), raster::values(new_file2), raster::values(new_file1))
  # composite_raster <- raster::stack(new_file1)

  # both metod give the same values but second method is faster 20 sec vs 5 sec
  # but first method not working with large files, return: Error: cannot allocate vector of size 3.8 Gb
  #table(values(final_raster_stack) == values(composite_raster))
  #
  rm(new_file2)
  gc() # free unused memory

  composite_raster <- raster::stack(final_raster)
  attr(composite_raster, "dname") <- dname

  rm(new_file1)
  gc() # free unused memory

  return(composite_raster)
}
