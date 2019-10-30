
#' Export glm-simulated temperature at fixed depths
#'
#' @param outfile file name to which the simulated temperature will be written
#' @param simout glm simulation output
#' @param export_depths vector of depths for which to export simulated water temperature
export_temp <- function(outfile, simout, export_depths){

  temp_data <- glmtools::get_temp(simout, reference = 'surface', z_out = export_depths)

  feather::write_feather(temp_data, outfile)
}


#' Depths to export for given glm simulation
#'
#' @param site_id nhd or mglp ID for lake that is being simulated
#' @param depth_interval depth in meters for creating a sequence from the lake surface to max depth
export_depths <- function(site_id, depth_interval){
  max_z = mda.lakes::getZmax(site_id)

  depths = seq(0, max_z, by = depth_interval)
  return(depths)
}
