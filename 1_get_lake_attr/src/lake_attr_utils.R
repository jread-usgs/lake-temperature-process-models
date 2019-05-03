# functions for gathering lake attributes

lake_list <- function(crosswalk_file){
  lake_list <- readRDS(crosswalk_file) %>%
    dplyr::filter(!duplicated(site_id))

  return(lake_list)
}

#' genreate a base nml file to drive GLM run
#' @param nml_file file path where nml file should be written
#' @param nhd_id lake nhd_id
#' @param nldas_x x coordinate for nldas grid
#' @param nldas_y y coordinate for nldas grid
#' @param drivers_yeti_path file path on USGS Yeti where driver files are held
#' @param drivers_time
get_base_lake_nml <- function(nml_file, nhd_id, nldas_x, nldas_y, drivers_yeti_path, drivers_time) {

  nml <- mda.lakes::populate_base_lake_nml(site_id = nhd_id,
                                           driver = file.path(drivers_yeti_path, # file path on Yeti
                                                              paste0('NLDAS_time[',drivers_time,']_x[',nldas_x,']_y[',nldas_y,'].csv')))

  # write the output
  write_nml(glm_nml = nml, file = nml_file)
}

#' #' Get the NLDAS driver time from Yeti
#' #' @param nldas_x x coordinate for nldas grid
#' #' @param nldas_y y coordinate for nldas grid
#' #' @param drivers_yeti_path file path on USGS Yeti where driver files are held
#' get_nldas_time <- function(nldas_x, nldas_y, drivers_yeti_path){
#'
#'   drivers_file = yeti_list_files(yeti_dir = drivers_yeti_path)
#'
#'   if(sum(grepl('.ind', drivers_file$files)) >= 1){
#'     unique_drivers_file = drivers_file$files[-grep('.ind', drivers_file$files)]
#'   }
#'   x_y = paste0('x[',nldas_x,']_y[',nldas_y,'].csv')
#'   unique_drivers_file[grep(eval(x_y), unique_drivers_file, fixed = T)]
#' }


#' Copied from pipeline #3
#' Throws error "failed: No such file or directory...rsync failed with code 23:
#' Partial transfer due to error" if any of the requested files are unavailable
retrieve_lake_data <- function(out_ind, priority_lakes, file_column, yeti_path) {
  # download the priority lake files from yeti, using the file info from
  # priority_lakes
  requested_files <- unique(priority_lakes[[file_column]])
  dest_files <- yeti_get(
    src_dir = yeti_path,
    dest_dir = unique(dirname(requested_files)),
    files = basename(requested_files))

  # write a single yaml of all the file names and md5 hashes
  sc_indicate(ind_file = out_ind, data_file = dest_files)
}

