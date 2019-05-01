# functions for gathering lake attributes

lake_list <- function(crosswalk_file){
  lake_list <- readRDS(crosswalk_file) %>%
    dplyr::filter(!duplicated(site_id))

  return(lake_list)
}

get_base_lake_nml <- function(nml_file, nhd_id, nldas_x, nldas_y, drivers_yeti_path) {
  # change Driver file path # ****************************************
  nml <- mda.lakes::populate_base_lake_nml(site_id = nhd_id,
                                driver = file.path(drivers_yeti_path, # file path on Yeti
                                                   paste0('NLDAS_time[0.346848]_x[',nldas_x,']_y[',nldas_y,'].csv'))) #times are all the same, but could make this an input

  # write and post the output
  # data_file <- as_data_file(ind_file)
  write_nml(glm_nml = nml, file = nml_file)
  # gd_put(remote_ind = ind_file, local_source = data_file)
}


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

