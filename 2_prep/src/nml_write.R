
#' in the future:
#'  - use sync_dir as an arg
#'  - prefix the yaml names with their model type (e.g., pb0, pball, other)
#'
#' @param base_nml an nml file to use as a starting point
write_nml_files <- function(fileout, nml_list, base_nml){

  nml_base <- read_nml(base_nml)

  site_ids <- names(nml_list)

  sync_dir <- '2_prep/sync'
  files_out <- file.path(sync_dir , paste0(site_ids, '.nml'))

  for (i in 1:length(site_ids)){

    site_id <- site_ids[i]
    nml_args <- nml_list[[site_id]]
    nml_args$lake_name <- nml_args$site_id
    nml_args$sim_name <- nml_args$site_id
    nml_args$site_id <- NULL

    nml_args$sw_factor = 1
    nml_args$start = '1979-04-01'
    nml_args$stop = '2019-12-31'
    nml_args$dt=3600

    nml_args$bsn_vals = length(nml_args$H)
    nml_args$the_depths = c(0, floor(nml_args$lake_depth*100)/100)
    nml_args$num_depths = 2
    nml_args$timefmt = 2
    nml_args$the_temps = c(3,4)
    nml <- set_nml(nml_base, arg_list = nml_args)
    write_nml(glm_nml = nml, file = files_out[i])
    # write nml here
  }
  sc_indicate(fileout, data_file = files_out)
  dest_dir <- file.path('/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models', sync_dir )
  yeti_put(dest_dir = dest_dir, local_dir = sync_dir, files = basename(files_out))
}

#' @param nml_obj a list of nml objects (lists of class `nml`) that are full GLM configs
#' @param pb0_nml_list used only for the true name/location of the meteo_fl
write_result_nml_files <- function(fileout, nml_objs, pb0_nml_list, sync_dir, prefix){
  site_ids <- names(nml_objs)

  files_out <- file.path(sync_dir , paste0(prefix, '_', site_ids, '.nml'))
  for (i in 1:length(site_ids)){

    this_nml <- nml_objs[[i]]
    this_nml <- set_nml(this_nml, arg_name = 'meteo_fl', arg_val = pb0_nml_list[[site_ids[i]]]$meteo_fl)
    write_nml(glm_nml = this_nml, file = files_out[i])
  }

  sc_indicate(fileout, data_file = files_out)
  dest_dir <- file.path('/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models', sync_dir )
  yeti_put(dest_dir = dest_dir, local_dir = sync_dir, files = basename(files_out))
}
