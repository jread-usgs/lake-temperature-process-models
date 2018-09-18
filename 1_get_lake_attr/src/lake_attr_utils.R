# functions for gathering lake attributes

lake_list <- function(crosswalk_file){
  lake_list <- readRDS(crosswalk_file) %>%
    dplyr::filter(!duplicated(site_id))

  return(lake_list)
}

get_base_lake_nml <- function(nhd_id, nldas_x, nldas_y, nml_out) {
  # change Driver file path # ****************************************
  nml <- mda.lakes::populate_base_lake_nml(site_id = nhd_id,
                                driver = file.path('driver-data', # file path on Yeti - this may change depending on where the nml files live (I think)
                                                   paste0('NLDAS_time[0.346848]_x[',nldas_x,']_y[',nldas_y,'].csv'))) #times are all the same, but could make this an input
  write_nml(glm_nml = nml, file = nml_out)
}
