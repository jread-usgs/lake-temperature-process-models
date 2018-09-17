# functions for gathering lake attributes

lake_list <- function(crosswalk_file){
  lake_list <- readRDS(crosswalk_file) %>%
    dplyr::filter(!duplicated(site_id))

  return(lake_list)
}



get_base_lake_nml <- function(nhd_id, nml_out) {
  # change Driver file path # ****************************************
  nml <- mda.lakes::populate_base_lake_nml(site_id = nhd_id,
                                driver = file.path("2_setup_models/meteo",
                                                   paste0(nhd_id, "_driver.csv")))
  write_nml(glm_nml = nml, file = nml_out)
}
