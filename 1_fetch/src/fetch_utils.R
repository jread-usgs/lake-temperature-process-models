#' Filter the nml args list to only those sites that have a meteo file available
filter_nml_list <- function(nml_list_unfiltered_rds) {
  nml_list_unfiltered <- readRDS(nml_list_unfiltered_rds)
  meteo_fl <- sapply(nml_list_unfiltered, `[[`, 'meteo_fl')
  nml_list <- nml_list_unfiltered[file.exists(file.path('../lake-temperature-model-prep/7_drivers_munge/out', meteo_fl))]
  return(nml_list)
}


filter_toha_models <- function(nml_list, kw_data){

  nml_with_kw_file <- nml_list[names(nml_list) %in% unique(kw_data$site_id)]

  model_hypso_ids <- sapply(names(nml_with_kw_file), function(x){
    length(nml_with_kw_file[[x]]$H) > 2
  })
  names(model_hypso_ids)[model_hypso_ids]
}
