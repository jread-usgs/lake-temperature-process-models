#' Filter the nml args list to only those sites that have a meteo file available
filter_nml_list <- function(nml_list_unfiltered_rds) {
  nml_list_unfiltered <- readRDS(nml_list_unfiltered_rds)
  meteo_fl <- sapply(nml_list_unfiltered, `[[`, 'meteo_fl')
  nml_list <- nml_list_unfiltered[file.exists(file.path('../lake-temperature-model-prep/7_drivers_munge/out', meteo_fl))]
  return(nml_list)
}
