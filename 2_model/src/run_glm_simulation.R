
#' Wrapper function for running uncalibrated glm simulation
#'
#' @param config_path nml file path for glm simulation
#' @param drivers_path driver data file path for glm simulation
#' @param sim_path glm simulation file path
#' @param preds_path file path to glm predictions
run_glm_simulation <- function(config_path, drivers_path, sim_path, preds_path){

  nml_sim_path <- file.path(sim_path, 'glm2.nml')
  file.copy(from = config_path, to = nml_sim_path, overwrite = TRUE)
  nml <- read_nml(nml_sim_path)
  write_nml(glm_nml = nml, file = nml_sim_path)

  run_glm(sim_path)
}
