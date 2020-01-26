

read_feather_ids <- function(feather_file){
  feather::read_feather(feather_file)$nhdhr_id
}

filter_finished_cals <- function(result_dir, pattern, dummy){
  # read and drop the `results` block of the nml

  result_files <- data.frame(file = dir(result_dir), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(file, pattern)) %>%
    mutate(file = file.path(result_dir, file),
           sim_id = stringr::str_remove(file, 'pball_'), sim_id = basename(stringr::str_remove(sim_id, '_results.nml')))

  # unclear why this isn't setting correctly in `calibration_utils.R`.
  # but looks like we need to override the nml values for the params since they are still the defaults
  cal_params <- c("cd", "sw_factor", "coef_mix_hyp")
  message('warning: hard-coding cal_params to', cal_params)
  lapply(result_files$file, FUN = function(x){
    nml <- glmtools::read_nml(x)
    nml <- set_nml(nml, arg_list = setNames(as.list(nml$results$cal_values), cal_params))
    nml$results <- NULL
    return(nml)
  }) %>% setNames(result_files$sim_id)
}
