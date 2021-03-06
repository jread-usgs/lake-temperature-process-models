extract_expansion_ids <- function(filepath, target_ids, source_ids){
  readRDS(filepath) %>% filter(!site_id %in% c(target_ids, source_ids)) %>%
    pull(site_id) %>% unique() %>% sort()
}

filter_unrun <- function(nml_list = nml_list, ...){
  already_ran <- c(...)
  all_ids <- names(nml_list)
  return(all_ids[!all_ids %in% already_ran])
}

read_feather_ids <- function(feather_file){
  feather::read_feather(feather_file)$nhdhr_id
}

filter_finished_cals <- function(result_dir, pattern, cal_lakes, dummy){
  # read and drop the `results` block of the nml

  result_files <- data.frame(file = dir(result_dir), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(file, pattern)) %>%
    mutate(file = file.path(result_dir, file),
           sim_id = stringr::str_remove(file, 'pball_'), sim_id = basename(stringr::str_remove(sim_id, '_results.nml')))

  # unclear why this isn't setting correctly in `calibration_utils.R`.
  # but looks like we need to override the nml values for the params since they are still the defaults
  cal_params <- c("cd", "sw_factor", "coef_mix_hyp")

  #success_runs <- sapply(result_files$file, function(x){glmtools::read_nml(x)}

  message('warning: hard-coding cal_params to ', paste(cal_params, collapse = ', '))
  yeti_results <- lapply(result_files$file, FUN = function(x){
    nml <- glmtools::read_nml(x)
    nml <- set_nml(nml, arg_list = setNames(as.list(nml$results$cal_values), cal_params))
    if (nml$results$rmse == 99){
      return(NULL)
    } else {
      nml$results <- NULL
      return(nml)
    }
  }) %>% setNames(result_files$sim_id)

  # couldn't think of a more elegant way to do this...

  failed_runs <- sapply(yeti_results, is.null)
  failed_ids <- names(failed_runs)[failed_runs]

  return(yeti_results[cal_lakes[!cal_lakes %in% failed_ids]])

}
