

read_feather_ids <- function(feather_file){
  feather::read_feather(feather_file)$nhdhr_id
}

filter_finished_cals <- function(result_dir, pattern, dummy){
  # read and drop the `results` block of the nml

  result_files <- data.frame(file = dir(result_dir), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(file, pattern)) %>%
    mutate(file = file.path(result_dir, file),
           sim_id = stringr::str_remove(file, 'pball_'), sim_id = basename(stringr::str_remove(sim_id, '_results.nml')))

  lapply(result_files$file, FUN = function(x){
    nml <- glmtools::read_nml(x)
    nml$results <- NULL
    return(nml)
  }) %>% setNames(result_files$sim_id)
}
