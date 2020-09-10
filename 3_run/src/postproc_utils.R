

summarize_transfer_glm <- function(fileout, dirname, pattern, n_runs, dummy){
  tran_results <- data.frame(file = dir(dirname), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(file, pattern)) %>% pull(file) %>%
    purrr::map(function(x){
      sim_id <- stringr::str_remove(string = x, 'transfer_') %>% stringr::str_remove('_rmse.csv')
      readr::read_csv(file.path(dirname, x), col_types = cols(
        source_id = col_character(),
        rmse = col_double(),
        sim_time = col_datetime(format = "")
      )) %>%
        mutate(target_id = sim_id, cr_time = {file.info(file.path(dirname, x))$ctime}) %>%
        select(target_id, source_id, rmse, cr_time)
    }) %>% purrr::reduce(rbind)

  use_targets <- group_by(tran_results, target_id) %>% tally() %>%
    filter(n == !!n_runs) %>% pull(target_id)

  filtered_results <- filter(tran_results, target_id %in% use_targets) %>% select(-cr_time)

  if (any(is.na(filtered_results$rmse)) | any(filtered_results$rmse == -999)){
    message_bad <- filtered_results %>% filter(is.na(rmse) | rmse == -999) %>%
      mutate(bad = paste0('target ', target_id, '+ source ', source_id, '=', rmse)) %>% pull(bad)
    stop('there are issues in the matrix result\n', message_bad)
  }

  readr::write_csv(filtered_results, path = fileout)
}


summarize_transfer_test <- function(fileout, tasks_ind, transfer_metamodel_file){
  transfer_data <- read_csv(transfer_metamodel_file) %>%
    mutate(target_id = paste0('nhdhr_', `target_id(nhdhr)`),
           source_id = paste0('nhdhr_', `best_predicted_site_id (nhdhr)`)) %>%
    select(target_id, source_id, predicted_rmse)

  result_files <- yaml.load_file(tasks_ind) %>% names
  purrr::map(result_files, function(x){
    feather::read_feather(x)
  }) %>% purrr::reduce(bind_rows) %>% inner_join(transfer_data) %>%
    rename(actual_rmse = rmse) %>%
    write_csv(path = fileout)

}
