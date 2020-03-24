

summarize_transfer_glm <- function(fileout, dirname, pattern, n_runs, dummy){
  tran_results <- data.frame(file = dir(dirname), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(file, pattern)) %>% pull(file) %>%
    purrr::map(function(x){
      sim_id <- stringr::str_remove(string = x, 'transfer_') %>% stringr::str_remove('_rmse.csv')
      readr::read_csv(file.path(dirname, x)) %>%
        mutate(target_id = sim_id, cr_time = {file.info(file.path(dirname, x))$ctime}) %>%
        select(target_id, source_id, rmse, cr_time)
    }) %>% purrr::reduce(rbind)

  use_targets <- group_by(tran_results, target_id) %>% tally() %>%
    filter(n == !!n_runs) %>% pull(target_id)

  tran_results %>% filter(target_id %in% use_targets) %>% select(-cr_time) %>%
    readr::write_csv(path = fileout)
}

