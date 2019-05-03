#' return rds of files on Yeti
#'
yeti_files <- function(ind_file, yeti_dir){
  files = yeti_list_files(yeti_dir = yeti_dir)

  if(sum(grepl('.ind', files$files)) >= 1){
    non_ind_files = dplyr::tibble(files = files$files[-grep('.ind', files$files)])
  }else{
    non_ind_files = dplyr::tibble(files = files$files)
  }

  data_file <- as_data_file(ind_file)
  saveRDS(non_ind_files, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file)
}
