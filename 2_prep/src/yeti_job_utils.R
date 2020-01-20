

yeti_put <- function(local_dir, dest_dir, files){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@yeti.cr.usgs.gov', user))
  on.exit(ssh::ssh_disconnect(session = session))

  file_paths = sprintf('%s/%s', local_dir, files)

  ssh::scp_upload(session = session, files = file_paths, to = dest_dir)

}

sync_drivers <- function(fileout, nml_list){

  nml_meteo_files <- sapply(nml_list, FUN = function(x) x$meteo_fl) %>% as.vector()

  sync_dir <- '2_prep/sync'
  file.copy(from = file.path('../lake-temperature-model-prep/7_drivers_munge/out', nml_meteo_files), to = file.path(sync_dir, nml_meteo_files), overwrite = TRUE)


  sc_indicate(fileout, data_file = file.path(sync_dir, nml_meteo_files))


  dest_dir <- file.path('/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models', sync_dir )
  yeti_put(dest_dir = dest_dir, local_dir = sync_dir, files = nml_meteo_files)
}

build_pball_job_df <- function(fileout, nml_list, sim_ids){

  # all_jobs <- data.frame(
  #     sim_id = c('pball_nhdhr_166868607','pball_nhdhr_166868799'),
  #     nml_file = c('2_prep/sync/nhdhr_166868607.nml', '2_prep/sync/nhdhr_166868799.nml'),
  #     meteo_file = c('2_prep/sync/NLDAS_time[0.351500]_x[225]_y[159].csv', '2_prep/sync/NLDAS_time[0.351500]_x[226]_y[158].csv'),
  #     result_file = c('3_run/sync/pball_nhdhr_1668686073_results.nml', '3_run/sync/pball_nhdhr_166868799_results.nml'),
  #     stringsAsFactors = FALSE)

  all_jobs <- build_job_df(sim_ids = sim_ids, nml_list = nml_list) %>%
    mutate(result_file = sprintf('3_run/sync/%s_results.nml', sim_id)) %>%
    saveRDS(fileout)

}

build_transfer_job_list <- function(fileout, cal_nml_obj, base_nml_list, sim_ids){
  # all_jobs <- list(
  #   list(
  #     sim_id = 'nhdhr_123423',
  #     nml_file = '2_prep/sync/nhdhr_123423.nml',
  #     meteo_file = '2_prep/sync/NLDAS_time[0.351500]_x[225]_y[159].csv',
  #     source_id = c('nhdhr_166868607','nhdhr_166868799'),
  #     source_cd = c(0.00123, 0.0122),
  #     source_Kw = c(0.23, 0.52),
  #     source_coef_wind_stir = c(0.22, 0.25),
  #     export_file = '3_run/sync/transfer_nhdhr_123423_rmse.csv'
  #   ))

  message('warning, this linked to the params used in `run_glm_cal` and they are hard-code here')
  all_jobs <- list()
  for (i in 1:length(sim_ids)){
    sim_id <- sim_ids[i]
    source_ids <- sim_ids[!sim_ids %in% sim_id]
    this_job <- list(
      sim_id = sim_id,
      base_nml_file = sprintf('2_prep/sync/%s.nml', sim_id),
      meteo_file = sprintf('2_prep/sync/%s', base_nml_list[[sim_id]]$meteo_fl),
      source_id = source_ids,
      source_cd = sapply(source_ids, function(x){
        glmtools::get_nml_value(cal_nml_obj[[x]], arg_name = 'cd')
      }, USE.NAMES = FALSE),
      source_Kw = sapply(source_ids, function(x){
        glmtools::get_nml_value(cal_nml_obj[[x]], arg_name = 'Kw')
      }, USE.NAMES = FALSE),
      source_coef_wind_stir = sapply(source_ids, function(x){
        glmtools::get_nml_value(cal_nml_obj[[x]], arg_name = 'coef_wind_stir')
      }, USE.NAMES = FALSE),
      export_file = sprintf('3_run/sync/transfer_%s_rmse.csv', sim_id)
    )
    all_jobs[[i]] <- this_job
  }
  saveRDS(all_jobs, fileout)
}

build_pb0_job_list <- function(fileout, nml_list, job_chunk = 40, temperature_file){

  # all_jobs <- list(
  #   data.frame(
  #     sim_id = c('pb0_nhdhr_166868607','pb0_nhdhr_166868799'),
  #     nml_file = c('2_prep/sync/nhdhr_166868607.nml', '2_prep/sync/nhdhr_166868799.nml'),
  #     meteo_file = c('2_prep/sync/NLDAS_time[0.351500]_x[225]_y[159].csv', '2_prep/sync/NLDAS_time[0.351500]_x[226]_y[158].csv'),
  #     export_file = c('3_run/sync/pb0_nhdhr_1668686073_temperatures.feather', '3_run/sync/pb0_nhdhr_166868799_temperatures.feather'),
  #     stringsAsFactors = FALSE),
  #   data.frame(
  #     sim_id = c('pb0_nhdhr_123423','pb0_nhdhr_1257342'),
  #     nml_file = c('2_prep/sync/nhdhr_123423.nml', '2_prep/sync/nhdhr_1257342.nml'),
  #     meteo_file = c('2_prep/sync/NLDAS_time[0.351500]_x[307]_y[122].csv', '2_prep/sync/NLDAS_time[0.351500]_x[311]_y[136].csv'),
  #     export_file = c('3_run/sync/pb0_nhdhr_123423_temperatures.feather', '3_run/sync/pb0_nhdhr_1257342_temperatures.feather'),
  #     stringsAsFactors = FALSE)
  # )

  sim_ids <- read_feather(temperature_file) %>%
    left_join(data.frame(site_id = names(nml_list), stringsAsFactors = FALSE), .) %>% group_by(site_id) %>%
    summarize(n = length(unique(date))) %>% arrange(desc(n)) %>% pull(site_id)

  build_job_list(sim_ids, job_chunk = job_chunk, nml_list) %>%
    saveRDS(fileout)

}

build_job_df <- function(sim_ids, nml_list){

  nml_list <- nml_list[sim_ids]
  data.frame(stringsAsFactors = FALSE,
             site_id = sim_ids,
             sim_id = paste0('pball_', sim_ids),
             nml_file = paste0('2_prep/sync/', sim_ids, '.nml'),
             meteo_file = paste0('2_prep/sync/',
                                 sapply(1:length(nml_list), FUN = function(x) nml_list[[x]]$meteo_fl)))
}

build_job_list <- function(sim_ids, job_chunk, nml_list){
  start_idx <- seq(1, to = length(sim_ids), by = job_chunk)
  end_idx <- c(tail(start_idx - 1, -1), length(sim_ids))
  all_jobs <- list()
  for (i in 1:length(start_idx)){
    all_jobs[[i]] <- data.frame(stringsAsFactors = FALSE,
                                sim_id = paste0('pb0_', sim_ids[start_idx[i]:end_idx[i]]),
                                nml_file = paste0('2_prep/sync/', sim_ids[start_idx[i]:end_idx[i]], '.nml'),
                                meteo_file = paste0('2_prep/sync/', sapply(start_idx[i]:end_idx[i], FUN = function(x) nml_list[[x]]$meteo_fl)),
                                export_file = paste0('3_run/sync/pb0_', sim_ids[start_idx[i]:end_idx[i]], '_temperatures.feather'))
  }
  return(all_jobs)
}

failed_array_jobs <- function(fileout, old_jobs, job_chunk, nml_list, dummy){
  old_jobs <- readRDS(old_jobs)
  files_done <- dir('3_run/sync')
  ids_done <- files_done %>% stringr::str_remove('pb0_') %>% stringr::str_remove("_temperatures.feather")

  all_ids <- sapply(old_jobs, function(x) c(str_remove(x$sim_id, 'pb0_'))) %>% unlist() %>% as.vector()
  failed_ids <- all_ids[!all_ids %in% ids_done]
  build_job_list(failed_ids, job_chunk = job_chunk, nml_list) %>%
    saveRDS(fileout)
}

remove_wrr <- function(temp_feather){
  files_done <- dir('3_run/sync')
  ids_done <- files_done %>% stringr::str_remove('pb0_') %>% stringr::str_remove("_temperatures.feather")
  wrr_ids <- dir('../ms-pgdl-wrr/data_release/out/') %>% stringr::str_extract('nhd_[0-9]+') %>% na.omit() %>% as.character()
  winslow_xwalk <- readRDS('../lake-temperature-model-prep/2_crosswalk_munge/out/winslow_nhdhr_xwalk.rds')
  wrr_nhdhr <- winslow_xwalk %>% filter(WINSLOW_ID %in% wrr_ids)
  files_done[!ids_done %in% wrr_nhdhr$site_id & ids_done %in% read_feather(temp_feather)$nhdhr_id]
}

zip_glm_out <- function(zip_filepath, files){

  cd <- getwd()
  on.exit(setwd(cd))
  setwd('3_run/sync')
  zip(file.path('../out', basename(zip_filepath)), files)
  setwd(cd)
}

zip_meteo <- function(zip_filepath, pb0_files, nml_list){
  pb0_ids <- pb0_files %>% stringr::str_remove('pb0_') %>% stringr::str_remove("_temperatures.feather")

  meteo_files <- sapply(pb0_ids, function(x) nml_list[[x]]$meteo_fl, USE.NAMES = FALSE) %>% unlist %>% unique()
  cd <- getwd()
  on.exit(setwd(cd))
  setwd('2_prep/sync')
  zip(file.path('../../3_run/out', basename(zip_filepath)), meteo_files)
  setwd(cd)
}

mapping_nldas_nhdhr <- function(filepath, pb0_files, nml_list){
  pb0_ids <- pb0_files %>% stringr::str_remove('pb0_') %>% stringr::str_remove("_temperatures.feather")

  meteos <- sapply(pb0_ids, function(x) nml_list[[x]]$meteo_fl, USE.NAMES = T)
  meteos <- meteos[!sapply(meteos, is.null)]

  mapping <- data.frame(site_id = names(meteos),
                        meteo_file = unlist(meteos) %>% unname(),
                        stringsAsFactors = FALSE)
  feather::write_feather(mapping, filepath)
}

subset_temperature <- function(filepath, pb0_files, temp_feather){
  pb0_ids <- pb0_files %>% stringr::str_remove('pb0_') %>% stringr::str_remove("_temperatures.feather")

  temperatures <- read_feather(temp_feather) %>%
    filter(nhdhr_id %in% pb0_ids)

  ids_m_100 <- temperatures %>% group_by(nhdhr_id) %>% summarize(n = length(unique(date))) %>%
    filter(n >= 100) %>% pull(nhdhr_id) %>% unique()

  temperatures %>% filter(nhdhr_id %in% ids_m_100) %>% write_feather(filepath)
}


extract_metadata <- function(filepath, pb0_files, nml_list, area_file){
  pb0_ids <- pb0_files %>% stringr::str_remove('pb0_') %>% stringr::str_remove("_temperatures.feather")
  areas <- readRDS(area_file)

  # want nhd_id	surface area	max depth	latitude	longitude	K_d

  purrr::map(pb0_ids, .f = function(x){
    data.frame(site_id = x,
               surface_area = filter(areas, site_id == !!x) %>% pull(areas_m2),
               max_depth = nml_list[[x]]$lake_depth,
               latitude = nml_list[[x]]$latitude,
               longitude = nml_list[[x]]$longitude,
               K_d = nml_list[[x]]$Kw, stringsAsFactors = FALSE)
  }) %>% purrr::reduce(rbind) %>% feather::write_feather(filepath)

}
