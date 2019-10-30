

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
