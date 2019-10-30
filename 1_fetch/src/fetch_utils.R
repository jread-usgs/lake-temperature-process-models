
build_nml_list <- function(H_A_file, cd_file, lat_lon_file, len_wid_file, lake_depth_file, layer_thick_file, meteo_fl_file, kw_file){


  nml_df_data <- readRDS(cd_file) %>%
    inner_join(readRDS(lat_lon_file), by = 'site_id') %>%
    inner_join(readRDS(len_wid_file), by = 'site_id') %>%
    inner_join(readRDS(lake_depth_file), by = 'site_id') %>%
    inner_join(readRDS(layer_thick_file), by = 'site_id') %>%
    inner_join(readRDS(meteo_fl_file), by = 'site_id') %>%
    inner_join(readRDS(kw_file), by = 'site_id') %>%
    filter(file.exists(file.path('../lake-temperature-model-prep/7_drivers_munge/out', meteo_fl)))

  # now note the H_A_file is a list()
  nml_list <- split(nml_df_data, seq(nrow(nml_df_data))) %>% setNames(nml_df_data$site_id)

  H_A_list <- readRDS(H_A_file)

  H_A_site_ids <- names(H_A_list)

  for (id in H_A_site_ids[H_A_site_ids %in% names(nml_list)]){
    nml_list[[id]] = as.list(nml_list[[id]])
    nml_list[[id]]$A = H_A_list[[id]]$A
    nml_list[[id]]$H = H_A_list[[id]]$H
  }

  return(nml_list)
}
