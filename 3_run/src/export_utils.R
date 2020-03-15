
library(dplyr)

export_temp <- function(filepath, nml_obj, nc_filepath){

  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)

  temp_data <- glmtools::get_temp(nc_filepath, reference = 'surface', z_out = export_depths)
  model_out <- glmtools::get_var(nc_filepath, var_name = 'hice') %>%
    dplyr::mutate(ice = hice > 0) %>% dplyr::select(-hice) %>%
    dplyr::left_join(temp_data, ., by = 'DateTime')
  feather::write_feather(model_out, filepath)

}

export_as_daily <- function(filepath, nml_obj, nc_filepath){

  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)

  temp_data <- glmtools::get_temp(nc_filepath, reference = 'surface', z_out = export_depths)
  rad_data <- glmtools::get_var(nc_filepath, reference = 'surface', z_out = 0, var_name = 'rad')
  model_out <- glmtools::get_var(nc_filepath, var_name = 'hice') %>%
    dplyr::left_join(temp_data, ., by = 'DateTime') %>%
    dplyr::left_join(rad_data, by = 'DateTime') %>%
    mutate(time = as.Date(lubridate::floor_date(DateTime, 'days'))) %>%
    group_by(time) %>% select(-DateTime) %>% summarize_all(mean) %>%
    dplyr::mutate(ice = hice > 0) %>% dplyr::select(-hice)

  # now summarize to daily
  feather::write_feather(model_out, filepath)

}
