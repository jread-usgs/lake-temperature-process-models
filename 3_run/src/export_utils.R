
library(dplyr)

export_temp <- function(filepath, nml_obj, nc_filepath){

  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)

  temp_data <- glmtools::get_temp(nc_filepath, reference = 'surface', z_out = export_depths) %>%
    mutate(date = as.Date(lubridate::ceiling_date(DateTime, 'days'))) %>% select(-DateTime)
  model_out <- glmtools::get_var(nc_filepath, var_name = 'hice') %>%
    dplyr::mutate(ice = hice > 0, date = as.Date(lubridate::ceiling_date(DateTime, 'days'))) %>% dplyr::select(-hice, -DateTime) %>%
    dplyr::left_join(temp_data, ., by = 'date') %>%
    rename(DateTime = date)
  feather::write_feather(model_out, filepath)

}

export_as_daily <- function(filepath, nml_obj, nc_daily, nc_hourly){

  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)

  #hourly export:
  rad_data <- glmtools::get_var(nc_hourly, reference = 'surface', z_out = 0, var_name = 'rad') %>%
    mutate(time = as.Date(lubridate::floor_date(DateTime, 'days'))) %>%
    group_by(time) %>% select(-DateTime) %>% summarize(rad_0 = mean(rad_0))

  #daily export:
  temp_data <- glmtools::get_temp(nc_daily, reference = 'surface', z_out = export_depths)
  kw_data <- glmtools::get_var(nc_daily, var_name = 'extc_coef', reference = 'surface', z_out = 0)
  model_out <- glmtools::get_var(nc_daily, var_name = 'hice') %>%
    dplyr::left_join(temp_data, ., by = 'DateTime') %>%
    dplyr::left_join(kw_data, by = 'DateTime') %>%
    mutate(time = as.Date(lubridate::floor_date(DateTime, 'days'))) %>%
    dplyr::mutate(ice = hice > 0) %>% dplyr::select(-hice, -DateTime) %>%
    dplyr::inner_join(rad_data, by = 'time') %>% # rad_data likely has one more date, and early and incomplete day one of sim
    select(time, everything())

  feather::write_feather(model_out, filepath)

}
