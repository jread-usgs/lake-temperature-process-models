

run_glm <- function(sim_dir, nml_obj, export_file = NULL){

  glmtools::write_nml(nml_obj, file.path(sim_dir, 'glm2.nml'))
  GLMr::run_glm(sim_dir, verbose = FALSE)
  out_dir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_file <- paste0(glmtools::get_nml_value(nml_obj, arg_name = 'out_fn'), '.nc')
  nc_path <- file.path(sim_dir, out_dir, out_file)

  if (!is.null(export_file)){
    export_temp(filepath = export_file, nml_obj, nc_filepath = nc_path)
  }
  invisible(nc_path)
}
library(glmtools)
extend_depth_calc_rmse <- function(nc_path, field_file, extend_depth){

  # mostly borrows from `resample_to_field`

  field_obs <- readr::read_csv(field_file) %>%
    select(date = DateTime, depth = Depth, obs = temp)

  dup_rows <- duplicated(field_obs[,1:2])
  if (any(dup_rows)){
    mssg <- paste0(' see rows ', paste(which(dup_rows), collapse=','))
    append_mssg <- ifelse(sum(dup_rows) < 10, mssg, '')
    stop(paste0('field file has one or more rows with duplicate date and depths.', append_mssg))
  }

  # -- cover case w/ no overlap?
  if (nrow(field_obs) == 0){stop('no field data overlap with simulation period')}

  unq_z <- sort(unique(field_obs$Depth))

  # build water temp data.frame
  var_data <- get_var(file = nc_path, reference = 'surface', var_name = var_name,
                      z_out = unq_z, t_out = unique(field_obs$DateTime),
                      method = method, precision = precision)

  #make it skinny:
  joined_temperature <- tidyr::gather(var_data, depth_cd, temp, -DateTime) %>%
    mutate(depth = as.numeric(stringr::str_remove(depth_cd, 'temp_'))) %>% select(-depth_cd) %>%
    filter(depth <= extend_depth) %>% group_by(DateTime) %>% arrange(depth) %>%
    tidyr::fill(temp, .direction = 'down') %>% ungroup() %>%
    mutate(date = as.Date(lubridate::ceiling_date(DateTime, 'days'))) %>%
    select(date, depth, modeled = temp) %>% inner_join(field_obs)


  sqrt(mean((joined_temperature$modeled - joined_temperature$obs)^2, na.rm=TRUE))
}

driver_add_rain <- function(drivers, months=7:9, rain_add=1){

  d_month = as.POSIXlt(drivers$time)$mon + 1
  d_year  = as.POSIXlt(drivers$time)$year + 1900

  indx    = d_month %in% months
  n_years = length(unique(d_year[indx]))
  n_days  = sum(indx)

  per_day = (rain_add * n_years)/n_days

  drivers[indx,]$Rain = drivers[indx,]$Rain + per_day

  return(drivers)
}
