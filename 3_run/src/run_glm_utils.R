
run_toha_model <- function(nml_file, kw_data, site_id, meteo_fl, export_fl = NULL){
  sim_dir <- file.path('.sim-scratch', site_id)

  on.exit(unlink(sim_dir, recursive = TRUE))

  nml_obj <- read_nml(nml_file)

  start <- get_nml_value(nml_obj, 'start') %>% as.Date()
  stop <- get_nml_value(nml_obj, 'stop') %>% as.Date()

  dir.create(sim_dir)
  kw_data %>% filter(site_id == !!site_id,
                     time >= start & time <= stop) %>% select(time, Kd) %>%
    readr::write_csv(file.path(sim_dir, 'Kw_file.csv'))
  readr::read_csv(meteo_fl) %>% mutate(Rain = case_when(Snow > 0 ~ 0, TRUE ~ Rain)) %>%
    readr::write_csv(file.path(sim_dir, basename(meteo_fl)))
  #file.copy(meteo_fl, file.path(sim_dir, basename(meteo_fl)))

  # run once with hourly output:
  glmtools::write_nml(nml_obj, file.path(sim_dir, 'glm3.nml'))
  GLM3r::run_glm(sim_dir, verbose = FALSE)
  hourly_fn <- paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc')

  # run again with daily output:
  daily_fn <- 'daily_out'
  nml_obj <- set_nml(nml_obj, arg_list = list(nsave = 24, out_fn = daily_fn))
  glmtools::write_nml(nml_obj, file.path(sim_dir, 'glm3.nml'))
  GLM3r::run_glm(sim_dir, verbose = FALSE)

  out_dir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  daily_fn <- paste0(daily_fn, '.nc')

  nc_hourly <- file.path(sim_dir, out_dir, hourly_fn)
  nc_daily <- file.path(sim_dir, out_dir, daily_fn)


  if (!is.null(export_fl)){
    export_as_daily(filepath = export_fl, nml_obj, nc_daily = nc_daily,
                    nc_hourly = nc_hourly)
  }
  unlink(sim_dir, recursive = TRUE)
}

run_glm <- function(sim_dir, nml_obj, export_file = NULL){

  glmtools::write_nml(nml_obj, file.path(sim_dir, 'glm2.nml'))

  GLMr::run_glm(sim_dir, verbose = FALSE)
  out_dir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_file <- paste0(glmtools::get_nml_value(nml_obj, arg_name = 'out_fn'), '.nc')
  if (out_dir == '.'){
    nc_path <- file.path(sim_dir, out_file)
  } else {
    nc_path <- file.path(sim_dir, out_dir, out_file)
  }

  if (!is.null(export_file)){
    export_temp(filepath = export_file, nml_obj, nc_filepath = nc_path)
  }
  invisible(nc_path)
}

library(glmtools)
extend_depth_calc_rmse <- function(nc_path, field_file, extend_depth){

  # mostly borrows from `resample_to_field`

  field_obs <- glmtools::read_field_obs(field_file)

  dup_rows <- duplicated(field_obs[,1:2])
  if (any(dup_rows)){
    mssg <- paste0(' see rows ', paste(which(dup_rows), collapse=','))
    append_mssg <- ifelse(sum(dup_rows) < 10, mssg, '')
    stop(paste0('field file has one or more rows with duplicate date and depths.', append_mssg))
  }

  # -- cover case w/ no overlap?
  if (nrow(field_obs) == 0){stop('no field data overlap with simulation period')}

  unq_z <- sort(unique(field_obs$Depth))

  method = 'match'
  precision = 'days'
  # build water temp data.frame
  var_data <- get_var(file = nc_path, reference = 'surface', var_name = 'temp',
                      z_out = unq_z, t_out = unique(field_obs$DateTime),
                      method = method, precision = precision)

  # as.Date:
  field_obs <- readr::read_csv(field_file) %>%
    select(date = DateTime, depth = Depth, obs = temp)

  #make it skinny:
  joined_temperature <- tidyr::gather(var_data, depth_cd, temp, -DateTime) %>%
    mutate(depth = as.numeric(substring(depth_cd, first = 6, last = 1000000L))) %>%
             select(-depth_cd) %>%
    filter(depth <= extend_depth) %>% group_by(DateTime) %>% arrange(depth) %>%
    tidyr::fill(temp, .direction = 'down') %>% ungroup() %>%
    mutate(date =  as.Date(DateTime)) %>% #as.Date(lubridate::ceiling_date(DateTime, 'days'))) was causing /usr/lib64/libstdc++.so.6: version `CXXABI_1.3.8' not found (required by /cxfs/projects/usgs/water/iidd/data-sci/lake-temp/glm-optim-wrr/Rlib/stringi/libs/stringi.so)[jread@yeti-login20 lake-temperature-process-models] scancel -u jread

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
