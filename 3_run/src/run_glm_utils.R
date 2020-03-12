
run_toha_model <- function(nml_file, kw_data, site_id, meteo_fl, export_fl = NULL){
  sim_dir <- '.sim-scratch'

  cdir <- getwd()
  on.exit(setwd(cdir))

  nml_obj <- glmtools::read_nml(nml_file)
  start <- get_nml_value(nml_obj, 'start') %>% as.Date()
  stop <- get_nml_value(nml_obj, 'stop') %>% as.Date()

  kw_data %>% filter(site_id == !!site_id,
                     time >= start & time <= stop) %>% select(time, Kd) %>% mutate(Kd = mean(Kd)) %>%
    readr::write_csv(file.path(sim_dir, 'Kw_file.csv'))
  # filter and write Kw_file; check dates!!
  readr::read_csv(meteo_fl) %>% driver_add_rain() %>%
    readr::write_csv(file.path(sim_dir, 'meteo_fl.csv'))

  # read meteo, add rain, write to file in sim dir
  # adjust nml: nsave, Kw_file, meteo_fl, start, stop
  nml_obj <- set_nml(nml_obj, arg_list = list(
    stop = '2018-12-31',
    meteo_fl = 'meteo_fl.csv',
    #nsave = 1,
    max_layer_thick = 0.75,
    out_fn = 'constant_kw_GLM3',
    coef_wind_stir = 0.23,
    out_dir = '.'
  ))
  #nml_obj[['glm_setup']]$Kw_file <- 'Kw_file.csv'
  nml_obj$light$Kw_file <- 'Kw_file.csv'

  glmtools::write_nml(nml_obj, file.path(sim_dir, 'glm3.nml'))


  setwd(sim_dir)

  Sys.setenv(LD_LIBRARY_PATH=system.file('extbin/nixGLM', package = "GLMr"))
  system2('/media/sf_mntoha-data-release/TOSS_DELETE/glm', wait = TRUE, stdout = NULL,
          stderr = NULL)
  #nc_path <- run_glm(sim_dir, nml_obj)
  browser()

  unlink(sim_dir)
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
