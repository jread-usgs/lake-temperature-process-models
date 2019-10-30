
run_glm_cal <- function(nml_file, cal_params = c('cd','Kw','coef_wind_stir'), cal_file){

  # do calibration...

  return(nml_obj)
}


optim_multilake_glm <- function(driver_file, nml_file, train_file, test_file, sim_dir, glm_nix_dir){

  nhd_id <- basename(driver_file) %>% strsplit('[_]') %>% .[[1]] %>% .[2] %>% sprintf('nhd_%s', .)
  nml_obj <- read_nml(nml_file)
  cd_start <- get_nml_value(nml_obj, "cd")
  kw_start <- get_nml_value(nml_obj, "Kw")
  meteo_file <- basename(driver_file)

  results_log <- file.path(sim_dir, 'results_log.txt')
  file.create(results_log)

  # set all fixed nml values
  # check metweo file is named right in nml??
  file.copy(from = driver_file, to = sprintf('%s/%s', sim_dir, meteo_file))
  nml_obj <- set_nml(nml_obj, arg_list = list(meteo_fl = meteo_file))

  train_filepath <- sprintf('%s/train_data.csv', sim_dir)
  test_filepath <- sprintf('%s/test_data.csv', sim_dir)
  file.copy(from = train_file, to = train_filepath)
  file.copy(from = test_file, to = test_filepath)

  initial_params = c('cd'=cd_start, coef_wind_stir=0.23, Kw = kw_start)
  parscale = c('cd'=0.0001, coef_wind_stir=0.001, Kw = 0.1*kw_start)
  min_rmse <<- 10
  # optimize initial parameters
  out = optim(fn = run_cal_simulation, par=initial_params, control=list(parscale=parscale),
              train_filepath = train_filepath, sim_dir = sim_dir, nml_obj = nml_obj, results_log = results_log, glm_nix_dir = glm_nix_dir)
  results <- data.frame(as.list(out$par), train_rmse = out$value)
  results$test_rmse <- compare_to_field(sprintf('%s/output.nc', sim_dir),
                                        test_filepath,
                                        metric = 'water.temperature')
  results$nhd_id <- nhd_id

  export_temp(filepath = paste0('out/fig_2/', exper_id, "_temperatures.feather"), nml_file, sim_path = sprintf('%s/output.nc', sim_dir))

  unlink(sim_dir, recursive = TRUE)

  return(results)
}

run_cal_simulation <- function(par, train_filepath, sim_dir, nml_obj, results_log, glm_nix_dir){

  nml_path <- file.path(sim_dir,'glm2.nml')

  nml_obj <- set_nml(nml_obj, arg_list = as.list(par)) # custom param shifts
  write_nml(glm_nml = nml_obj, file = nml_path)

  rmse = tryCatch({

    sim = run_glm_copy(sim_dir, glm_nix_dir)
    last_time <- glmtools::get_var(sprintf('%s/output.nc', sim_dir), 'wind') %>%
      tail(1) %>% pull(DateTime)
    if (last_time < as.Date(as.Date(get_nml_value(nml_obj, "stop")))){
      stop('incomplete sim, ended on ', last_time)
    }
    rmse = compare_to_field(file.path(sim_dir, 'output.nc'),
                            field_file = train_filepath,
                            metric = 'water.temperature')
  }, error = function(e){
    cat(paste0(e$message,'\n'), file = results_log, append = TRUE)
    return(10) # a high RMSE value
  })


  # need to compare to obs and return NLL or RMSE stats; using rmse for now
  if (rmse < min_rmse){
    min_rmse <<- rmse
    cat(paste0(min_rmse,',',Sys.time(), '\n'), file = results_log, append = TRUE)
  }


  return(rmse)
}
