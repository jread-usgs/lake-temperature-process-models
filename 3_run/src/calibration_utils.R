
run_glm_cal <- function(nml_file, sim_dir, cal_params = c('cd','Kw','coef_wind_stir'),
                        cal_parscale = c('cd' = 0.0001, 'Kw' = expression(0.1 * Kw), 'coef_wind_stir' = 0.001),
                        caldata_fl){

  # read cal/obs file, filter to _this_ lake if needed, write to csv/tsv so it will work with glmtools::compare_to_field

  nml_obj <- glmtools::read_nml(nml_file)
  # get starting values for params that will be modified
  cal_starts <- sapply(cal_params, FUN = function(x) glmtools::get_nml_value(nml_obj, arg_name = x))

  # define parscale for each cal param
  # have to do all of this to match the WRR method of parscale Kw being a function of Kw:
  parscale <- sapply(names(cal_parscale), FUN = function(x) {
    if (class(cal_parscale[[x]]) == 'call') {
      eval(cal_parscale[[x]], envir = setNames(data.frame(cal_starts[[x]]), x))
    } else cal_parscale[[x]]
    })

  # use optim to pass in params, parscale, calibration_fun, compare_file, sim_dir
  out <- optim(fn = set_eval_glm, par=cal_starts, control=list(parscale=parscale),
               caldata_fl = caldata_fl, sim_dir = sim_dir, nml_obj = nml_obj)

  nlm_obj <- glmtools::set_nml(nml_obj, arg_list = as.list(out$par))

  # write the rmse and other details into a new block in the nml "results"
  nml_obj$results <- list(rmse = out$value,
                          sim_time = format(Sys.time(), '%Y-%m-%d %H:%M'),
                          cal_params = cal_params,
                          cal_parscale = parscale)

  return(nml_obj)
}

set_eval_glm <- function(par, caldata_fl, sim_dir, nml_obj){
  # set params, run model, check valid, calc rmse
  # message(paste(as.list(par), collapse = ', ', sep = '| '))

  # run model, verify legit sim and calculate/return calibration RSME, otherwise return 10 or 999 (something high)
  rmse = tryCatch({

    nml_obj <- glmtools::set_nml(nml_obj, arg_list = as.list(par))
    # from "run_glm_utils.R"
    sim_fl <- run_glm(sim_dir, nml_obj)

    last_time <- glmtools::get_var(sim_fl, 'wind') %>%
      tail(1) %>% pull(DateTime)

    if (last_time < as.Date(glmtools::get_nml_value(nml_obj, "stop"))){
      stop('incomplete sim, ended on ', last_time)
    }

    rmse <- glmtools::compare_to_field(sim_fl, field_file = caldata_fl,
                                       metric = 'water.temperature')

  }, error = function(e){
    message(e)
    return(99) # a high RMSE value
  })
  return(rmse)
}
