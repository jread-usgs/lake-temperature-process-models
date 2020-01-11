
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
  out <- optim(fn = run_eval_glm, par=cal_starts, control=list(parscale=parscale),
               caldata_fl = caldata_fl, sim_dir = sim_dir, nml_obj = nml_obj)

  results <- data.frame(as.list(out$par), train_rmse = out$value)
  # within cal function, set_nml using params from optim, write nml
  # run model, verify legit sim and calculate/return calibration RSME, otherwise return 10 or 999 (something high)
  # optim returns $par for final params and $value for final RMSE; store these and/or write the whole modified nml

  return(nml_obj)
}

set_eval_glm <- function(){

}
