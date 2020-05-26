run_transfer_model <- function(output_fl,
                   base_nml,
                   src_nml,
                   target_meteo,
                   src_cd,
                   src_coef_mix_hyp,
                   src_sw_factor){

  if (file.exists(output_fl)){
    return()
  }

  source_id <- basename(tools::file_path_sans_ext(src_nml))
  sim_id <- basename(tools::file_path_sans_ext(base_nml))
  sim_dir <- file.path("sim-scratch", sim_id)

  on.exit(unlink(sim_dir, recursive = TRUE))
  dir.create(sim_dir)
  meteo_filepath <- file.path(sim_dir, paste0(sim_id, '.csv'))

  export_depth <- glmtools::read_nml(base_nml) %>% glmtools::get_nml_value('lake_depth')

  base_meteo <- target_meteo
  meteo_data <- readr::read_csv(base_meteo, col_types = 'Dddddddd', n_max = 14975)
  caldata_fl <- file.path(sim_dir, paste0(sim_id, '_obs.csv'))

  # filter data file, write to "calibration_obs.tsv" in sim_dir or pre-write the file?
  cal_obs <- feather::read_feather('2_prep/out/temperature_obs.feather') %>% filter(site_id == sim_id) %>%
    group_by(date, depth) %>% summarise(temp = mean(temp)) %>%
    select(DateTime = date, Depth = depth, temp)

  # the source nml is the *uncalibrated* source nml; the params set below are the three (or more) cal params:
  src_nml_obj <- glmtools::read_nml(src_nml)

  # getting the values for the calibrated parameters
  nml_args <- list(cd = src_cd, coef_mix_hyp = src_coef_mix_hyp, sw_factor = src_sw_factor)

  nml_args$meteo_fl <- basename(meteo_filepath)
  # write meteodata into fresh file
  readr::write_csv(driver_add_rain(meteo_data), path = meteo_filepath)
  this_nml_obj <- glmtools::set_nml(src_nml_obj, arg_list = nml_args)

  rmse <- tryCatch({
    nc_path <- run_glm(sim_dir, this_nml_obj, export_file = NULL)
    readr::write_csv(cal_obs, caldata_fl)

    last_time <- glmtools::get_var(nc_path, 'wind') %>%
      tail(1) %>% pull(DateTime)

    if (lubridate::ceiling_date(last_time) < as.Date(glmtools::get_nml_value(this_nml_obj, "stop"))){
      message('incomplete sim, ended on ', last_time)
      stop('incomplete sim, ended on ', last_time)
    }

    rmse <- extend_depth_calc_rmse(nc_path, field_file = caldata_fl,
                                   extend_depth = export_depth)
  }, error = function(e){
    message('returning error') #e
    return(-999)
  })

  feather::write_feather(tibble(target_id = sim_id, source_id = source_id, rmse = rmse), path = output_fl)

}
