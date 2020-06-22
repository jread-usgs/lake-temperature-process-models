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

  # the source nml is the *uncalibrated* source nml; the params set below are the three (or more) cal params:
  src_nml_obj <- glmtools::read_nml(src_nml)

  # getting the values for the calibrated parameters
  nml_args <- list(cd = src_cd, coef_mix_hyp = src_coef_mix_hyp, sw_factor = src_sw_factor)

  nml_args$meteo_fl <- basename(meteo_filepath)
  # write meteodata into fresh file
  readr::write_csv(driver_add_rain(meteo_data), path = meteo_filepath)
  this_nml_obj <- glmtools::set_nml(src_nml_obj, arg_list = nml_args)

  tryCatch({
    nc_path <- run_glm(sim_dir, this_nml_obj, export_file = NULL)

    last_time <- glmtools::get_var(nc_path, 'wind') %>%
      tail(1) %>% pull(DateTime)

    if (lubridate::ceiling_date(last_time) < as.Date(glmtools::get_nml_value(this_nml_obj, "stop"))){
      message('incomplete sim, ended on ', last_time)
      stop('incomplete sim, ended on ', last_time)
    }

    export_temp(filepath = output_fl, nml_obj = this_nml_obj, nc_filepath = nc_path)
  }, error = function(e){
    message('returning error')
  })

}
