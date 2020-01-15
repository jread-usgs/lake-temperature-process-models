

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
