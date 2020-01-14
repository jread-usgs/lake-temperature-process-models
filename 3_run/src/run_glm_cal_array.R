

task_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA'))

if(is.na(task_id)){
  stop("ERROR Can not read task_id, NA returned")
}

all_jobs <- readRDS('2_prep/out/glm_pball_array.rds')
source('3_run/src/run_glm_utils.R')
source('3_run/src/calibration_utils.R')


these_jobs <- all_jobs[[task_id]]

for (j in 1:nrow(these_jobs)){
  this_job <- these_jobs[j, ]
  sim_dir <- file.path(Sys.getenv('LOCAL_SCRATCH', unset="sim-scratch"), this_job$sim_id)
  dir.create(sim_dir, recursive = TRUE)
  nml_obj <- glmtools::read_nml(this_job$nml_file)
  base_meteo <- this_job$meteo_file
  # write the file to here:
  meteo_filepath <- file.path(sim_dir, paste0(this_job$sim_id, '.csv'))
  nml_obj <- glmtools::set_nml(nml_obj, arg_name = 'meteo_fl', basename(meteo_filepath))
  meteo_data <- readr::read_csv(base_meteo)
  readr::write_csv(driver_add_rain(meteo_data), path = meteo_filepath)

  # prep caldat_fl file to be used for calibration:
  stop('need to write to calibration file')

  nml_cal <- run_glm_cal(nml_file, sim_dir, caldata_fl)
  glmtools::write_nml(glm_nml = nml_cal, this_job$export_file)

  unlink(sim_dir, recursive = TRUE)
}


