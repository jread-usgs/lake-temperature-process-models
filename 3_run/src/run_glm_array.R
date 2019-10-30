

# for single GLM PB0 jobs (no calibration), we'll get a list where each list element contains n lake jobs to run
list(
  data.frame(
    sim_id = c('pb0_nhdhr_166868607','pb0_nhdhr_166868799'),
    nml_file = c('2_prep/sync/nhdhr_166868607.nml', '2_prep/sync/nhdhr_166868799.nml'),
    meteo_file = c('2_prep/sync/NLDAS_time[0.351500]_x[225]_y[159].csv', '2_prep/sync/NLDAS_time[0.351500]_x[226]_y[158].csv'),
    export_file = c('3_run/sync/pb0_nhdhr_1668686073_temperatures.feather', '3_run/sync/pb0_nhdhr_166868799_temperatures.feather'),
    stringsAsFactors = FALSE),
  data.frame(
    sim_id = c('pb0_nhdhr_123423','pb0_nhdhr_1257342'),
    nml_file = c('2_prep/sync/nhdhr_123423.nml', '2_prep/sync/nhdhr_1257342.nml'),
    meteo_file = c('2_prep/sync/NLDAS_time[0.351500]_x[307]_y[122].csv', '2_prep/sync/NLDAS_time[0.351500]_x[311]_y[136].csv'),
    export_file = c('3_run/sync/pb0_nhdhr_123423_temperatures.feather', '3_run/sync/pb0_nhdhr_1257342_temperatures.feather'),
    stringsAsFactors = FALSE)
  )

task_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA'))

if(is.na(task_id)){
  stop("ERROR Can not read task_id, NA returned")
}

all_jobs <- readRDS('2_prep/out/glm_pb0_array.rds')
source('3_run/src/run_glm_utils.R')

these_jobs <- all_jobs[[task_id]]

for (j in 1:nrow(these_jobs)){
  this_job <- these_jobs[j, ]
  sim_dir <- file.path(Sys.getenv('LOCAL_SCRATCH', unset="sim-scratch"), this_job$sim_id)
  dir.create(sim_dir, recursive = TRUE)
  nml_obj <- glmtools::read_nml(this_job$nml_file)
  meteo_filepath <- file.path(sim_dir, glmtools::get_nml_value(nml_obj, 'meteo_fl'))
  file.copy(this_job$meteo_file, to = meteo_filepath)
  run_glm(sim_dir, nml_obj, export_file = this_job$export_file)
  unlink(sim_dir, recursive = TRUE)
}


