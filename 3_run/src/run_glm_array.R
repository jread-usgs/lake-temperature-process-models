

# for single GLM PB0 jobs (no calibration), we'll get a list where each list element contains n lake jobs to run
list(
  data.frame(
    sim_id = c('pb0_nhdhr_123123','pb0_nhdhr_1252342'),
    nml_file = c('nhdhr_123123.nml', 'nhdhr_1252342.nml'),
    meteo_file = c('NLDAS_time[0.351500]_x[309]_y[127].csv', 'NLDAS_time[0.351500]_x[312]_y[130].csv')),
  data.frame(
    sim_id = c('pb0_nhdhr_123423','pb0_nhdhr_1257342'),
    nml_file = c('nhdhr_123423.nml', 'nhdhr_1257342.nml'),
    meteo_file = c('NLDAS_time[0.351500]_x[307]_y[122].csv', 'NLDAS_time[0.351500]_x[311]_y[136].csv'))
  )

task_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA'))

if(is.na(task_id)){
  stop("ERROR Can not read task_id, NA returned")
}


sim_dir <- file.path(Sys.getenv('LOCAL_SCRATCH', unset="sim-scratch"), sprintf('task_%s_%s', task_id, exper_id))
dir.create(sim_dir, recursive = TRUE)
