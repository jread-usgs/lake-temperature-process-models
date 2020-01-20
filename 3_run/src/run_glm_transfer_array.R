

# all_jobs <- list(
#   list(
#     sim_id = 'nhdhr_123423',
#     nml_file = '2_prep/sync/nhdhr_123423.nml',
#     meteo_file = '2_prep/sync/NLDAS_time[0.351500]_x[225]_y[159].csv',
#     source_id = c('nhdhr_166868607','nhdhr_166868799'),
#     source_cd = c(0.00123, 0.0122),
#     source_Kw = c(0.23, 0.52),
#     source_coef_wind_stir = c(0.22, 0.25),
#     export_file = '3_run/sync/transfer_nhdhr_123423_rmse.csv'
#   ))


task_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA'))

if(is.na(task_id)){
  stop("ERROR Can not read task_id, NA returned")
}

all_jobs <- readRDS('2_prep/out/glm_transfer_array.rds')
source('3_run/src/run_glm_utils.R')
source('3_run/src/export_utils.R')
library(dplyr)

these_jobs <- all_jobs[[task_id]]

sim_dir <- file.path(Sys.getenv('LOCAL_SCRATCH', unset="sim-scratch"), these_jobs$sim_id)
meteo_filepath <- file.path(sim_dir, paste0(these_jobs$sim_id, '.csv'))

base_nml <- these_jobs$base_nml_file
nml_obj <- glmtools::read_nml(base_nml)
nml_obj <- glmtools::set_nml(nml_obj, arg_name = 'meteo_fl', basename(meteo_filepath))

base_meteo <- these_jobs$meteo_file
export_file <- these_jobs$export_file
caldata_fl <- file.path(sim_dir, paste0(this_job$site_id, '_obs.csv'))

# filter data file, write to "calibration_obs.tsv" in sim_dir or pre-write the file?
cal_obs <- feather::read_feather('2_prep/out/temperature_obs.feather') %>% filter(site_id == these_jobs$sim_id) %>%
  group_by(date, depth) %>% summarise(temp = mean(temp)) %>%
  select(DateTime = date, Depth = depth, temp)

out_file <- file(export_file, "w")
cat(sprintf('source_id,rmse,sim_time\n'), file = out_file)
for (j in 1:length(these_jobs$source_id)){
  dir.create(sim_dir, recursive = TRUE) # recreate each time for freshness
  info_names <- names(these_jobs)[names(these_jobs) != 'source_id']
  param_names <- info_names[grepl(info_names, pattern = '^source')]
  nml_args <- setNames(lapply(param_names, FUN = function(x){
    these_jobs[[x]][j]
  }), substring(param_names, first = 8, last = 1000000L)) # remove "source_"
  # write meteodata into fresh file
  readr::write_csv(driver_add_rain(meteo_data), path = meteo_filepath)
  this_nml_obj <- glmtools::set_nml(nml_obj, arg_list = nml_args)

  rmse <- tryCatch({
    nc_path <- run_glm(sim_dir, this_nml_obj, export_file = NULL)
    readr::write_csv(cal_obs, caldata_fl)

    last_time <- glmtools::get_var(nc_path, 'wind') %>%
      tail(1) %>% pull(DateTime)

    if (last_time < as.Date(glmtools::get_nml_value(nml_obj, "stop"))){
      stop('incomplete sim, ended on ', last_time)
    }
    rmse <- glmtools::compare_to_field(nc_path, field_file = caldata_fl,
                                       metric = 'water.temperature')
  }, error = function(e){
    message(e)
    return(-999)
  })
  unlink(sim_dir, recursive = TRUE)

  sim_time = format(Sys.time(), '%Y-%m-%d %H:%M')
  cat(sprintf('%s,%s,%s\n', these_jobs$source_id[j], rmse, sim_time), file = out_file)

}
close(out_file)

