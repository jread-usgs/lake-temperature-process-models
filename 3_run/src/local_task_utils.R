

create_toha_local_run_plan <- function(nml_file_group){

  nml_files <- yaml::yaml.load_file(nml_file_group)
  site_ids <- names(nml_files) %>% basename() %>% stringr::str_remove('_glm3.nml')
  files <- tibble(site_id = site_ids, nml_file = names(nml_files))

  run_toha_step <- create_task_step(
    step_name = 'run_toha',
    target_name = function(task_name, step_name, ...) {
      sprintf('4_toha/out/pb0_%s_temperatures_irradiance.feather', task_name)
    },
    command = function(task_name, step_name, ...) {

      nml_file <- files %>% filter(site_id == task_name) %>% pull(nml_file)
      nml_obj <- read_nml(nml_file)
      meteo_fl <- get_nml_value(nml_obj, arg_name = 'meteo_fl')
      # need location of driver file
      # input of kw data
      # 2_prep/out/driver_file_group.yml
      # nml file?
      sprintf("run_toha_model(export_fl = target_name,
      kw_data = kw_data,
      nml_file = '%s',
      site_id = I('%s'),
      meteo_fl = '2_prep/sync/%s')",
              nml_file, task_name, meteo_fl) # add remove/keep_IDs here later...
    }
  )


  create_task_plan(site_ids, list(run_toha_step), final_steps='run_toha', add_complete = FALSE)
}

create_toha_tasks_makefile <- function(makefile, task_plan, final_targets){

  include <- "2_prep.yml"
  packages <- c('GLMr', 'glmtools', 'mda.lakes', 'dplyr', 'readr')
  sources <- c('3_run/src/run_glm_utils.R', '3_run/src/export_utils.R')

  create_task_makefile(task_plan, makefile, include = include, packages = packages, sources = sources, final_targets = final_targets)
}
