

create_toha_local_run_plan <- function(site_ids, nml_list){

  run_toha_step <- create_task_step(
    step_name = 'run_toha',
    target_name = function(task_name, step_name, ...) {
      sprintf('4_toha/out/%s_temp_opt.feather', task_name)
    },
    command = function(task_name, step_name, ...) {
      meteo_fl <- nml_list[[task_name]]$meteo_fl
      # need location of driver file
      # input of kw data
      # 2_prep/out/driver_file_group.yml
      # nml file?
      sprintf("run_toha_model(export_fl = target_name,
      kw_data = kw_data,
      nml_list = nml_list,
      site_id = I('%s'),
      meteo_fl = '2_prep/sync/%s')",
              task_name, meteo_fl) # add remove/keep_IDs here later...
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
