
create_glm3_local_run_plan <- function(nml_file_group){

  nml_files <- yaml::yaml.load_file(nml_file_group)
  site_ids <- names(nml_files) %>% basename() %>% stringr::str_remove('_glm3.nml')
  files <- tibble(site_id = site_ids, nml_file = names(nml_files))

  run_glm3_step <- create_task_step(
    step_name = 'run_glm3',
    target_name = function(task_name, step_name, ...) {
      sprintf('4_toha/out/pb0_%s_temperatures.feather', task_name)
    },
    command = function(task_name, step_name, ...) {

      nml_file <- files %>% filter(site_id == task_name) %>% pull(nml_file)
      nml_obj <- read_nml(nml_file)
      meteo_fl <- get_nml_value(nml_obj, arg_name = 'meteo_fl')
      # need location of driver file
      # input of kw data
      # 2_prep/out/driver_file_group.yml
      # nml file?
      sprintf("run_glm3_model(export_fl = target_name,
      kw_data = kw_data,
      nml_file = '%s',
      site_id = I('%s'),
      meteo_fl = '2_prep/sync/%s')",
              nml_file, task_name, meteo_fl) # add remove/keep_IDs here later...
    }
  )


  create_task_plan(site_ids, list(run_glm3_step), final_steps='run_glm3', add_complete = FALSE)
}


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

create_pb0_local_run_plan <- function(...){
  site_ids <- unique(c(...))

  run_pb0_step <- create_task_step(
    step_name = 'run_pb0_model',
    target_name = function(task_name, step_name, ...) {
      # pb0_nhdhr_120020090_temperatures.feather
      sprintf('3_run/sync/pb0_%s_temperatures.feather', task_name)
    },
    command = function(task_name, step_name, ...) {
      sprintf("run_pb0_model(output_fl = target_name,
      nml_file = '2_prep/sync/%s.nml')",
              task_name)
    }
  )
  create_task_plan(site_ids, list(run_pb0_step), final_steps='run_pb0_model', add_complete = FALSE)

}

create_transfer_local_run_plan <- function(job_array_file, transfer_metamodel_file){

  job_array <- readRDS(job_array_file)
  job_target_ids <- sapply(1:length(job_array), function(x)job_array[[x]]$sim_id)
  all_source_ids <- job_array[[1]]$source_id # each target has all of the same source IDs


  combo_ids <- read_csv(transfer_metamodel_file) %>%
    mutate(combined_id = paste0(target_id, "_t|s_", source_id)) %>%
             pull(combined_id)

  run_transfer_step <- create_task_step(
     step_name = 'run_transfer',
     target_name = function(task_name, step_name, ...) {
       sprintf('4_transfer/out/%s_temperatures.feather', task_name)
     },
     command = function(task_name, step_name, ...) {
       string_pieces <- stringr::str_split(task_name, '_t|s_')[[1]]
       if(length(string_pieces) != 3){
         stop('task_name didnt split as expected')
       }
       this_target_id <- string_pieces[1]
       this_source_id <- string_pieces[3]

       job_idx <- which(job_target_ids == this_target_id)
       these_jobs <- job_array[[job_idx]]

       src_idx <- which(these_jobs$source_id == this_source_id)
       src_nml <- these_jobs$source_nml[src_idx]
       base_nml <- these_jobs$base_nml_file
       target_meteo <- these_jobs$meteo_file
       src_cd <- these_jobs$source_cd[src_idx]
       src_coef_mix_hyp <- these_jobs$source_coef_mix_hyp[src_idx]
       src_sw_factor <- these_jobs$source_sw_factor[src_idx]

       sprintf("run_transfer_model(output_fl = target_name,
      base_nml = '%s',
      src_nml = '%s',
      target_meteo = '%s',
      src_cd = I(%s),
      src_coef_mix_hyp = I(%s),
      src_sw_factor = I(%s))",
               base_nml, src_nml, target_meteo, src_cd, src_coef_mix_hyp, src_sw_factor)
     }
   )
   create_task_plan(combo_ids, list(run_transfer_step), final_steps='run_transfer', add_complete = FALSE)
}

create_transfer_tasks_makefile <- function(makefile, task_plan, final_targets){

  include <- "2_prep.yml"
  packages <- c('GLMr', 'glmtools', 'mda.lakes', 'dplyr', 'readr')
  sources <- c('3_run/src/run_glm_utils.R', '3_run/src/export_utils.R', '3_run/src/run_transfer_local.R')

  create_task_makefile(task_plan, makefile, include = include, packages = packages, sources = sources, final_targets = final_targets)
}

create_pb0_tasks_makefile <- function(makefile, task_plan, final_targets){

  packages <- c('GLMr', 'glmtools', 'dplyr', 'readr')
  sources <- c('3_run/src/run_glm_utils.R', '3_run/src/export_utils.R', '3_run/src/run_transfer_local.R')

  create_task_makefile(task_plan, makefile, packages = packages, sources = sources, final_targets = final_targets)
}
