

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


create_transfer_local_run_plan <- function(job_array_file, transfer_metamodel_file, target_range){

  job_array <- readRDS(job_array_file)
  job_target_ids <- sapply(1:length(job_array), function(x)job_array[[x]]$sim_id)[target_range[1]:target_range[2]]
  all_source_ids <- job_array[[1]]$source_id # each target has all of the same source IDs

  combo_ids <- sapply(job_target_ids, function(x) paste0(x, "_t|s_", all_source_ids)) %>% unlist() %>% c()

   transfer_data <- read_csv(transfer_metamodel_file) %>%
     mutate(target_id = paste0('nhdhr_', `target_id(nhdhr)`),
            source_id = paste0('nhdhr_', `best_predicted_site_id (nhdhr)`)) %>%
     select(target_id, source_id, predicted_rmse)

   run_transfer_step <- create_task_step(
     step_name = 'run_transfer',
     target_name = function(task_name, step_name, ...) {
       sprintf('4_transfer/out/%s_rmse.feather', task_name)
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
