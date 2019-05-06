
create_glm_sim_tasks <- function(nml_files,
                                 glm_inputs_yeti_path,
                                 drivers_yeti_path,
                                 drivers_time,
                                 glm_sim_yeti_path,
                                 glm_preds_yeti_path,
                                 glm_summary_yeti_path,
                                 feature_nldas_coords,
                                 depth_interval){

  nml_files <- readRDS(nml_files) # nml files on Yeti

  lake_list <- unlist(strsplit(unlist(strsplit(nml_files$files, 'glm_', fixed = T)), '.nml', fixed = T))

  feature_nldas_coords = readRDS(feature_nldas_coords)

  tasks <- lake_list %>% as_tibble() %>% rename(site_id = value) %>%
    left_join(feature_nldas_coords %>%
                as_tibble() %>%
                select(site_id, nldas_coord_x, nldas_coord_y) %>%
                mutate(site_id = as.character(site_id)), # site_id is a factor for feature_nldas_coords
              by = 'site_id') %>%
    dplyr::rename(tn = site_id)

  ###############
  tasks <- tasks[1:111,]  # cutting down on length for debugging; removing this when we want to make entire nml makefile
  ###############

  config_path <- scipiper::create_task_step(
    step_name = 'config_path',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn == task_name)
      sprintf('%s_config_path', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf("I('%s/glm_%s.nml')" = c(glm_inputs_yeti_path, cur_task$tn))
    }
  )

  drivers_path <- scipiper::create_task_step(
    step_name = 'drivers_path',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn == task_name)
      sprintf('%s_drivers_path', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf("I('%s/NLDAS_time[%s]_x[%s]_y[%s].csv')" = c(drivers_yeti_path,
                                                           drivers_time,
                                                           cur_task$nldas_coord_x,
                                                           cur_task$nldas_coord_y))
    }
  )

  preds_path <- scipiper::create_task_step(
    step_name = 'preds_path',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn == task_name)
      sprintf('%s_preds_path', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf("I('%s')" = glm_preds_yeti_path)
    }
  )

  summary_path <- scipiper::create_task_step(
    step_name = 'summary_path',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn == task_name)
      sprintf('%s_summary_path', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf("I('%s/%s_output.feather')" = c(glm_summary_yeti_path, cur_task$tn))
    }
  )

  depths <- scipiper::create_task_step(
    step_name = 'depths',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn == task_name)
      sprintf('%s_export_depths', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf(
        "export_depths(",
        "site_id = I('%s')," = cur_task$tn,
        "depth_interval = I(%.2f))" = depth_interval)
    }
  )

  glm_sim <- scipiper::create_task_step(
    step_name = 'glm_sim',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn == task_name)
      sprintf('glm_out_%s.nc', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf(
        "run_glm_simulation(",
        "config_path = %s_config_path," = cur_task$tn,
        "drivers_path = %s_drivers_path," = cur_task$tn,
        "sim_path = I('%s')," = glm_sim_yeti_path,
        "preds_path = %s_preds_path)" = cur_task$tn)
    }
  )

  glm_summary <- scipiper::create_task_step(
    step_name = 'glm_summary',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn == task_name)
      sprintf('glm_summary_%s.feather', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf(
        "export_temp(",
        "outfile = %s_summary_path," = cur_task$tn,
        "sim_out = I('%s/glm_out_%s.nc')," = c(glm_sim_yeti_path, cur_task$tn),
        "export_depths = %s_export_depths)" = cur_task$tn)
    }
  )


  step_list <- list(
    config_path,
    drivers_path,
    preds_path,
    summary_path,
    depths,
    glm_sim,
    glm_summary
  )

  glm_sim_task_plan <- scipiper::create_task_plan(
    task_names = tasks$tn,
    task_steps = step_list,
    add_complete = FALSE,
    final_steps = 'glm_summary',
    ind_dir = '2_model/out')
}
