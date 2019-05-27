
create_nml_tasks <- function(lake_list, drivers_yeti_path, drivers_time, feature_nldas_coords, glm_start, glm_end, dt, nsave){
  feature_nldas_coords = readRDS(feature_nldas_coords)

  tasks <- lake_list %>% as_tibble() %>%
    left_join(feature_nldas_coords %>%
                as_tibble() %>%
                select(site_id, nldas_coord_x, nldas_coord_y) %>%
                mutate(site_id = as.character(site_id)), # site_id is a factor for feature_nldas_coords
              by = 'site_id') %>%
    dplyr::rename(tn = site_id)

  ###############
  #tasks <- tasks[1:10,]
  #tasks <- tasks[19300:19308,]  # cutting down on length for debugging; removing this when we want to make entire nml makefile
  ###############

  base_nml <- scipiper::create_task_step(
    step_name = 'base_nml',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      sprintf('1_get_lake_attr/tmp/glm_%s.nml', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf(
        "get_base_lake_nml(",
        "nml_file = target_name,",
        "site_id = I('%s')," = cur_task$tn,
        "nldas_x = I('%s')," = cur_task$nldas_coord_x,
        "nldas_y = I('%s')," = cur_task$nldas_coord_y,
        "drivers_yeti_path = I('%s')," = drivers_yeti_path,
        "drivers_time = I('%s')," = drivers_time,
        "start = I('%s')," = glm_start,
        "stop = I('%s')," = glm_end,
        "dt = I(%d)," = dt,
        "nsave = I(%d))" = nsave)
    })

  step_list <- list(
    base_nml
  )

  nml_task_plan <- scipiper::create_task_plan(
    task_names = tasks$tn,
    task_steps = step_list,
    add_complete = FALSE,
    final_steps = 'base_nml',
    ind_dir = '1_get_lake_attr/out')
}


