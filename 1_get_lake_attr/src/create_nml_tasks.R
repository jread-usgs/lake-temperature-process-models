
create_nml_tasks <- function(lake_list, feature_nldas_coords){

  tasks <- lake_list %>% as_tibble() %>%
    left_join(feature_nldas_coords %>%
                as_tibble() %>%
                select(site_id, nldas_coord_x, nldas_coord_y) %>%
                mutate(site_id = as.character(site_id)), # site_id is a factor for feature_nldas_coords
              by = 'site_id') %>%
    dplyr::rename(tn = site_id)

  base_nml <- scipiper::create_task_step(
    step_name = 'base_nml',
    target_name = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      sprintf('1_get_lake_attr/out/glm_%s.nml.ind', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, tn==task_name)
      psprintf(
        "get_base_lake_nml(",
        "nhd_id = I('%s')," = cur_task$tn,
        "nldas_x = I('%s')," = cur_task$nldas_coord_x,
        "nldas_y = I('%s')," = cur_task$nldas_coord_y,
        "ind_file = target_name)")
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


# helper function to sprintf a bunch of key-value (string-variableVector) pairs,
# then paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  non_null_args <- which(!sapply(args, is.null))
  args <- args[non_null_args]
  argnames <- sapply(seq_along(args), function(i) {
    nm <- names(args[i])
    if(!is.null(nm) && nm!='') return(nm)
    val_nm <- names(args[[i]])
    if(!is.null(val_nm) && val_nm!='') return(val_nm)
    return('')
  })
  names(args) <- argnames
  strs <- mapply(function(template, variables) {
    spargs <- if(template == '') list(variables) else c(list(template), as.list(variables))
    do.call(sprintf, spargs)
  }, template=names(args), variables=args)
  paste(strs, collapse=sep)
}
