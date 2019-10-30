


create_glm_sim_tasks_makefile <- function(makefile, task_plan, remake_file){
  scipiper::create_task_makefile(
    makefile = makefile,
    task_plan = task_plan,
    include = remake_file,
    packages = c('dplyr', 'scipiper', 'mda.lakes', 'lakeattributes', 'GLMr', 'glmtools'),
    sources = c('2_model/src/run_glm_simulation.R', '2_model/src/simulation_summary.R'),
    file_extensions = c('nml', 'ind', 'nc', 'feather'))
}
