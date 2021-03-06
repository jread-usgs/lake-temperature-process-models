# lake-temperature-process-models

Dependent files from pipeline #1 lake-temperature-model-prep:
  -  feature_crosswalk.rds and .ind 
  -  lakes_sf.rds and .ind
  -  NLDAS_grid coords and cell resolution  

 The following files are dependencies in pipeline #3 lake-temperature-neural-networks:
  -  [`feature_nldas_coords.rds`](https://drive.google.com/drive/u/1/folders/1pbhIjfYUPZ4lEICm5zwJFjIjGYEz1qwi) and [`.ind`](https://github.com/USGS-R/lake-temperature-neural-networks/tree/master/in)

If any of these files are changed / updated in this pipeline, remember to: 
  1. copy the update .rds file to the dependent pipeline's drive (which is hyperlinked above) and to _ALSO_
  2. copy the updated .ind file to the dependent pipeline's github repository (which is also hyperlinked above)

### end around on the batch jobs
In order to get around the issue with UV being down, my VM being slow, and normal being even slower in batch mode, I set up a single node in normal for interactive mode, and ran scmake in R.

I used rsync to get the new task table up to Yeti:
```
rsync -avz 3_pb0_src_trg_tasks.yml jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_pb0_src_trg_tasks.yml

#or
rsync -avz 3_pb0_hyperscales_tasks.yml jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_pb0_hyperscales_tasks.yml
```
Used salloc to get 7 hours in interactive (in the end, I needed 5.5hrs to run 450 models):
```
salloc -A cida -n 1 -p normal -t 7:00:00
```
then ssh'd into the node I was given, and from there, got into the working directory
```
ssh n3-98
cd /cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models
```

As a test of loop_tasks in parallel, I asked for 4 cores
```
salloc -A cida -n 4 -p normal -t 7:00:00

rsync -avz pb0_src_trg_plan.rds jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/pb0_src_trg_plan.rds
rsync -avz hyperscales_pb0_plan.rds  jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/hyperscales_pb0_plan.rds
```
loaded modules on the node and then scipiper::loop_tasks() after installing `foreach` and `doParallel` on Yeti:
```r
loop_tasks(task_plan = readRDS('pb0_src_trg_plan.rds'), task_makefile = '3_pb0_src_trg_tasks.yml', n_cores = 4)
#or
loop_tasks(task_plan = readRDS('hyperscales_pb0_plan.rds'), task_makefile = '3_pb0_hyperscales_tasks.yml', n_cores = 8)
```

### setting up Yeti

This project is in `/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models`

I have set up the R library dir in `/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/Rlib` and used `.Renviron` file in the project root to specify `R_LIBS=...`

For installing packages, I did

```
module avail #list of all avail
module purge #DON'T DO THIS???
module load legacy # had to do this w/ Yeti refresh
module load R/3.6.3
module load tools/nco-4.7.8-gnu 
module load tools/netcdf-c-4.3.2-intel #tools/netcdf-c-4.6.2-gnu need this because there is a Yeti error where 
#module load gdal/2.2.0-gcc
#module load proj/5.2.0-gcc-7.1.0


#module load tools/nco-4.4.4-gnu ??tools/nco-4.7.8-gnu 
#module load tools/netcdf-4.3.2-gnu ??tools/netcdf-c-4.6.2-gnu

 module list
Currently Loaded Modulefiles:
  1) tools/nco-4.4.4-gnu      3) tools/hdf5-1.8.13-gnu    5) tools/netcdf-4.3.2-gnu
  2) tools/hdf-4.2.10-gnu     4) tools/szip-2.1-gnu
  
  WORKED!!
install.packages("GLMr", repos=c("https://owi.usgs.gov/R",getOption("repos")))

??
module load hdf5/1.8.18-gcc6.1.0
[jread@yeti-login20 lake-temperature-process-models] module load netcdf/4.4.1.1-gcc6.1.0

```

then 
`R`

from R:
```r
install.packages(c('stringi','stringr','dplyr','tidyr','rLakeAnalyzer','lubridate','remotes','ncdf4','readr','feather'))

library(remoates)
install_github('tidyverse/dplyr@v0.8.5')
install_version("vctrs", version = "0.2.4", repos = "http://cran.us.r-project.org")
install_version("tidyr", version = "1.0.0", repos = "http://cran.us.r-project.org")

#remotes::install_github('GLEON/GLM3r')
#remotes::install_github('GLEON/GLMr')
remotes::install_github('usgs-r/glmtools')
#remotes::install_github('mrc-ide/syncr')
```
This project uses rsync and ssh to pull/push data/predictions from Yeti in the `1_get_lake_attr` and `2_model` phase. For this to work, SSH keys must be set up for communication with Yeti:

  -  If you don't yet have a local ssh key pair, use `ssh-keygen -t rsa` from within a local terminal.
  -  Copy the public key to Yeti with `ssh-copy-id username@yeti.cr.usgs.gov` (also from within your local terminal). You can then check that you're set up by running `ssh username@yeti.cr.usgs.gov` from a terminal - it should log you in without a password.
  -  On Windows with RStudio, there will be a problem in that SSH/rsync assume your `.ssh` folder is at `~/.ssh`, but `~` means `C:/Users/username` within a terminal but `C:/Users/username/Documents` within RStudio. Therefore you should create a symlink for the `.ssh` folder by calling `ln -s ~/.ssh ~/Documents/.ssh` in a bash shell.



### my workflow
I build things locally and some (not all) sync to Yeti as part of the process. 
Seems rsync is faster than the cross-platform ssh package
I need to sync nmls and drivers to yeti, then use the "out" job list (rds) to tell yeti what to do
When the job list is on yeti, I need to modify the batch file with the same number of array jobs that appear in the job table
When the jobs are done, I sync _from_ yeti (see below) the feather files. 
Then I run a failed job array (same process as above) which picks up maybe a few more lakes when they run again. I don't know why they'd fail the first time but not the second. Doesn't make sense to me and I haven't dug into the lobs. 

So, this is clunky and seems that if I used `drake` instead on Yeti, I would be happier. 

Still some pain points with moving files around to and from Yeti. Kinda slow and maybe I should zip files or something (although rsync must be compressing, right?)

sync to yeti
```
cd 2_prep/sync
rsync -avz .  jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/2_prep/sync

# sync the job lists to yeti:
cd 2_prep/out
rsync -avz .  jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/2_prep/out

# sync task table to yeti:
rsync -avz 3_pb0_src_trg_tasks.yml jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_pb0_src_trg_tasks.yml

rsync -avz 3_pb0_hyperscales_tasks.yml jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_pb0_hyperscales_tasks.yml
```

sync from yeti
```
cd 3_run/sync
rsync -avz jread@yeti-dtn1.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_run/sync/. .

cd 3_run/out
rsync -avz jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_run/out/pb0_src_trg_tasks.rds.ind pb0_src_trg_tasks.rds.ind 

rsync -avz jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_run/out/pb0_hyperscales_tasks.ind pb0_hyperscales_tasks.ind

```

