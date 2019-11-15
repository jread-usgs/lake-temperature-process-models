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



### setting up Yeti

This project is in `/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models`

I have set up the R library dir in `/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/Rlib` and used `.Renviron` file in the project root to specify `R_LIBS=...`

For installing packages, I did

```
module avail #list of all avail
module purge
module load tools/nco-4.4.4-gnu ??tools/nco-4.7.8-gnu 
module load tools/netcdf-4.3.2-gnu ??tools/netcdf-c-4.6.2-gnu

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
install.packages(c('remotes'))



remotes::install_github('GLEON/GLM3r')
remotes::install_github('GLEON/GLMr')
remotes::install_github('usgs-r/glmtools')
remotes::install_github('mrc-ide/syncr')
```
This project uses rsync and ssh to pull/push data/predictions from Yeti in the `1_get_lake_attr` and `2_model` phase. For this to work, SSH keys must be set up for communication with Yeti:

  -  If you don't yet have a local ssh key pair, use `ssh-keygen -t rsa` from within a local terminal.
  -  Copy the public key to Yeti with `ssh-copy-id username@yeti.cr.usgs.gov` (also from within your local terminal). You can then check that you're set up by running `ssh username@yeti.cr.usgs.gov` from a terminal - it should log you in without a password.
  -  On Windows with RStudio, there will be a problem in that SSH/rsync assume your `.ssh` folder is at `~/.ssh`, but `~` means `C:/Users/username` within a terminal but `C:/Users/username/Documents` within RStudio. Therefore you should create a symlink for the `.ssh` folder by calling `ln -s ~/.ssh ~/Documents/.ssh` in a bash shell.

sync to yeti
```
cd 2_prep/sync
rsync -avz .  jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/2_prep/sync

# sync the job lists to yeti:
cd 2_prep/out
rsync -avz .  jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/2_prep/out
```

sync from yeti
```
cd 3_run/sync
rsync -avz jread@yeti.cr.usgs.gov:/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-process-models/3_run/sync/. .
``

