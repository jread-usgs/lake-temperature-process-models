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
