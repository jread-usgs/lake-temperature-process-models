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
