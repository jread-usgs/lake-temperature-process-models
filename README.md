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


This project uses rsync and ssh to pull/push data/predictions from Yeti in the `1_get_lake_attr` and `2_model` phase. For this to work, SSH keys must be set up for communication with Yeti:

  -  If you don't yet have a local ssh key pair, use `ssh-keygen -t rsa` from within a local terminal.
  -  Copy the public key to Yeti with `ssh-copy-id username@yeti.cr.usgs.gov` (also from within your local terminal). You can then check that you're set up by running `ssh username@yeti.cr.usgs.gov` from a terminal - it should log you in without a password.
  -  On Windows with RStudio, there will be a problem in that SSH/rsync assume your `.ssh` folder is at `~/.ssh`, but `~` means `C:/Users/username` within a terminal but `C:/Users/username/Documents` within RStudio. Therefore you should create a symlink for the `.ssh` folder by calling `ln -s ~/.ssh ~/Documents/.ssh` in a bash shell.

