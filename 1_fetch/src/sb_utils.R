fetch_sb_file <- function(fileout, sb_file = basename(fileout), sb_id, unzip = FALSE, dummy){

  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }

  files <- sbtools::item_file_download(sb_id = sb_id, names = sb_file, overwrite_file = TRUE,
                              destinations = ifelse(unzip, paste0(fileout, '.zip'), fileout))

  if (unzip){
    unzip(zipfile = files, overwrite = TRUE, files = basename(fileout), exdir = dirname(fileout))
    unlink(files)
  }
}
