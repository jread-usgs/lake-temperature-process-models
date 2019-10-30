

yeti_put <- function(local_dir, dest_dir, files){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@yeti.cr.usgs.gov', user))
  on.exit(ssh::ssh_disconnect(session = session))

  file_paths = sprintf('%s/%s', local_dir, files)

  ssh::scp_upload(session = session, files = file_paths, to = dest_dir)

}
