
#' get the x and y indices of cells that correspond to each nhd_id
#'
#' @param cell_grid an `st_sf` of polygons with fields "x" and "y"
#' @param points spatial points sharing the same CRS as `cell_grid`
#'
#' @return a data.frame with nhd_id that corresponds to the cell x and y fields
feature_to_cell_coords <- function(ind_file, cell_grid, points){
  which_cell <- st_intersects(points, cell_grid) # tells which NLDAS grid cell contains each point

  #  all features are within grid cell
  nhd_nldas_coords <- points %>%
    mutate(nldas_cell_row = as.numeric(which_cell)) %>%
    left_join(as_tibble(cell_grid) %>% mutate(nldas_cell_row = as.numeric(row.names(cell_grid))) %>% select(x, y, nldas_cell_row),
              by = 'nldas_cell_row') %>%
    select(-nldas_cell_row) %>%
    rename(nldas_coord_x = x, nldas_coord_y = y)

  # write and post the output
  data_file <- as_data_file(ind_file)
  saveRDS(nhd_nldas_coords, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file)
}
