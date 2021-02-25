list_to_stack <- function(raster_list, new_res, dest_crs = CRS("+proj=longlat")) {
  require(raster)
  for(i in seq_along(raster_list)) {
    raster_list[[i]][raster_list[[i]] == 0] <- NA
    raster_list[[i]] <- trim(raster_list[[i]])
  }
  ext_list <- lapply(X = raster_list, function(x) {as.matrix(x@extent)})
  matrix_extent <- matrix(unlist(ext_list), ncol=length(ext_list))
  rownames(matrix_extent)<-c("xmin", "ymin", "xmax", "ymax")
  best_extent <- extent(min(matrix_extent[1,]), max(matrix_extent[3,]), 
                        min(matrix_extent[2,]), max(matrix_extent[4,]))
  res_list2 <- lapply(raster_list, resample, 
                      raster(ext = best_extent, crs = dest_crs, resolution = new_res))
  res_stack <- stack(res_list2)
  return(res_stack)
}
