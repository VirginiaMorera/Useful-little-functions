list_to_stack <- function(raster_list) {
  require(raster)
  ext_list <- lapply(X = raster_list, function(x) {as.matrix(x@extent)})
  
  matrix_extent <- matrix(unlist(ext_list), ncol=length(ext_list))
  rownames(matrix_extent)<-c("xmin", "ymin", "xmax", "ymax")
  
  # create an extent with the extrem values of your extent
  best_extent <- extent(min(matrix_extent[1,]), max(matrix_extent[3,]), 
                        min(matrix_extent[2,]), max(matrix_extent[4,]))
  res_list <- lapply(X = raster_list, function(x) {res(x)})
  min_res <- min(c(unlist(res_list)))
  res_list2 <- lapply(raster_list, resample, 
                      raster(ext = best_extent, crs = ras_list[[1]]@crs, resolution = min_res))
  res_stack <- stack(res_list2)
  return(res_stack)

  }
