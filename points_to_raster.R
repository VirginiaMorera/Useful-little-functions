# this function will take a data.frame or data.table with columns for Lon and Lat  and will calculate a kernel for each ID, and then calculate a raster where the value of each cell is the count of areas present in that cell. 

# x must be a dataframe with columns Longitude, Latitude and ID
# Lon is the name of the column containing Longitude
# Lat is the name of the column containing Latitude
# proj is the CRS object containing the projection of the data
# ID is the column containing the ID to calculate the kernels for
# perc is the value of the % utilisation distribution we want to calculate
# min.pos is the minimum number of positions in each group necessary to calculate a kernelUD (minimum is 5 since kernelUD function doesn't work with less than 5)
# mask is a sf or SpatialPolygonDataFrame object containing the polygons to plot over the raster (e.g land if the raster is marine). Only for plotting purposes

points_to_raster <- function(x, Lon = "Longitude", Lat = "Latitude", proj = CRS("+init=epsg:4326"), ID = "Name", 
                             perc = 50, min.pos = 5, mask = world) {
  require(sf)
  require(sp)
  require(plyr)
  require(tidyverse)
  require(fasterize)
  require(adehabitatHR)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  # remove data with <5 reloc
  x_sum <- plyr::ddply(x, ~ Name, summarise, pos = length(Latitude))
  x %>% 
    filter(Name %!in% c(x_sum[x_sum$pos < min.pos,]$Name)) %>% 
    droplevels() -> x 
    
  if(nrow(x) > 0) {
    # generate spatial points
    
    sp <- SpatialPointsDataFrame(coords = x[,c(Lon, Lat)], 
                                 proj4string = proj, 
                                 data = data.frame(Name = x[,ID]))
    
    # to control where the function is
    message(unique(x$dataGroup))
    
    # calculate UDs
    kud <- kernelUD(sp, h = "href", same4all = F, extent = 1, grid = 500)
    
    # calculate core areas
    core <- try(getverticeshr(kud, percent = perc))
    
    if(class(core) != "try-error") {
      # turn core areas to sf
      core %>% 
        st_as_sf() %>% 
        st_set_crs(proj) -> core_sf
      
      # rasterize
      count_gear <- fasterize(core_sf, 
                              raster = raster::raster(core, nrows = 100*diff(range(core@bbox[1,])), 
                                                      ncols = 100*diff(range(core@bbox[2,]))),
                              fun = "count") 
      rm(list = c("sp", "kud", "core", "core_sf"))
      
      #plot
      plot(count_gear)
      plot(mask, add = T)
      return(count_gear)}
  } else {
    return(cat("not enough relocations to calculate kernel", "\n"))
  }
}
