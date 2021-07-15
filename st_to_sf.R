sf_to_df <- function(dat, projected = FALSE) {
  if(projected) {
    x2 <- x %>% 
      mutate(x = unlist(map(dat$geometry,1)),
             y = unlist(map(dat$geometry,2))) %>% 
      st_set_geometry(NULL)} else {
    
    x2 <- x %>% 
      mutate(Longitude = unlist(map(dat$geometry,1)),
             Latitude = unlist(map(dat$geometry,2))) %>% 
      st_set_geometry(NULL)}
  return(x2)
}
