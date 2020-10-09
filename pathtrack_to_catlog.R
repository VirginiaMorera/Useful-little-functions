pathtrack_to_catlog <- function(x) {
  # x <- pos_files[1]
  require(vroom)
  require(lubridate)
  require(tidyverse)
  gps_file <- vroom(x, 
                    skip = 5, col_names = F)
  gps_file %>% 
    mutate(Date = dmy(paste(X1, X2, X3)), 
           Time = paste(X4, X5, X6, sep = ":")) %>%
    select(Date, Time, Latitude = X9, Longitude = X10) -> gps_catlog
  
  return(gps_catlog)
}

