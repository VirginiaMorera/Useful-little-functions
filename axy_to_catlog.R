axy_to_catlog <- function(x) {
  # i = 1
  require(vroom)
  require(reshape2)
  require(lubridate)
  
  x <- vroom(x, col_names = F)
  Date <- colsplit(x$X1, pattern = ",", names = c("Date", "Time"))[,1]
  Date2 <- dmy(Date)
  Date3 <- as.character(format(Date2, format = "%m/%d/%Y"))
  Time <- colsplit(x$X1, pattern = ",", names = c("Date", "Time"))[,2]
  catlog <- data.frame(Date = Date3, 
                       Time = Time, 
                       Latitude = x$X2,
                       Longitude = x$X3)
  
  return(catlog)
}
