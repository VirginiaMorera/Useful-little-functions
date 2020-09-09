## This funtion will serch for GLS activity files (.deg for migrate .act for Biotrack*) and assign state wet/dry to each gps location of the x dataframe
## x is a dataframe with at least a column Ring identifying the bird. x must contain only 1 Ring, it can be applied to a multi-bird dataset using purrr:::map_df
## Folder is the full path of the folder containing the GLS files
## act_ext is the extension of the files containing the activity data
## * it is possible that line 24 needs ton be changed for Biotrack data, since in those devices the info on each rows corresponds to the period following that timestamp, and not previous. But so far I've only used it with Migrate devices, so there. 


assign_act <- function(x, folder, act_ext = "driftadj.deg") {
  Ring <- unique(x$Ring)
  cat(c(Ring, "\n"))
  ringfiles <- list.files(path = folder, pattern = Ring)
  ringfiles <- grep(act_ext, ringfiles, value = TRUE, invert = F)
  if(length(ringfiles) > 0) {
    
    act_data <- do.call(rbind,
                        lapply(ringfiles, function(i){
                          read.csv(paste(folder, i, sep = "/"), skip=19, header=TRUE, sep="", dec=".")}))
    act_data$Date_Time <- dmy_hms(paste(act_data$DD.MM.YYYY, act_data$HH.MM.SS, sep = " ")) # can't make this inside a mutate, it causes an error when running the function 
    act_value <- rep(NA, length = nrow(x))
    for (i in 1:nrow(x)) {
      # i = 1
      a <- x[i,]
      act_data %>% 
        filter(Date_Time >= a$Date_Time) %>% 
        slice(1L) %>% 
        dplyr::select(wet.dry) -> act_v
      act_value[i] <- act_v[1,1]} 
  } else {
    act_value <- rep("no_act", nrow(x))
  }
  
  x$act_value <- act_value
  return(x)
}
