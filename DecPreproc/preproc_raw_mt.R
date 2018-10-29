preproc_raw_mt <- function(df){
  # initalize package
  if (!("mousetrap" %in% rownames(installed.packages()))) {install.packages("mousetrap", dependencies = TRUE)}
  library(mousetrap)
  
  containsTimestamps = "Time1" %in% colnames(Data)
  containsET = "TET_X1" %in% colnames(Data)
  
  # Exclude Eye-Tracking Data
  if(containsET){
    temp_indx <- !grepl("TET_[XY][[:digit:]]+", colnames(Data)) 
    Data <- Data[,temp_indx]  
  }
  
  # Preproc and normalize mousetrajectories
  # N of mouse_coordinates
  tempv <- colnames(Data)[grepl("Mouse_[XY][[:digit:]]+", colnames(Data))] %>%
    sub("Mouse_[XY]", "", .)
  n_mtcoor <- max(as.numeric(tempv))
  
  if(containsTimestamps){
    #PROBLEM: timestamps are written for Eye-Tracking Data not MT Data (Weird Bug)
    # need to create artificial time stamps
    # Estimate the average timestemp based on mean(RT/sum(!is.na(coors)))
    tstamp <-  t(Data[,grepl("Mouse_[X][[:digit:]]+", colnames(Data))]) %>%
      apply(.,2, function(x) sum(!is.na(x))) 
    avg_tstamp <- Data$TrackingSlide.RT/tstamp
    #aavg_tstamp <- round(mean(avg_tstamp))
    
    # create artificial "Time_X" Columns
    mat_tstamp <- artificial_tstamp(n_mtcoor, tstamp, avg_tstamp)
    
    # name columns
    df_tstamp <- as.data.frame(mat_tstamp)
    colnames(df_tstamp) <- strcat("Time_", as.character(seq(from = 1, to = n_mtcoor, by = 1)))
    Data <- cbind(Data, df_tstamp)
    
    # clear ET timestamps
    temp_indx <- !grepl("Time[[:digit:]]+", colnames(Data)) 
    Data <- Data[,temp_indx]  
    
    # create mousetrap class
    mtrap_df <- mt_import_wide(Data, xpos_label = "Mouse_X", ypos_label = "Mouse_Y", 
                            timestamps_label = "Time_", mt_id_label = "SubTrial", 
                            pos_sep = "", pos_ids = 1:n_mtcoor)
  } else {
    
    # Estimate the average timestemp based on mean(RT/n_coor)
    tstamp <-  t(Data[,grepl("Mouse_[X][[:digit:]]+", colnames(Data))]) %>%
      apply(.,2, function(x) sum(!is.na(x))) 
    avg_tstamp <- Data$TrackingSlide.RT/tstamp
    #aavg_tstamp <- round(mean(avg_tstamp))
    
    # create artificial "Time_X" Columns
    mat_tstamp <- artificial_tstamp(n_mtcoor, tstamp, avg_tstamp)
    
    
    # name columns
    df_tstamp <- as.data.frame(mat_tstamp)
    colnames(df_tstamp) <- strcat("Time_", as.character(seq(from = 1, to = n_mtcoor, by = 1)))
    Data <- cbind(Data, df_tstamp)
    mtrap_df <- mt_import_wide(Data, xpos_label = "Mouse_X", ypos_label = "Mouse_Y", 
                               timestamps_label = "Time_", mt_id_label = "SubTrial", 
                            pos_sep = "", pos_ids = 1:n_mtcoor)
  }
  

  mtrap_df <- mt_align_start(mtrap_df, use = "trajectories", save_as = "trajectories", 
                          dimensions = c("xpos", "ypos"), start = c(0,0), verbose = FALSE)

  mtrap_df <- mt_remap_symmetric(mtrap_df, use = "trajectories", save_as = "trajectories",
                              dimensions = c("xpos", "ypos"), remap_xpos = "left", remap_ypos = "up")
  
  # Time-normalization does not work properly: TrackingSlide.RT does not match the last timestamp
  # (timestamp_101), thus, we do not normalize, x,y and timestamps.
  #
  # mtrap_df_t <- mt_time_normalize(mtrap_df, use = "trajectories", 
  #                                 save_as = "tn_trajectories",  nsteps = 101)
  # 
  # mtrap_df_sp <- mt_spatialize(mtrap_df_t, use = "tn_trajectories", dimensions = c("timestamps", "xpos", "ypos"), 
  #                           save_as = "sp_trajectories", n_points = 101)
  # 
  # #Okay i found no neat way of doing this, thus the timenormalized timestamps are 
  # #replacing the timestamps in mtrap_df_sp
  # mtrap_df_sp$sp_trajectories[,,1] <- mtrap_df_t$tn_trajectories[,,1]
  
  mt_norm <- mt_export_wide(mtrap_df, use = "trajectories")
  mt_norm <- mt_norm[order(as.numeric(mt_norm$mt_id)),]
  
  Data_norm <- cbind(Data, mt_norm)
  
  # Prune df
  temp_indx <- !grepl("Mouse_[XY][[:digit:]]+", colnames(Data_norm)) 
  Data_norm <- Data_norm[,temp_indx]  
  temp_indx <- !grepl("Time_[[:digit:]]+", colnames(Data_norm)) 
  Data_norm <- Data_norm[,temp_indx]  
  temp_indx <- !grepl("mt_id", colnames(Data_norm)) 
  Data_norm <- Data_norm[,temp_indx] 
  
  return(list(mtrap_df,Data_norm))
}
