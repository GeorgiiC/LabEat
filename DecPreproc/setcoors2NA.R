setcoors2NA <- function(df){
  m_t <- data.matrix(dplyr::select(df,  num_range("timestamps_", 1:100)))
  for (tr in 1:dim(df)[1]) {
    m_t_i <- na.omit(m_t[tr,])
    onset_idx <- as.numeric(which(abs(m_t_i - df$Onset[tr]) == min(abs(m_t_i - df$Onset[tr]),na.rm = TRUE)))[1] # subtracting Onset from each timepoint
    df$Onsett[tr] <- paste0("timestamps_", as.character(onset_idx))
    if(onset_idx !=1){
      df[tr, paste0("xpos_", 1:as.integer(onset_idx-1))] <- NA # Setting all X coordinates before that timepoint to NA
      df[tr, paste0("ypos_", 1:as.integer(onset_idx-1))] <- NA # Setting all y coordinates before that timepoint to NA
    }
  }
  return(df)
}


