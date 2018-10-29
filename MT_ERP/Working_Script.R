## Working Script
setwd("Z:/Claudio/git_local")

# packages
# initialize packages
if (!("dplyr" %in% rownames(installed.packages()))) {install.packages("dplyr", dependencies = TRUE)}
library(dplyr)

if (!("pracma" %in% rownames(installed.packages()))) {install.packages("pracma", dependencies = TRUE)}
library(pracma)


#Import and general preproc
source("preproc_raw_eprime.R")
source("preproc_raw_mt.R")
source("artificial_tstamp.R")
source("setcoors2NA.R")
source("DecEtToolbox.R")
Datadir <- "Z:/DEC-EFD/Daten/Eprime/DEC/E_Prime_Export"
Data_preproc <- data.frame()

setwd(Datadir) # set working directory
files = list.files(pattern = "*.txt")

for (f in files) {
  print(f)
  Data <- preproc_raw_eprime(Datadir, f) #import and general preproc
  Data_norm <- preproc_raw_mt(Data)[[2]] # normalize mouse coordinates and timestamps
  #mtrap_Data_norm <- preproc_raw_mt(Data)[[1]] # mtrap class -> for later clustering
  
  # Calculate mouse-tracking measures (xflip, AUC)
  # Set coors before PictureOnset to NA and calculate mt-measures
  Data_mt <- setcoors2NA(Data_norm)
  m_x <- data.matrix(dplyr::select(Data_mt, num_range("xpos_", 1:504)))
  m_y <- data.matrix(dplyr::select(Data_mt, num_range("ypos_", 1:504)))
  
  for (trl in 1:dim(m_x)[1]){
    tempX <- m_x[trl,] %>% .[!is.na(.)]
    tempY <- m_y[trl,] %>% .[!is.na(.)]
    outputlist = getGoalDrivenAUC(tempX, tempY, plotIt = FALSE)
    
    # AUC
    Data_mt$AUC[trl] = outputlist[[1]]
    # Xflips
    Data_mt$Xflip[trl] = length(getXFlipIndices(tempX, kernelsize = 1)) #length for the count of Xflips.
  }
  
  # clear xpos, ypos, timestamps variables
  temp_indx <- !grepl("xpos_[[:digit:]]+", colnames(Data_mt)) 
  Data_mt <- Data_mt[,temp_indx]  
  temp_indx <- !grepl("timestamps_[[:digit:]]+", colnames(Data_mt)) 
  Data_mt <- Data_mt[,temp_indx]  
  temp_indx <- !grepl("ypos_[[:digit:]]+", colnames(Data_mt)) 
  Data_mt <- Data_mt[,temp_indx] 
  
  # construct DF
  if (isempty(Data_preproc)){
    Data_preproc <- Data_mt
  } else {
    Data_preproc    <- rbind(Data_preproc, Data_mt) 
  }
}