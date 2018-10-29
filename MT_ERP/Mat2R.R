Mat2R <- function(pn){
  # packages
  if (!("R.matlab" %in% rownames(installed.packages()))) {
    install.packages("R.matlab", dependencies = TRUE)
  }
  library(R.matlab)
  
  if (!("devtools" %in% rownames(installed.packages()))) {
    install.packages("devtools", dependencies = TRUE)
  }
  library(devtools)
  
  if (!("reach" %in% rownames(installed.packages()))) {
    install_github("schmidtchristoph/reach/reach")
  }
  library(reach)

  scriptName <- "ImportData.m"
  mypath <- paste(getwd(), "Matlab2R_ImportData/", sep = "/")
  if (!dir.exists(file.path(mypath))) dir.create(file.path(mypath), showWarnings = F)
  setwd(file.path(mypath))
  print(paste("Script and Data is saved under:", mypath, sep = " "))
  scriptCode <- paste0("pn = ", "'",pn, "'", ",", 
                "data = load(pn, '-mat')  
                save data -v7")
  writeLines(scriptCode, con = scriptName)
  
  runMatlabScript(scriptName)
  inpData <- R.matlab::readMat("data.mat")
  
  # reshape inpData into array & rename dimensions 
  # dimensions are [timepoint, trial, electrode label]
  indx_chan <- lapply(dimnames(inpData$data)[1], 
                      function(x) grepl("chan[0-9]+", x))
  chan_list <- inpData$data[unlist(indx_chan)]
  d1 <- dim(chan_list[[1]])[1]; d2 <- dim(chan_list[[1]])[2]; d3 <- length(chan_list)
  chan_names <- lapply(inpData$data[dimnames(inpData$data)[[1]] == "labels"], 
                       function(x) unlist(x))
  trial_names <- list(1:d2)
  time_names <- lapply(inpData$data[dimnames(inpData$data)[[1]] == "times"], 
                       function(x) unlist(x))
  chan_arr <- array(unlist(chan_list), 
                    dim = c(d1, d2, d3), 
                    dimnames = c(time_names, trial_names, chan_names))

  return(chan_arr)
}
