preproc_raw_eprime <- function(Datadir, fn){
  path2files <- paste0(Datadir, "/", fn)
  options(stringsAsFactors = FALSE) # strings are imported as characters (ignoring default as factors)
  
  # import
  Data <- read.table(
    file = path2files,
    header = TRUE,
    skip = 1,
    sep = '\t',
    blank.lines.skip = TRUE,
    dec = "."
  ) 
  
  containsTimestamps = "Time1" %in% colnames(Data)
  containsET = "TET_X1" %in% colnames(Data)
  
  # clearing unnecessary variables
  DataTempA <- dplyr::select(
    Data,
    Subject,
    SubTrial,
    Procedure.Trial.,
    Init_Time,
    PicRight_Presented,
    PicLeft_Presented,
    Target,
    PicL,
    PicnameL,
    PicR,
    PicnameR,
    Target_X,
    Target_Y,
    TPicOnset,
    TrackingSlide.RT,
    TrackingSlide.RTTime,
    TrackingSlide.OnsetTime,
    num_range("Mouse_X", 1:504),
    num_range("Mouse_Y", 1:504)
  )
  
  if(containsET){
    DataTempB <- dplyr::select(
      Data,
      SubTrial,
      Procedure.Trial.,
      num_range("Time", 1:504),
      num_range("TET_X", 1:504),
      num_range("TET_Y", 1:504)
    )
    Data = merge(DataTempA, DataTempB, by=c("SubTrial", "Procedure.Trial."))
    
  }else if (containsTimestamps)
  {
    DataTempB <- dplyr::select(
      Data,
      SubTrial,
      Procedure.Trial.,
      num_range("Time", 1:504)
    )
    
    Data = merge(DataTempA, DataTempB, by=c("SubTrial", "Procedure.Trial."))
    DataTempB = NULL
    
  }else
  {
    Data = DataTempA
  }
  DataTempA = NULL
  
  #Preproc
  # Factors are converted to strings first (error handling)
  i <- sapply (Data, is.factor)                                       # in i werden alle Variablen mit class "factor" gespeichert
  Data[i] <- lapply(Data[i], as.character)                            # umwandeln in strings
  
  # Subsetting only Trials which are during Testing
  Data <- subset(Data, Data$Procedure.Trial. == "TestingProc")        # subset ziehen
  Data <- subset(Data, Data$Target != "?")                            # subset ziehen
  Data$Procedure.Trial. <- NULL                                       # l?schen von Variable (nicht mehr gebraucht)
  Data$Init_Time <- NULL
  
  Data <- Data[order(Data$SubTrial), ]
  
  # Preprocessing ----
  # Calculating real ResponseTime (RT - Init)
  # Behelf weil Pic.OnsetTime fehlt!!!
  Data$RT <- Data$TrackingSlide.RTTime - Data$TPicOnset
  Data$Onset <- Data$TPicOnset - Data$TrackingSlide.OnsetTime # Stimulus Onset per trial in ms
  
  
  # Exclude values, which due to some rare bug, are too high 
  Data <- Data[!Data$RT > 4000,]
 
}