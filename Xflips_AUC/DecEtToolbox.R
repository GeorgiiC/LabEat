# DEC Toolbox for Mouse- and Gaze-Path Analysis
#  
getGoalDrivenAUC <- function(mouseX,
                             mouseY,
                             screenWidth = 1920,
                             screenHeight = 1080,
                             plotIt = FALSE)
{
  # setup
  require(pracma)
  scalar1 <- function(x) {x / sqrt(sum(x^2))}
  
  mouseXnorm = mouseX / screenWidth
  mouseYnorm = mouseY / screenHeight
  
  target = c(tail(mouseXnorm, 1), tail(mouseYnorm, 1))
  pathVectors = mapply(c, mouseXnorm, mouseYnorm, SIMPLIFY = FALSE)
  directions = list()
  targets = list()
  
  # calculate movement and target vectors.
  for (i in 2:length(pathVectors))
  {
    directions[[i - 1]] = pathVectors[[i]] - pathVectors[[i - 1]]
    targets[[i - 1]] =  target - pathVectors[[i - 1]]
  }
  angles = rep(0, length(directions))
  
  # canculate angles between movement and direct vectors in grad
  suppressWarnings(
    for (i in 1:length(directions))
    {
      angle = acos(sum(directions[[i]] * targets[[i]]) / (sqrt(sum(
        directions[[i]]
        * directions[[i]]
      )) * sqrt(sum(
        targets[[i]] * targets[[i]]
      ))))
      angles[i] = (angle * 180) / (pi)
    }
  )
  
  # before removing NAN replace them by 0 or extrapolation for outputlist[[4]]
  # don't use this data to calculate the AUC!
  # 0 means no change in angles
  angles_ts <- draftZeroTail(angles) # replaces NaN at the end of the angles time series with 0 
  angles_ts <- draftZeroFront(angles_ts) # replaces NaN at the beginning of the angles time series with 0 
  angles_ts <- draftInterpolateAndExtrapolate(angles_ts) # for remaining NaN in the angle time series
  
  # remove na points (ie points without movement).
  angles = angles[!is.na(angles)]
  angles = angles / 180
  
  if (plotIt)
    plot(angles, type = "l", ylim = c(0,1))
  
  xSequence = seq(0, 1, 1 / (length(angles) - 1))
  
  #return AOC
  AUC <- trapz(xSequence, angles)
  return(list(AUC, angles, xSequence, angles_ts))
}



getXFlipIndices <-  function(mouseX, kernelsize = 1)
{
  stopifnot(length(mouseX) >= 2 * kernelsize, kernelsize >= 1)
  
  wasRising = mean(mouseX[(1 + kernelsize):(2 * kernelsize)]) >=  mean(mouseX[1:kernelsize])
  detectedFlipIndices = c()
  
  x = 1 + kernelsize
  
  while (x <= (length(mouseX) - kernelsize)) {
    meanLower = mean(mouseX[(x - kernelsize):x])
    meanUpper = mean(mouseX[(x + 1):(x + kernelsize - 1)])
    
    if (meanUpper == meanLower)
    {
      x = x + 1
      next
    }
    
    isRising = meanUpper >=  meanLower
    
    if (isRising != wasRising)
    {
      detectedFlipIndices = c(detectedFlipIndices, x)
      x = x + kernelsize
      wasRising = isRising
    }
    x = x + 1
  }
  
  detectedFlipIndices
}



draftZeroTail <- function(angles)
{
  for (i in length(angles):1) {
    if(!is.na( angles[i]))
      break
    angles[i] = 0
  }
  angles
}

draftZeroFront <- function(angles)
{
  for (i in 1:length(angles)) {
    if(!is.na( angles[i]))
      break
    angles[i] = 0
  }
  angles
}

draftInterpolateAndExtrapolate <- function(angles)
{
  if (!("zoo" %in% rownames(installed.packages()))) {install.packages("zoo", dependencies = TRUE)}
  library(zoo)
  na.approx(angles, rule=2)
}