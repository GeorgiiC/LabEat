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

pointInsideRect <-  function(p, r, tol)
{
  rTemp = r
  rTemp[1] = r[1]-tol
  rTemp[2] = r[2]-tol
  rTemp[3] = r[3]+tol*2
  rTemp[4] = r[4]+tol*2
  !((p[1] < rTemp[1]) || (p[2] < rTemp[2]) || (p[1] > (rTemp[1] + rTemp[3])) || (p[2] > (rTemp[2] + rTemp[4])))
}

etSummary <- function(etX, etY, time, areaL, areaR, plotPoints = FALSE, Onset, tol) #CG maybe a mistake .. correcting timeT to time; added Onset, tol
{
  isFirstIntersectionL = NA; #CG changed NULL to NA  
  isLastIntersectionL = NA; #CG changed NULL to NA 
  isIntersectionBeforeL = NULL; 
  viewCountL = 0
  viewCountR = 0
  viewTimeL = 0.0
  viewTimeR = 0.0
  startWatch = 0.0
  isWatchingPic = FALSE
  firstFixationIdxL = NA; #CG changed NULL to NA 
  firstFixationIdxR = NA; #CG changed NULL to NA 
  
  for(i in 1:length(etX)){
    
    if(time[i] < Onset)
      next
    
    p = c(etX[i], etY[i])
    isLeftIntersection = pointInsideRect(p, areaL, tol)
    isRightIntersection = pointInsideRect(p, areaR, tol)
    isIntersection = isLeftIntersection || isRightIntersection
    
    if(is.na(firstFixationIdxL) && isLeftIntersection) ## CG: added & changed i into time[i] & is.null 2 is.na
      firstFixationIdxL = time[i];
    
    if(is.na(firstFixationIdxR) && isRightIntersection )
      firstFixationIdxR = time[i];
    
    if(isIntersection && plotPoints)
      points(p[1],p[2],col=rgb(1,0,0,0.07), pch=19, cex=3)
    
    if(is.na(isFirstIntersectionL) && isIntersection)
      isFirstIntersectionL=isLeftIntersection
    
    if(is.null(isIntersectionBeforeL) && isIntersection) # start stopwatch
    {
      isWatchingPic = TRUE
      startWatch = time[i]
      
      if(isLeftIntersection)
        viewCountL = viewCountL+1
      else
        viewCountR = viewCountR+1
    } 
    else if((!is.null(isIntersectionBeforeL) && !isIntersection)||i==length(etX)) # stop watch
    {
      isWatchingPic = FALSE
      watchTime = time[i]-startWatch
      
      if(watchTime<80)
      {
        if(!is.null(isIntersectionBeforeL) && isIntersectionBeforeL)
          viewCountL = viewCountL-1
        else
          viewCountR = viewCountR-1
      } 
      else
      {
        if(!is.na(isLastIntersectionL)){ # CG PROBLEM: if isLastIntersectionL == NA && i==length(etX)) -> added && !is.na(isLastIntersectionL) 
          if (isLastIntersectionL)  # 
            viewTimeL = viewTimeL+watchTime                        
          else 
            viewTimeR = viewTimeR+watchTime
        }
      }
      
    }  
    
    if(isIntersection)
    {
      isIntersectionBeforeL = isLeftIntersection
      isLastIntersectionL = isLeftIntersection
    }
    else
      isIntersectionBeforeL = NULL
  }
  
  list(
    firstIntersectionIsLeft=isFirstIntersectionL, 
    lastIntersectionIsLeft=isLastIntersectionL, 
    viewCountL=viewCountL, 
    viewCountR=viewCountR, 
    viewTimeL=viewTimeL, 
    viewTimeR=viewTimeR,
    firstFixationIdxL=firstFixationIdxL,
    firstFixationIdxR=firstFixationIdxR)
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