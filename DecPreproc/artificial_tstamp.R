artificial_tstamp <- function(n, values, step){
  mat_tstamp <- matrix(NA, nrow = length(values), ncol = n, byrow = TRUE)
  for (ivalue in 1:length(values)){
    mat_tstamp[ivalue, seq_len(values[ivalue])] <- seq(from = 0, to = (as.numeric(values[ivalue])-1)*step[ivalue], by = step[ivalue])
  }
  return(mat_tstamp)
}