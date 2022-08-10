ymdhms_to_j <- function(year, month, day, hour, minute, second) {
  
  cmd = sprintf('date2mjd %d %d %d %d %d %.6f', year, month, day, hour, minute, second)
  
  ret = system(cmd, intern = TRUE )
  
  as.numeric(strsplit(tail(ret,1), ":")[[1]][2])
}

#' @importFrom stringi stri_match_first
find_one_output <- function(out, pattern) {
  ret = NA
  for (i in seq_along(out)) {
    ret = stri_match_first(out[i], regex = pattern)
    if (!is.na(ret[1,1])) {
      break
    }
  }
  
  ret
}
