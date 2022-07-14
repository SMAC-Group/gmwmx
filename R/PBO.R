#' @importFrom fs file_move
get_file_with_cache <- function(url) {
  
  base_name = tail(strsplit(url, '/')[[1]],1)
  folder= tempdir()
  
  cache_file = paste(folder, base_name, sep = '/')
  
  if (!file.exists(cache_file)) {
    cmd = sprintf("wget %s -O %s.part", url, cache_file)
    ret = system(cmd, ignore.stdout = T, ignore.stderr = T)
    
    file_move(
      paste(cache_file, '.part', sep = ""),
      cache_file
    )
    
    if (ret != 0) {
      stop("Failed downloading data")
    }
  }
  
  cache_file
}

#'Load station data from PBO
#'
#' @param  station_name A \code{string} specifying the PBO station name.
#' @param column A \code{string} specifying the name of the column to extract.
#' @param time_range A \code{vector} of 2 specifying the time range of data to extract.
#' @param scale A \code{scalar} specifying an optional scaling parameter applied to the extracted data.
#' @return A \code{gnssts} object that contains the data associated with the specified PBO station.
#' @importFrom utils read.table
#' @export
#' @examples 
#' \donttest{
#' pbo_cola_data = PBO_get_station("COLA", column="dE")
#' str(pbo_cola_data)
#'}
PBO_get_station <- function(station_name, column, time_range = c(-Inf, Inf), scale = 1) {
  

  file_url = sprintf('https://data.unavco.org/archive/gnss/products/position/%s/%s.pbo.igs14.pos', station_name, station_name)
  tmpf = get_file_with_cache(file_url)
  
  # determine how many header lines
  con = file(tmpf, "r")
  skip = 1
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( startsWith(line, "End Field Description") ) {
      break
    }
    skip = skip + 1
  }
  
  close(con)
  
  data = read.table(tmpf, skip = skip, header = T)
  
  if (! (column %in% colnames(data)) ) {
    stop(sprintf("column '%s' not available", column))
  }
  
  i = data[,"JJJJJ.JJJJ"] > time_range[1]-1e-6 & data[,"JJJJJ.JJJJ"] < time_range[2]+1e-6
  
  if (sum(i) == 0) {
    stop("No data available within the required time range")
  }
  
  data = data[i, ]
  
  ts = create.gnssts(
    t = data[,"JJJJJ.JJJJ"], 
    y = data[, column] * scale,
    jumps = PBO_get_offsets(station_name)
  )
  
  ts
}

#'Extract offsets for a PBO station
#'
#' @param station_name A \code{string} specifying the PBO station name.
#' @return A \code{vector} specifying the offsets of a PBO station.
#' @examples 
#' \donttest{
#' pbo_cola_offsets = PBO_get_offsets(station_name = "COLA")
#' pbo_cola_offsets
#'}
#' @export
PBO_get_offsets <- function(station_name) {
  
  offset_file_url = 'https://data.unavco.org/archive/gnss/products/offset/cwu.kalts_nam08.off'
  
  tmpf = get_file_with_cache(offset_file_url)
  
  data = read.table(tmpf, skip = 38, header = F, comment.char = '!')
  
  colnames(data) <- c(".Site", "YYYY", "MM", "DD", "HR", "MN", "dN (mm)", "sN (mm)", "dE (mm)", "sE (mm)", "dU (mm)", "sU (mm)", "TYPE")
  
  idx = which(data[,'.Site'] == station_name)
  
  if (length(idx) > 0) {
    gaps = rep(NA, length(idx))
    for (j in 1:length(idx)) {
      gaps[j] = ymdhms_to_j(data[idx[j], "YYYY"], data[idx[j], "MM"], data[idx[j], "DD"], data[idx[j], "HR"], data[idx[j], "MN"], 0)
    }
  } else {
    gaps = c()
  }
  
  gaps
}


pboepoch_to_j <- function(epoch) {
  x = sprintf("%.0f", epoch)
  
  date = list(
    year = substr(x,1,4),
    month = substr(x,5,6),
    day = substr(x,7,8),
    hour = substr(x,9,10),
    minute = substr(x,11,12),
    second = substr(x,13,14)
  )
  
  date = lapply(date, as.numeric)
  do.call(ymdhms_to_j, date)
}
