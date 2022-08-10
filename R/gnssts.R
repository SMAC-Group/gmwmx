#' Create a gnssts object
#' 
#' @param t A \code{vector} specifying the time of each observation of the time series.
#' @param y A \code{vector} specifying the values of each observation of the time series.
#' @param jumps A \code{vector} specifying the time values for which there is a jump.
#' @param sampling_period An \code{integer} specifying the sampling period.
#' @return A \code{gnssts} object.
#' @importFrom utils tail
#' @export
#' 
#' @examples 
#' phase <- 0.45
#' amplitude <- 2.5
#' sigma2_wn <- 15
#' bias <- 0
#' trend <- 5 / 365.25
#' cosU <- amplitude * cos(phase)
#' sinU <- amplitude * sin(phase)
#' year <- 5
#' n <- year * 365
#' jump_vec <- c(200, 300, 500)
#' jump_height <- c(10, 15, 20)
#' nbr_sin <- 1 
#' A <- create_A_matrix(1:n, jump_vec, n_seasonal = nbr_sin)
#' x_0 <- c(bias, trend, cosU, sinU, jump_height)
#' eps <- rnorm(n = n, sd = sqrt(sigma2_wn))
#' yy <- A %*% x_0 + eps
#' gnssts_obj <- create.gnssts(t = 1:length(yy), y = yy, jumps = jump_vec)
#' str(gnssts_obj)
#' 
create.gnssts = function(t, y, jumps = NULL, sampling_period = 1) {
  
  # TODO: add further consistency checks
  
  if (length(t) != length(y)) {
    stop("t and y must have the same lenght")
  }
  
  x = list()
  
  x$t = as.numeric(t)
  x$y = as.numeric(y)
  
  valid_jumps = which(jumps>t[1] & jumps < tail(t,1))
  
  if (any(valid_jumps == FALSE)) {
    warning("some jumps are outside the provided time range, ignored")
  }
  
  x$jumps = jumps[valid_jumps]
  x$sampling_period = sampling_period
  
  # if no jumps, specify jumps to NULL
  if(length(x$jumps) == 0){
    x$jumps = NULL
  }
  
  # set class
  class(x) <- "gnssts"
  
  invisible(x)
}

#' Write a gnssts object
#' 
#' @param x A \code{R} object to save as a \code{gnssts} object.
#' @param filename A \code{string} specifying the name of the file to write.
#' @param format A \code{string} specifying the format of the file to write.
#' @return No return value. Write a \code{gnssts} object in a .mom file by default. 
#' @importFrom utils write.table
#' @export
#' @examples 
#' phase <- 0.45
#' amplitude <- 2.5
#' sigma2_wn <- 15
#' bias <- 0
#' trend <- 5 / 365.25
#' cosU <- amplitude * cos(phase)
#' sinU <- amplitude * sin(phase)
#' year <- 5
#' n <- year * 365
#' jump_vec <- c(200, 300, 500)
#' jump_height <- c(10, 15, 20)
#' nbr_sin <- 1
#' A <- create_A_matrix(1:n, jump_vec, n_seasonal = nbr_sin)
#' x_0 <- c(bias, trend, cosU, sinU, jump_height)
#' eps <- rnorm(n = n, sd = sqrt(sigma2_wn))
#' yy <- A %*% x_0 + eps
#' gnssts_obj <- create.gnssts(t = 1:length(yy), y = yy, jumps = jump_vec)
#' str(gnssts_obj)
#' \dontrun{
#' write.gnssts(x = gnssts_obj, filename = "test.mom")
#' }
write.gnssts = function(x, filename, format = "mom") {
  
  if (! ("gnssts" %in% class(x)) ) {
    stop("x must be a gnssts object")
  }
  
  if (format == "mom") {
    
    if (substr(filename, nchar(filename)-3, nchar(filename)) != ".mom") {
      stop("filename must end with '.mom'")
    }
    
    headers = c(
      sprintf("# sampling period %.6f", x$sampling_period)
    ) 
    if (!is.null(x$jumps[1])) {
      for (i in seq_along(x$jumps)) {
        headers[i+1] = sprintf("# offset %.6f", x$jumps[i])
      }
    }
    
    con = file(filename, "w")
    writeLines(headers, con)
    
    write.table(
      cbind(
        x$t,
        x$y
      ), 
      file = con, row.names = FALSE, col.names = FALSE, append = TRUE
    )
    
    close(con)
    
  } else {
    stop(sprintf("format '%s' not supported", format))
  }
}


#' Read a gnssts object
#' 
#' @param filename A \code{string} specifying the name of the file to read.
#' @param format A \code{string} specifying the format of the file to read.
#' @return Return a \code{gnssts} object.
#' @importFrom utils read.table
#' @importFrom stringi stri_match_first
#' @export
#' @examples 
#' phase <- 0.45
#' amplitude <- 2.5
#' sigma2_wn <- 15
#' bias <- 0
#' trend <- 5 / 365.25
#' cosU <- amplitude * cos(phase)
#' sinU <- amplitude * sin(phase)
#' year <- 5
#' n <- year * 365
#' jump_vec <- c(200, 300, 500)
#' jump_height <- c(10, 15, 20)
#' nbr_sin <- 1
#' A <- create_A_matrix(1:n, jump_vec, n_seasonal = nbr_sin)
#' x_0 <- c(bias, trend, cosU, sinU, jump_height)
#' eps <- rnorm(n = n, sd = sqrt(sigma2_wn))
#' yy <- A %*% x_0 + eps
#' gnssts_obj <- create.gnssts(t = 1:length(yy), y = yy, jumps = jump_vec)
#' str(gnssts_obj)
#' \dontrun{
#' write.gnssts(x = gnssts_obj, filename = "test.mom")
#' gnssts_obj <-read.gnssts(filename = "test.mom", format = "mom")
#' }
read.gnssts = function(filename, format = "mom") {
  if (format == "mom") {
    raw = read.table(file = filename, header = FALSE, comment.char = "#")
    
    x = list()
    x$t = raw[,1]
    x$y = raw[,2]
    x$sampling_period = 1 # default one, overwritten if found in mom header
    x$jumps = c()
    
    # read comments at the beginning of the file
    
    con = file(filename, "r")
    while (TRUE) {
      l = readLines(con, n=1)
      
      if (grepl("#", l)) {
        
        ret = stri_match_first(l, regex = "\\s*#\\s*sampling period\\s+(.*)")
        if (!is.na(ret[1,1])) {
          sampling_period = as.numeric(ret[1,2])
          
          if (!is.na(sampling_period)) {
            x$sampling_period = sampling_period
          } else {
            stop(sprintf("malformed .mom\n  cannot understand line '%s'", l))
          }
        }
        
        ret = stri_match_first(l, regex = "\\s*#\\s*offset\\s+(.*)")
        if (!is.na(ret[1,1])) {
          offset = as.numeric(ret[1,2])
          
          if (!is.na(offset)) {
            x$jumps[length(x$jumps)+1] = offset
          } else {
            stop(sprintf("malformed .mom\n  cannot understand line '%s'", l))
          }
        }
      } else {
        break
      }
    }
    
    close(con)
    
    class(x) <- "gnssts"
    
    invisible(x)
  } else {
    stop(sprintf("format '%s' not supported", format))
  }
}