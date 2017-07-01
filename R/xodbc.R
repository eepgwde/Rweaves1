#' Prepare to use Java for Excel loading.
#' 
#' Sets JAVA_HOME in the environment and loads \code{xlsx}.
#' 
#' @export
java.excel0 <- function() {
  session.32bit <- as.logical(grep("i386", sessionInfo()$platform))
  
  ## 32 bit only
  if (session.32bit) {
    java.home <- "C:/Program Files (x86)/Java/jre7"
    java.home <- "C:/Program Files (x86)/Java/jre1.8.0_73"
    stopifnot(file.exists(java.home))
    Sys.setenv(JAVA_HOME = java.home)
    require("xlsx")
  }
  
}

td.spool <- "select min(peakspool), avg(peakspool), max(peakspool)
, 100*max(currentspool)/max(coalesce(maxprofilespool, maxspool)) as PercentInUse
from dbc.DiskSpace
where DatabaseName = user"



#' Read an SQL script and convert to query calls.
#'
#' No multi-line comments allowed viz. /* */
#' Just --. Be cautious with ; it is used as a delimiter.
#' 
#' If the ofile token is found in the file, it is converted to ':filename.ext
#' on a line on it's own.
#' 
#' @param f0 a filename.
#' @param ofile token for the filename to use for output.
#' @export
read.sql <- function(f0, ofile='@file') {
  f1 <- readLines(f0)
  ## drop comments
  t.re <- paste('--[ ]*', ofile, sep='')
  fns <- grepl(t.re, f1)
  f2 <- gsub('--.*$', '', f1)
  if (any(fns)) {
    f3 <- gsub(t.re, '', f1[fns])
    f3 <- gsub('^[ ]*', ':', f3)
    f3 <- gsub('$', ';', f3)
    f2[fns] <- f3
  }
  f2 <- paste(f2, collapse=' ')
  f1 <- strsplit(f2, ';')
  f1 <- f1[[1]]  
  f1 <- sapply(f1, stringi::stri_trim, USE.NAMES=FALSE)
  f1 <- gsub('[\t\r\n]+', ' ', f1)
  f1 <- f1[ nchar(f1) > 0]
  return(f1)
}


#' Replace strings with the value from the key-value pairs in a dictionary
#' 
#' This can be after read.sql() to replace parameters with values.
#' 
#' @export 
transform0.sql <- function(txt0, dict0) {
  invisible(lapply(names(dict0), function(x) 
  { txt0 <<- gsub(x, dict0[[ x ]], txt0, fixed=TRUE) }))
  
  return(txt0) 
}

#' Submit a list of SQL queries to the channel.
#'
#' The list should be the output of read.sql().
#' This can write to a CSV file if embedded in the queries. They appear as
#' the strings :filename.ext in the queries.
#' 
#' @param ch1 a channel
#' @param qs the queries - one per element, no newlines or such.
#' @param verbose0 whether to log the queries.
#' @return list of values returned by sqlQuery() in errors FALSE mode.
#' @export
exec.sql <- function(ch1, qs, verbose0=NULL, nodo=FALSE, ofun=write.csv) {
  idx <- 1
  errs0 <- list()
  if (is.null(verbose0)) {
    verbose0 <- getOption("verbose")
  }
  fname <- NULL
  for (q0 in qs) {
    idx <<- idx + 1
    
    if (length(grep('^:', q0)) > 0) {
      fname <- gsub('^:', '', q0)
      next
    }
    
    q1 <- nodo
    if (!nodo) {
      q1 <- sqlQuery(ch1, q0, errors = !is.null(fname))
      if (!is.null(fname)) {
        ofun(q1, file=fname, row.names=FALSE, na="")
      }
    }
    if (verbose0) {
      print(paste("idx: ", idx, ": ", q1, ": ", q0, ": ", fname))
    }
    if (is.null(fname)) {
      errs0 <- append(errs0, list(x=q1))
    }
    fname <- NULL

    odbcClearError(ch1)
  }
  return(errs0)
}

#' Get the files embedded in a query.
#'
#' The list should be the output of read.sql().
#' 
#' @param qs the queries - one per element, no newlines or such.
#' @return list of filenames.
#' @export
files.sql <- function(qs, ...) {
  qs <- qs[grepl('^:', qs)]
  qs <- gsub('^:', '', qs)
  return(qs)
}

#' Given a list (of data frames or filenames) collate them into a single Excel workbook.
#' 
#' @note
#' read.csv() is used to load the file to a data-frame.
#' @param fls the filenames in a list.
#' @return the filename.
#' @export
collate.xl <- function(fls, filename="default.xlsx", overwrite=TRUE, ...) {
  
  if (file.exists(filename)) {
    file.remove(filename)
  }  
  append0 <- FALSE
  idx <- 1
  for (f0 in fls) {
    if (is.character(f0)) {
      f1 <- gsub('\\..*$', '', f0)
      t0 <- read.csv(f0)
    } else {
      t0 <- f0
      f1 <- sprintf("df%02d", idx)
    }
    write.xlsx(t0, filename, sheetName = f1, append=append0, 
               row.names=FALSE, showNA=FALSE)
    append0 <- TRUE
    idx <- idx + 1
  }
  return(filename)
}

#' Lines in a file.
#' 
#' @note
#' Very slow compared to wc(1). There are some shell scripts cleaner0.sh and sampler0.sh that can sample better.
#' @param fls the filenames in a list.
#' @return the filename.
#' @export
lines.txt <- function(filename) {
  testcon <- file(filename,open="r")
	readsizeof <- 20000
	nooflines <- 0
	( while((linesread <- length(readLines(testcon,readsizeof))) > 0 ) 
	nooflines <- nooflines+linesread )
	close(testcon)
	return(nooflines)
}

#' Generate a sample set of row numbers for a file.
#' 
#' @note
#' This can be used with sample.csv
#' @param nrows the number of rows in the whole file
#' @param filename if nrows is not given, calculate the number of rows in the named file.
#' @param size0 the number of rows in the sample
#' @param sampler the function to generate the samples.
#' @return a vector of integers greater than zero.
#' @export

sample.rows <- function(nrows=NULL, filename=NULL, size0=NULL, sampler=sample.int) {
	if (!is.null(filename) & is.null(nrows)) {
		nrows <<- lines.txt(filename)
	}
	if (is.null(size0)) {
		size0 <<- floor(nrows/5)
	}
	n0 <- sampler(nrows, size=size0)
	n0 <- n0[n0 > 0]
	return(n0)
}

#' Sample a CSV file into a data frame given a vector of row numbers.
#'
#' This can help with files that are too large to load at once.
#' 
#' @note
#' There are some shell scripts cleaner0.sh and sampler0.sh that can sample better.
#' @param filename the name of the file.
#' @param rows a vector of row numbers to take from a file.
#' @return a data frame.
#' @export
sample.csv <- function(filename, rows, ...) {
	h0 <- read.csv(filename, nrows=1, 
		header=TRUE, stringsAsFactors=FALSE)
		
	l0 <- lapply(rows, function(x) read.csv(filename, nrows=1, 
		skip = x, header=FALSE, stringsAsFactors=TRUE) )
	df <- do.call(rbind, l0)
	colnames(df) <- colnames(h0)
	return(df)
 }


#' Excel serial data conversion.
#'
#' 
#' 
#' @param dates a list of integer serial date number, usually around 42000
#' @param days, return a date-only
#' @return a list of POSIX date-time, or, if days, as date only.
#' @export
as.Date.excel <- function(dts, days=FALSE, ...) {
	dts1 = as.POSIXct(as.Date(dts, origin="1899-12-30"))
	if (days) {
		dts1 = as.Date(dts1)
	}
	return(dts1)
 }
