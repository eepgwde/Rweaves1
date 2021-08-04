
#' Append a logical indicator column if an entry matches a regular expression.
#'
#' If to0 is NULL then to0 is "i." prepended to from0
#' 
#' @param tbl a dataframe
#' @param re a regular expression to match
#' @param from0 string: column name in tbl to match within
#' @param to0 string: the column name to use for the new logical column.
#' @return the processed data frame
#' @author weaves
#' @export
factor.matchre <- function(tbl, re, from0, to0=NULL) {
    if (is.null(to0)) {
        to0 <- sprintf("i.%s", from0)
    }
    tbl[, to0] <- grepl(re, as.character(tbl[, from0]), ignore.case=TRUE)
    tbl
}

#' Global substitution on a column.
#'
#' Ignores case.
#' 
#' @param tbl a dataframe
#' @param re a regular expression to match
#' @param from0 string: column name in tbl to match within
#' @param to0 string: the string to replace the re.
#' @return the processed data frame
#' @author weaves
#' @export
factor.blank <- function(tbl, re, from0, to0=NULL) {
    tbl[, to0] <- gsub(re, to0, tbl[, from0], ignore.case=TRUE)
    tbl
}

#' Trim white space in a column.
#'
#' @param tbl a dataframe
#' @param from0 string: column name in tbl to trim.
#' @return the processed data frame
#' @author weaves
#' @export
factor.blanks <- function(tbl, from0) {
    tbl[, to0] <- trimws(tbl[, from0])
    tbl
}

#' Teradata NA replacement.
#'
#' Teradata and Excel put '?' in place.
#' 
#' @param x a character vector
#' @param na0 the typical NA from elsewhere
#' @author weaves
#' @export
td.na <- function(x, na0="?") {
  x[x=='?'] <- NA
  return(x)
}

#' If a column is entirely NA, delete the column.
#'
#' Name of the column in a data frame.
#' 
#' @param df0 data frame
#' @param col0 is the string name of the column.
#' @export
empty.col <- function(df0, col0) {
  all0 <- all(is.na(df0[[ col0 ]]))
  if (!all0) { return(df0) }
  
  df0[[ col0 ]] <- NULL
  return(df0)
}

#' Interpret a character string as date
#' 
#' @export
dmy.dt <- function(x, fmt="%d/%m/%Y") {
  if (class(x) != "character") {
    x <- as.character(x)
  }
  as.Date(x, format=fmt)
}

#' Clean CURE data table
#' 
#' @export
cure.clean0 <- function(tbl) {
  
  x0 <- lapply(colnames(tbl), function(x) tbl[[ x ]] <<- td.na(as.character(tbl[[x]])))
  
  x0 <- lapply(colnames(tbl), function(x) tbl <<- empty.col(tbl, x))
  
  cols <- colnames(tbl)
  
  x1.idxs <- grepl(".+_DT$", cols)
  x0 <- lapply(cols[x1.idxs], function(x) tbl[[ x ]] <<- dmy.dt(tbl[[x]]))
  
  x1.names <- c("PARTY_ID", "AGRMNT_ID")
  
  x1.idxs <- grepl(".+_ID$", cols)
  x1.idxs <- setdiff(cols[x1.idxs], x1.names)
  x0 <- lapply(x1.idxs, function(x) tbl[[ x ]] <<- as(tbl[[x]], "numeric"))
  
  for (n0 in x1.names) {
    if (n0 %in% cols) {
      x0 <- lapply(n0, function(x) tbl[[ x ]] <<- as(tbl[[x]], "numeric"))
    }
  }
  
  x1.idxs <- grepl("^((.+_AM)|(START_TM))$", cols)
  x0 <- lapply(cols[x1.idxs], function(x) tbl[[ x ]] <<- as(tbl[[x]], "numeric"))
  
  x1.idxs <- ! grepl(".+_(DT|ID|TM|AM)$", cols)
  x0 <- lapply(cols[x1.idxs], function(x) tbl[[ x ]] <<- as.factor(tbl[[x]]))
  
  x1.idxs <- grepl("Date_Created", cols, ignore.case=TRUE)
  x0 <- lapply(cols[x1.idxs], function(x) tbl[[ x ]] <<- dmy.dt(tbl[[x]], fmt="%Y-%m-%d"))
  
  colnames(tbl) <- tolower(colnames(tbl))
  return(tbl)
  
}

#' Just known products
#' 
#' @export
cure.p <- function(left, right) {
  left$party_id == right$party_id
}

#' Just known products
#' 
#' @export
cure.p.a <- function(left, right) {
  cure.p(left, right) & left$agrmnt_id == right$agrmnt_id
}

#' Just known products
#' 
#' @export
cure.dt0 <- function(left, right, doffset=c(7, 7)) {
  (right$start_dt <= (left$date_created + doffset[1])) & 
    (right$start_dt >= (left$date_created - doffset[2]))
}

#' Find the values within a range.
#'
#' Use the features of log to avoid 0, NULL and NA.
#' 
#' @export
approx.value <- function(s, v, err0=0.05) {
  u0 <- log(v) + log(1+err0)
  l0 <- log(v) + log(1-err0)
  s0 <- log(s)
  which(s0 <= u0 & s0 >= l0)
}


#' Find a complaint within a date range.
#' 
#' For a party/agreement optionally within a value
#' @export
cure.dt <- function(x, cure=cure1, fn=cure.p.a, fn1=approx.value, doffset=c(7, 7)) { 
  x.cure2 <- cure[fn(x, cure),]
  if (nrow(x.cure2) <= 0) { return(NULL) }
  
  x.cure3 <- x.cure2[cure.dt0(x, x.cure2, doffset=doffset),]
  if (nrow(x.cure3) <= 0) { return(NULL) }
  
  if (!is.null(fn1)) {
    idxes <- fn1(x$value0, x.cure3$instrn_paymnt_am)
    if (length(idxes) <= 0) { return(NULL) }
    y <- x.cure3[idxes,]
  } else {
    y <- x.cure3
  }
  
  y <- merge(y, x[, c("source_complaint_id", "date_created", "value0")])
  return(y)
}

#' Given rules return a unique dataset.
#' 
#' @export
rules.itemsets <- function(rules) {
  itemsets <- unique(generatingItemsets(rules))
  itemsets.df <- as(itemsets, "data.frame")
  itemsets.df <- itemsets.df[with(itemsets.df, order(-support,items)),]
  names(itemsets.df)[1] <- "itemset"
  return(itemsets.df)
}

#' Names of columns that match a given class
#' 
#' @export
class.columns <- function(tbl, cls0="character") {
  return( sapply(colnames(tbl), function(x) { any(grepl(cls0, class(tbl[[ x ]]))) }, USE.NAMES=FALSE) )
}

#' Apply a str function to all columns.
#' 
#' @param tbl a data-frame.
#' @param FUN optional, a function that can process a list of strings, default is from \code{stringi}.
#' @export
fn.columns <- function(tbl, FUN=stringi::stri_trim_both, 
                       cls0="character") {
  idx <- class.columns(tbl, cls0)
  x0 <- lapply(colnames(tbl)[idx], function(x) { tbl[[ x ]] <<- FUN( tbl[[ x ]] ) } )
  return(tbl)
}

#' Returns an odds-against calculation
#' 
#' p0 is a probability. Works for vectors too.
#' @export
odds.against <- function(p0) {
  return( (1-p0)/p0 )
}

#' Converts probabilities to decimal odds.
#' 
#' p0 is a probability. Works for vectors too.
#' @export
decimal.p <- function(p0) {
  if (is.null(p0)) 
    return(c(99.01, 90, 75, 66.67, 50, 33, 25, 10, 0.99) / 100)
  
  return(1/p0)
}

#' Converts probabilities to Moneyline
#'
#' If you pass f0 as TRUE, then it return fractional odds with odds-on
#' as negative.
#'   
#' p0 is a probability. Works for vectors too.
#' @export
moneyline.p <- function(p0, f0=FALSE) {
  idxs <- p0 > 0.5
  o0 <- odds.against(p0)
  o0[idxs] <- - 1/o0[idxs]
  return(o0 * ifelse(f0, 1, 100))
}

