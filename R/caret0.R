#' Given a list with the 'weaves:caret' structure, remove zero variables.
#'
#' 
#' 
#' @param w a caret workspace list
#' @param cls0 a regular expression of classes to match.
#' @param from0 string: column name in tbl to match within
#' @param to0 string: the column name to use for the new logical column.
#' @return only the processed data frame
#' @author weaves
#' @export
caret.zv <- function(w, cls0="(factor|numeric|logical|integer)") {
    df <- w[['df']]
    clss <- class.columns(df, cls0=cls0)

    df0 <- df[, colnames(df)[clss] ]

    w[['nzv']] <<- nearZeroVar(df0, saveMetrics = TRUE)

    w[['zv']] <<- rownames(w[['nzv']][w[['nzv']]$zeroVar,])
    if (is.null(w[['zv']])) { return(df) }
    
    clss <- setdiff(colnames(df), w[['zv']])
    df <- df[, clss] 
    df
}
