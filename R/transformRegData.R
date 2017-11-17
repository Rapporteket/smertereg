#' Transform data to be used in reports for smerte
#' 
#' Perform any transformation of original registry data by applying some
#' relevant metadata describing the variables in the original data set.
#' So far _meta_ is provided by the built-in kodebok, and operations are
#' replicas of those found in _les_dd_oqr_
#' Use of this function is likely to be highly specific...
#'
#' @param dat a data frame containing original data
#' @param meta a data frame describing the each column in the original data
#'
#' @return a data frame of transformed data
#' @export
#'
#' @examples

transformRegData <- function(dat, meta) {
  # what we need to do
  
  # 
}