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
#' @param is_var_lang_no logical TRUE if norwegian names (default)
#'
#' @return a data frame of transformed data
#' @export
#'
#' @examples

transformRegData <- function(dat, meta, is_var_lang_no = TRUE) {
  
  # check for duplicates
  if (any(duplicated(names(dat)))) {
    stop("Duplicate(s) found:\n",
         stringr::str_c(names(dat)[duplicated(names(dat))]),
         "Stopping",
         collapse = "\n")
  }
  
  # names language
  if (is_var_lang_no) {
    dd_kolid = "oqr_variabel_id_norsk"
  } else {
    dd_kolid = "oqr_variabel_id_engelsk"
  }
  
  # meta names
  d_full <- meta$variabel_id[match(names(dat), meta[[dd_kolid]])] %>%
    dplyr::coalesce(names(dat))
  
  # meta entry ids
  # kb_info = meta %>% dplyr::distinct(variabel_id, .keep_all = TRUE)
}