#' Predicate if detect all possible regex patterns
#'
#' @param series A character vector
#' @param regex A character of regex pattern with "pattern1|pattern2|...|patternK"
#'
#' @return A logical vector. TRUE if fits ALL patterns.
#' @export
#'
#' @examples none
str_detect_all= function(series, regex){
  nConds = stringr::str_count(regex,"\\|")+1
  stringr::str_count(
    series,regex)==nConds
}

#' Add meta data to a data frame
#'
#' @param df A data frame
#' @param meta A list of attributes
#'
#' @return
#' @export
#'
#' @examples addMeta(df, list("source"="WDI", "units"="millions"))
addMeta <- function(df, meta) {
  for (.x in seq_along(meta)) {
    attr(df, names(meta)[[.x]]) <- meta[[.x]]
  }
  return(df)
}

