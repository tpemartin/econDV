#' Predicate if detect all possible regex patterns
#'
#' @param series A character vector
#' @param regex A character of regex pattern with "pattern1|pattern2|...|patternK"
#'
#' @return A logical vector. TRUE if fits ALL patterns.
#' @export
#'
#' @examples
str_detect_all= function(series, regex){
  nConds = stringr::str_count(regex,"\\|")+1
  stringr::str_count(
    series,regex)==nConds
}
