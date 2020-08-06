#' Read filelines to clipboard
#'
#' @param filepath A character. File to readLines from
#'
#' @return content in clipboard for pasting
#' @export
#'
#' @examples none
read2clip <- function(filepath){
  readLines(filepath)
  clipr::write_clip(eadLines(filepath))
}
