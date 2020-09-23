#' Launch Boxy SVG and open filename inside
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples none
svgOpen <- function(filename){
  system(glue::glue("open -a 'Boxy SVG' {filename}"))
}

# svgfile <-
#   "/Users/martin/Github/course-HS-society-and-citizen/img/gg_GDPRank_base3.svg"

#' insert SVG as an object html
#'
#' @param svgfile
#'
#' @return
#' @export
#'
#' @examples none
insertSVG <- function(svgfile){
  htmltools::withTags(
    object(
      type="image/svg+xml",
      data=svgfile))
}
