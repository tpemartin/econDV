#' Create a theme for time series that fit the format of the Economist
#'
#' @return
#' @export
#'
#' @examples none
theme_timeSeries <-function(){
  ggplot2::theme(
    axis.title=ggplot2::element_blank(),
    axis.line.y=ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      color="#EEEEEE"
    ),
    legend.position = "none"
  )
}
