
#' Set up chinese font environment
#'
#' @param need2Knit A logical, default=T includes fig.showtext=T and fig.retina=1
#'
#' @return
#' @export
#'
#' @examples none
setup_chinese <- function(need2Knit=F){
  if(need2Knit){
    if(!require(knitr)) install.packages("knitr")
    knitr::opts_chunk$set(fig.showtext=TRUE, fig.retina = 1)
  }

  if(!require(showtext)) install.packages("showtext")
  if(!require(ggplot2)) install.packages("ggplot2")

  font_add(
    "Noto Sans TC",
    regular="NotoSansTC-Regular.otf",
    bold="NotoSansTC-Bold.otf")
  showtext_auto()
  theme_set(
    theme(
      text=
        element_text(
          family = "Noto Sans TC")
    )
  )
}


