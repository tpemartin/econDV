library(dplyr)
library(lubridate)
usethis::use_package("dplyr")
usethis::use_package("lubridate")
usethis::use_package("glue")
#' Create ggplot breaks/labels/limits that make sample period easily to understand
#'
#' @param dateSeries A date series from lubridate::ymd()
#' @param dY An integer. The span between two break years. Default=1
#' @param labeltype A character, either 'yyyy' or 'yy', specifies how you want
#' the year to be labels.
#'
#' @return A list of 3: breaks, labels and limits, for you to use in scale_...
#' in ggplot2
#' @export
#'
#' @examples none
get_break_labels= function(dateSeries, dY=1, labeltype="yy"){

  dateSeries %>%
    {c(min(.), max(.))} -> dateRange

  dateRange %>%
    year() -> yearRange
  yearRange %% 5 -> modulusOf5

  breakYearRange = dplyr::if_else(
    modulusOf5!=0,
    yearRange+(5-modulusOf5),
    yearRange
  )

  glue::glue(
    "{breakYearRange}-01-01"
  ) %>%
    lubridate::ymd() -> breakDateRange

  breakDateRange[[1]] =
    ifelse(
      breakDateRange[[1]]>=dateRange[[1]],
      breakDateRange[[1]],
      breakDateRange[[1]]+lubridate::years(5))

  breakDateRange[[2]] =
    ifelse(
      breakDateRange[[2]]<=dateRange[[2]],
      breakDateRange[[2]],
      breakDateRange[[2]]-lubridate::years(5))

  breaks=seq(
    from=year(breakDateRange[[1]]),
    to=year(breakDateRange[[2]]),
    by=dY
  ) %>%
    paste0(
      .,"-01-01"
    ) %>%
    ymd()

  limits = dateRange

  switch(
    labeltype,
    "yyyy"={
      c(
        as.character(dateRange[[1]]),
        as.character(year(breaks)),
        as.character(dateRange[[2]])
      ) %>%
        str_extract(
          '[0-9]{4}[-0-9]{0,3}'
        )

    },
    "yy"={

      as.character(year(breaks)) %>%
        str_extract("[0-9]{2}$") -> yearBreaks

      c(
        as.character(dateRange[[1]]),
        as.character(dateRange[[2]])
      ) %>%
        str_extract(
          '[0-9]{4}[-0-9]{0,3}'
        ) %>%
        {c(
          .[[1]], yearBreaks, .[[2]]
        )}
    },
    {
      stop("labeltype takes only 'yyyy' or 'yy' ")
    }
  ) -> .labels

  list(
    breaks=c(dateRange[[1]], breaks, dateRange[[2]]),
    labels=.labels,
    limits=dateRange
  )
}


#' 產生geom_rect所需xmin, xmax, ymin, ymax dataframe
#'
#' @param df_x A data frame with xmin and xmax columns
#'
#' @return A data frame with xmin and xmax inherited from df_x, and ymin=-Inf,
#' ymax=Inf. If xmin/xmax is not date class, will be converted accordingly.
#' @export
#'
#' @examples none
gen_df_geom_rect <- function(df_x){
  df_x$ymin = rep(-Inf, nrow(df_x))
  df_x$ymax = rep(Inf, nrow(df_x))
  df_x$xmax = lubridate::ymd(df_x$xmax)
  df_x$xmin = lubridate::ymd(df_x$xmin)
  df_x
}

#' Convert series into 1, 2, 3 rank numbers. 1 means the highest.
#'
#' @param series A numeric vector
#'
#' @return An integer vector, showing the rank, 1 means the larges/highest.
#' @export
#'
#' @examples none
get_rankNumber <- function(series){
  series %>%
    sort.int(
      decreasing=T,
      index.return = T) -> results
  series[results$ix] <- 1:length(series)
  as.integer(series)
}

