targetUrl <- "https://data.ntpc.gov.tw/api/datasets/71CD1490-A2DF-4198-BEF1-318479775E8A/json"
library(jsonlite)
library(googlesheets4)
plan01 <- drake::drake_plan(
  list_newTaipeiYouBike=fromJSON(
    targetUrl, simplifyDataFrame = F), # list class
  df_newTaipeiYouBike=fromJSON(
    targetUrl), # data.frame class
  startSalary={
    read_sheet(
      "https://docs.google.com/spreadsheets/d/1PPWeFGqedVsgZmV81MeA0oJDCBkj5upkHltlWap7xUY/edit#gid=1835013852",
      skip=3
    )
  },
  data01={
    list(
      list_newTaipeiYouBike=list_newTaipeiYouBike,
      df_newTaipeiYouBike=df_newTaipeiYouBike,
      startSalary=startSalary
    )
  }
)

library(drake); library(future)
mk_data01 %<-% {
  make(plan01)
}
