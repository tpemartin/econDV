# plan_dataCleaning------------
plan_dataCleaning=drake::drake_plan(
# > plan begins -----------
# >> youBikeRightClass--------------
youBikeRightClass = { # deal with class
  df_newTaipeiYouBike %>% map(class)
  
  numericVs <- c("tot","sbi","lat","lng")
  
  df_newTaipeiYouBike %>% 
    mutate(
      across(
        all_of(numericVs),
        as.numeric
      )
    ) -> df_newTaipeiYouBike2
  
  factorVs <- c("sarea")
  df_newTaipeiYouBike2 %>%
    mutate(
      across(
        all_of(factorVs),
        as.factor
      )
    ) -> df_newTaipeiYouBike3
  
  df_newTaipeiYouBike3 %>%
    mutate(
      mday=ymd_hms(mday)
    )
},

# >> checkNA--------------
checkNA={
  totNA <- function(.x) sum(is.na(.x))
  smplSize <- function(.x) length(.x) - totNA(.x)
  youBikeRightClass %>%
    summarise(
      across(
        everything(),
        list(na=totNA, size=smplSize) # na, size會成為欄位名稱一部份
      )
    )
},

# >> df_inflationTW--------------
# data cleaning
df_inflationTW={
  df_cpiTW %>%
    filter(
      str_detect(TYPE, "年增率"),
      Item=="總指數(民國105年=100)"
    ) %>% #View() %>%
    mutate(
      date=ymd(
        glue::glue("{TIME_PERIOD}D01")),
      year=year(date)
    ) %>%
    group_by(
      year
    ) %>%
    mutate(
      n=n() # n() basically count nObs
    ) %>%
    filter(
      n==12
    ) %>%
    mutate(
      Item_VALUE=parse_number(Item_VALUE)
    ) %>%
    summarise( # !!! summarise 一定會ungroup
      inflation=mean(Item_VALUE)
    ) %>%
    mutate(
      iso2c="TW",
      country="Taiwan"
    ) -> df_inflationTW
  df_inflationTW
},

# >> df_worldInflation--------------

df_worldInflation={
  world_inflation %>%
    rename(
      inflation=FP.CPI.TOTL.ZG
    ) -> world_inflation2
  
  world_inflation2 %>%
    bind_rows(
      df_inflationTW
    ) -> df_worldInflation
  
  df_worldInflation
},

# >> world_inflationComplete--------------
world_inflationComplete = {
  world_inflation %>%
    rename(
      inflation=FP.CPI.TOTL.ZG
    ) -> world_inflation2
  
  bind_rows(
    data01$df_inflationTW,
    world_inflation2
  )
},

# >> world_inflationWithTaiwan--------------
world_inflationWithTaiwan = {
  world_inflation %>%
    rename(
      inflation=FP.CPI.TOTL.ZG
    ) -> world_inflation2
  
  world_inflation2 %>%
    left_join(
      ISOcountry %>%
        select(-en), # 不選en
      by = c("iso2c" = "code")
    ) -> world_inflationWithTaiwan
  world_inflationWithTaiwan
},

# >> world_inflationComplete_wide--------------
world_inflationComplete_wide = {
  world_inflationComplete %>%
    pivot_wider(
      id_cols = c("year", "country", "inflation"),
      names_from = "country",
      values_from = "inflation"
    ) ->
  world_inflationComplete_wide
  world_inflationComplete_wide
}

# > plan ends ------------
)

# make plan -----------------
mk_plan_dataCleaning %<-% {
  library(googlesheets4)
  library(econDV)
  # >> startSalary--------------
  startSalary %=% read_sheet(
    "https://docs.google.com/spreadsheets/d/1PPWeFGqedVsgZmV81MeA0oJDCBkj5upkHltlWap7xUY/edit#gid=1835013852", 
    skip=3
  )
  #WDI::WDIsearch(string="Inflation") -> inflation_search
  #inflation_search

  #target_series <- inflation_search[1,]
  world_inflation %=% WDI::WDI(
    indicator = "FP.CPI.TOTL.ZG"# target_series[["indicator"]]
  )
  
  df_cpiTW %=% {
    con=url("https://www.dropbox.com/s/8585yr3t0bbcb5b/df_cpiTW.rda?dl=1")
    load(file=con)
    df_cpiTW
  }
  
  df_newTaipeiYouBike %=% jsonlite::fromJSON(
    "https://data.ntpc.gov.tw/api/datasets/71CD1490-A2DF-4198-BEF1-318479775E8A/json") # data.frame class
    
  drake::make(plan_dataCleaning)
}

