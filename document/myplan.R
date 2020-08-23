# myplan------------
myplan=drake::drake_plan(
# > plan begins -----------
# >> downloaddata--------------
downloaddata = {
  library(readr)
  taiwanStatistics <- read_csv(
    file_in("https://www.dropbox.com/s/d5v23cab2b74y5i/Taiwan%E5%9C%8B%E6%B0%91%E6%89%80%E5%BE%97%E7%B5%B1%E8%A8%88%E5%B8%B8%E7%94%A8%E8%B3%87%E6%96%99.csv?dl=1"),
locale = locale(encoding = "BIG5"), skip = 3)
  taiwanStatistics
},

# >> dataReady--------------
dataReady = {
  downloaddata %>%
  rename(
    "年"="X1"
  ) %>%
  mutate(
    across(
      everything(),
      as.numeric
    )
  )
},

# >> gg_taiwanEconomicGrowth--------------
gg_taiwanEconomicGrowth = {
  dataReady %>%
    ggplot()+
    geom_line(
      aes(
        x=年, y=`經濟成長(%)`
      )
    )
},

# >> save_gg_taiwanEconomicGrowth--------------
save_gg_taiwanEconomicGrowth = {
  ggsave(
    file_out("taiwanEconomicsGrowth.svg"),
    gg_taiwanEconomicGrowth,
    width=8,
    height=5
  )
  
}

# > plan ends ------------
)

# make plan -----------------
mk_myplan = function(){
params <- list(planpath="plan",planname="myplan")

library(readr)
library(dplyr)
library(ggplot2)

  drake::make(myplan)
}

