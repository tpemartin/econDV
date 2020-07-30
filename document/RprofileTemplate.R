.packages_init = c(
  "dplyr", "lubridate", "stringr", "ggplot2",
  "plotly", "purrr", "future", "drake", "renv",
  "svglite", "jsonlite", "readr", "rlang",
  "tidyr", "showtext", "rprojroot"
)
purrr::map(
  packages_init,
  ~require(.x, character.only = T)
)
showtext_auto(enable=TRUE) #啟用字體
theme_set(theme_classic())
