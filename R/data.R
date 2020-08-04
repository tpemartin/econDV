#' Obtain dataset from dbnomics
#'
#' @param rdb0 A list. Returned value from rdbnomics::rdb(api_link=....)
#' @param queryString A character.
#'
#' @return data base from the rdb0 dataset that fits queryString query.
#' @export
#'
#' @examples none
get_rdb <- function(rdb0, queryString){
  df_0=rdbnomics::rdb(
    provider_code = rdb0$provider_code[[1]],
    dataset_code = rdb0$dataset_code[[1]],
    query=queryString)
  df_0
}


# helpers -----------------------------------------------------------------


gen_ISOcountry = function(){
  countryISO_en <-
    jsonlite::fromJSON("https://raw.githubusercontent.com/flyeven/country-code-zh-TW/master/country-code.json")

  countryISO_zhTW <-
    jsonlite::fromJSON("https://raw.githubusercontent.com/flyeven/country-code-zh-TW/master/country-code.zh-TW.json")

  library(dplyr)
  countryISO_en %>%
    rename(
      en=name
    ) %>%
    left_join(
      countryISO_zhTW %>%
        rename(
          zh=name
        ),
      by="code"
    ) -> ISOcountry

  ISOcountry
}
