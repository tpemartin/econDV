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
