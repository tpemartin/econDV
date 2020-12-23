#' Get discrepancy extraction indices from all.equal comparison between two ggplot objects
#'
#' @param object1 A ggplot object.
#' @param object2 A ggplot object.
#'
#' @return A list of indices
#' @export
#'
#' @examples none.
get_discrepancyExtractionIndex <- function(object1, object2) {
  assertthat::assert_that(is(object1, "ggplot") && is(object2, "ggplot"),
                          msg = "Input arguments should be a ggplot class object."
  )
  all.equal(
    object1, object2
  ) -> inequalMsg

  str_extract_all(inequalMsg, "(?<=Component )([^:]+)") %>%
    map(
      ~ str_remove_all(.x, "[“”]")
    ) -> list_discrepancy
  for (i in seq_along(list_discrepancy)) {
    map(
      as.list(list_discrepancy[[i]]),
      ~ {
        ifelse(str_detect(.x, "^[:digit:]+$"),
               as.integer(.x), .x
        )
      }
    ) -> list_discrepancy[[i]]
  }
  return(list_discrepancy)
}
#' Get all discrepancy extractions expression in text
#'
#' @param list_discrepancy A list of return value from get_discrepancyExtractionIndex
#' @param referenceObject A ggplot object
#'
#' @return A character vector of discrepancy component value extraction expression in text
#' @export
#'
#' @examples none.
get_allDiscrepancyExpressionsInText <- function(list_discrepancy, referenceObject){
  objectQuo <- rlang::enquo(referenceObject)
  map_chr(
    list_discrepancy,
    ~get_discrepancyExpression(.x, objectQuo)
  ) -> chr_discrepancy
  unique(chr_discrepancy)
}
#' Get all discrepancy extraction expressions
#'
#' @param list_discrepancy A list of return value from get_discrepancyExtractionIndex
#' @param referenceObject A ggplot object
#'
#' @return A list of discrepancy component value extraction expressions
#' @export
#'
#' @examples none.
get_allDiscrepancyExpressions <- function(list_discrepancy, referenceObject){
  get_allDiscrepancyExpressionsInText(list_discrepancy, referenceObject) -> chr_discrepancy
  map(
    chr_discrepancy,
    ~parse(text=.x)
  )
}
#' Show discrepancy extraction expression on screen
#'
#' @param discrepancyExpression A list of return value from get_allDiscrepancyExpressions/inText
#' @param needEval logical, default=T, also show component values on screen
#'
#' @return
#' @export
#'
#' @examples none
show_values <- function(discrepancyExpression, needEval=T){
  purrr::walk(
    discrepancyExpression,
    show_value4oneExpression,
    needEval
  )
}

# helpers -----------------------------------------------------------------

show_value4oneExpression <- function(oneExpression, needEval=T){
  discrepancyExpression <-
    ifelse(
      is.expression(oneExpression),
      as.character(oneExpression), oneExpression
    )
  cat(discrepancyExpression,"\n")
  if(needEval) eval(parse(text=discrepancyExpression))
}
get_discrepancyExpression <- function(discrepancy_index, objectQuo){
  referenceObject <- eval(objectQuo)
  chain_index <- character()
  for(.x in seq_along(discrepancy_index)){
    # fix index
    .ix <- discrepancy_index[[.x]]
    .ix <-
      ifelse(
        str_detect(.ix, "^[:digit:]+$"),
        as.integer(.ix),
        .ix
      )
    if(
      testit::has_error(referenceObject[[.ix]], silent=T)
    ) sort(names(referenceObject))[[as.integer(.ix)]] -> .ix

    index_glue <-
      ifelse(
        is.integer(.ix),
        glue::glue("[[{.ix}]]"),
        glue::glue("[[\"{.ix}\"]]")
      )
    referenceObject <- referenceObject[[.ix]]
    chain_index <- paste0(chain_index, index_glue)
  }
  paste0(objectQuo, chain_index)
}

