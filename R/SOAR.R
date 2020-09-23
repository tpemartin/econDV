#' Enchanced `=` to store object in R.cache
#'
#' @param obj A symbol or name character represents the object to be created.
#' @param expr An expression command that creates the value for the object.
#'
#' @return Null.
#' @export
#'
#' @examples none
`%=%` <- function(obj, expr){
  obj=rlang::ensym(obj)
  expr=rlang::enexpr(expr)
  if(!exists(rlang::as_name(obj))){
    todo <- rlang::expr(
      {
        !!obj <- !!expr
        SOAR::Store(!!obj)
      }
    )
    rlang::eval_tidy(
      todo
    )
  } else {
    todo1 <- rlang::expr(
      {
        SOAR::Store(!!obj)
      }
    )
    rlang::eval_tidy(
      todo1
    )
  }

}
