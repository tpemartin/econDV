#' Read filelines to clipboard
#'
#' @param filepath A character. File to readLines from
#'
#' @return content in clipboard for pasting
#' @export
#'
#' @examples none
read2clip <- function(filepath){
  readLines(filepath)
  clipr::write_clip(eadLines(filepath))
}

#' Create save functions with designated saving directory
#'
#' @param savepath A character, represents saving directory
#'
#' @return none
#' @export
#'
#' @examples save2img <- save2_functional("img");
save2_functional <- function(savepath) {
  function(Obj) {
    Obj <- rlang::ensym(Obj)
    Objname <- rlang::expr_deparse(Obj)
    savefilename <- paste0(
      Objname, ".rda"
    )

    # browser()
    stopifnot(
      "savepath does not exist" =
        (exists("savepath"))
    )
    savefilepath <- file.path(
      savepath, savefilename
    )
    rlang::expr(save(!!Obj, file=savefilepath)) -> todo
    rlang::eval_tidy(
      todo
    )
    #save(Objname, file = savefilepath)
    message("Saved as ", savefilepath)
  }
}
