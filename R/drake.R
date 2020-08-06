

#' Create source_plan function source file under planPath
#'
#' @return A function that source an R script under planPath
#' @export
#'
#' @examples # # NOT RUN. produce error
#' # create_source_plan()
#'
#' planPath ="/Users/martin/Github/course-HS-society-and-citizen/plans"
#' source_plan <- create_source_plan()
#'
create_source_plan <- function(){
    stopifnot(
      "Missing planPath. Please created the definition object planPath."=
        exists("planPath", envir = globalenv())
    )
  source_functional(planPath)
}

#' List makes (future) in your global environment
#'
#' @return character string of promises of make/mk_plan...
#' @export
#'
#' @examples none
list_makes <- function(){
  ls(envir = globalenv()) %>%
    str_subset("^(mk|make)")
}
#' List available plans in your global environment
#'
#' @return A character string of drake plans
#' @export
#'
#' @examples none
list_plans <- function(){
  ls(envir = globalenv()) %>%
    str_subset("^plan")
}

#' List all files under plans folder
#'
#' @return A character string of all files
#' @export
#'
#' @examples none
list_plan_files <- function(){
  rprojroot::is_rstudio_project-> pj
  pj$make_fix_file() -> myroot
  planFolder=file.path(myroot(),"plans")
  stopifnot(
    "Folder plans does not exist"=
      dir.exists(
        planFolder
      )
  )
  list.files(
    path = planFolder
  )
}

#' Send a drake plan wrapper to clipboard to paste
#'
#' @return
#' @export
#'
#' @examples none
clip_planWrapper <- function(){
  clipr::write_clip(econDV::drake_plan_wrapper)
}


# helpers -----------------------------------------------------------------

source_functional <- function(path){
  function(filename){
    source(
      file.path(path,filename)
    )
  }
}
