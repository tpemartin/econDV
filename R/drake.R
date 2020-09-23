###
#' Purl active Rmd into a drake plan R script, named "plan_{activeRmdName}.R", then make the plan.
#'
#' @return
#' @export
#'
#' @examples purlActiveRmd_thenPlanMake()
purlActiveRmd_thenPlanMake <- function(){
  rstudioapi::getSourceEditorContext() -> activeSource
  activeSource$path -> activeRmd
  # normalizePath(activeRmd) -> activeRmd
  # stringr::str_remove(activeRmd, rootPath) ->
  #   html2open
  webDirRoot <- dirname(activeRmd)
  activeRmdBase <- basename(activeRmd)
  drakePlanname <-
    paste0("plan_",
           stringr::str_remove(activeRmdBase,"\\.[rR][mM][dD]$"))
  purl_drakePlan(activeRmd, drakePlanname)
  drakefilename <-
    file.path(
      webDirRoot,paste0(drakePlanname,".R")
    )
  source(drakefilename)
  makeName <-
    paste0(
      "mk_",drakePlanname
    )
  do.call(makeName, list())

}

#' Purl Rmd to a drake plan R script
#'
#' @description All R chunks with chunk names without drake=F will be purled to
#' a Drake plan R script.
#'
#' @param filename A character defines the file path name of Rmd
#' @param plan_name A character defines the R script name to be plan_name.R
#'
#' @return invisible. A character of all R script lines
#' @export
#'
#' @examples none
purl_drakePlan <- function(filename, plan_name){
  readLines(filename) -> Rmdlines

  frontmatterParams={
    knitr::knit_params(Rmdlines) -> paramsList
    if(length(paramsList)!=0){
      paramsList %>%
        purrr::map_chr(
          ~{
            glue::glue(c('{.x$name}="{.x$value}"'))
          }
        ) -> paramsString
      paramsString %>%
        paste0(
          collapse = ","
        ) %>%
        paste0(
          "params <- list(",
          .,
          ")"
        ) -> paramsSetupString
    } else {
      paramsSetupString="# no params in the frontmatter"
    }

    paramsSetupString
  }

  # find drake information
  {
    Rmdlines %>%
      stringr::str_which("(?<=```\\{r )[[:alnum:]_]+") -> whichHasRSetting
    Rmdlines[whichHasRSetting] %>%
      stringr::str_remove_all("\\s") %>%
      stringr::str_detect("drake=F") -> pickDrakeF
    whichHasRSetting[!pickDrakeF] -> whichHasDrakeObjects
    Rmdlines[whichHasDrakeObjects] %>%
      stringr::str_extract("(?<=```\\{r )[[:alnum:]_]+") -> drakeObjects
  }

  # define drake block begins and ends
  {
    whichDrakeLineEnds <- vector("integer", length(whichHasDrakeObjects))
    for(.x in seq_along(whichHasDrakeObjects)){
      begin <- whichHasDrakeObjects[[.x]]+1
      end <- ifelse(.x!=length(whichHasDrakeObjects),
                    whichHasDrakeObjects[[.x+1]]-1,
                    length(Rmdlines))
      whichSeq <- begin:end
      Rmdlines[whichSeq] %>% stringr::str_which("^```") %>%
        whichSeq[.] %>%
        min() -> whichDrakeLineEnds[[.x]]
    }

    tidyr::tibble(
      object=drakeObjects,
      begin=whichHasDrakeObjects+1,
      end=whichDrakeLineEnds-1
    ) -> drakeLocations
  }



  # define drake body function
  nDrakeObjs <- nrow(drakeLocations)
  {
    # drakeLocations %>%
    #   slice(.x) -> oneSlice
    # Rmdlines %>%
    #   get_drakeBody(oneSlice)
    drakeBody <- c()
    makecondition <- c()
    for(.x in 1:nDrakeObjs){
      oneSlice <- drakeLocations[.x,]
      Rmdlines %>%
        get_drakeBody(oneSlice) -> oneSliceBody
      oneSliceBody[[1]] %>%
        stringr::str_replace("<-","=") -> oneSliceBody[[1]]
      if(oneSlice$object=="makecondition"){
        makecondition <- oneSliceBody
        next
      }
      oneSliceBody %>% length() -> lastWhich
      oneSliceBody[[lastWhich]] =
        ifelse(
          .x!=nDrakeObjs,
          oneSliceBody[[lastWhich]] %>%
            paste0(.,","), #str_replace("\\}$","\\},"),
          oneSliceBody[[lastWhich]]
        )
      targetSlice <-
        c(
          glue::glue("# >> {oneSlice$object}--------------"),
          oneSliceBody,
          ""
        )
      drakeBody <- c(
        drakeBody,
        targetSlice
      )

    }

  }

  # produce drake R script
  {
    prefix <- c(
      "# {plan_name}------------",
      "{plan_name}=drake::drake_plan(",
      "# > plan begins -----------"
    )
    suffix <- c(
      "# > plan ends ------------",
      ")",
      ""
    )
    makePlan <- c(
      "# make plan -----------------",
      "mk_{plan_name} = function(){",
      frontmatterParams,
      "",
      makecondition,
      "",
      # "mkEnv=rlang::current_env()",
      "library(drake)",
      "make({plan_name}, cache=cacheNew)",
      "}",
      "",
      "vis_plan <- function(){",
      "vis_drake_graph({plan_name}, cache = cacheNew)",
      "}",
      ""
    )


    # assemble
    drakeScripts <-
      c(
        prefix,
        drakeBody,
        suffix,
        makePlan
      )
  }


  plan_nameExtract=stringr::str_extract(plan_name,"(?<=/)[[:alnum:]_\\.]+$")
  plan_nameExtract=stringr::str_replace(plan_nameExtract,"\\.R","")
  plan_name0=ifelse(is.na(plan_nameExtract), plan_name, plan_nameExtract)
  plan_name0=stringr::str_replace(plan_name0,".R","")
  planfilepath=
    ifelse(
      is.na(plan_nameExtract),
      "",
      stringr::str_extract(plan_name,
                           glue::glue("[:graph:]+(?={plan_nameExtract})"))
    )
  drakeScripts %>%
    stringr::str_replace_all("\\{plan_name\\}", plan_name0) ->
    drakeScriptsFinal
  writeLines(
    drakeScriptsFinal,
    con=paste0(
      stringr::str_replace(plan_name,"\\.R",""),
      ".R")
  )

  invisible(drakeScriptsFinal)

}


#' Create source_plan function source file under planPath
#'
#' @return A function that source an R script under planPath
#' @export
#'
#' @examples # # NOT RUN. produce error
#' # create_source_plan()
#'
#' planPath ="/Users/martin/Github/course-HS-society-and-citizen/plans"
#' Create Rmd template that can generate drake plan scripts
#'
#' @param planname A character.
#' @param title A character. (if null, default=planname)
#' @param root A chracter. (if null, default=getwd())
#'
#' @return
#' @export
#'
#' @examples none
create_planRmd <- function(planname, title=NULL, root=NULL){

  # readLines(
  #   "/Users/martin/Github/econDV/document/planRmdTemplate.Rmd"
  # ) -> planRmdTemplate
  #
  # usethis::use_data(planRmdTemplate,internal=T, overwrite=T)

  library(dplyr)
  planRmdTemplate %>%
    stringr::str_replace_all(
      c("\\{title\\}"=ifelse(is.null(title),planname,title),
        "\\{planname\\}"=planname,
        "\\{filename\\}"=
          ifelse(is.null(root),
                 file.path(getwd(),
                           paste0(planname,".Rmd")),
                 file.path(root,
                           paste0(planname,".Rmd")
                 )))
    ) -> myRmdLines

  writeLines(
    myRmdLines,
    paste0(planname,".Rmd")
  )
}

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

get_drakeBody = function(Rmdlines, oneSlice){
  oneSlice %>%
    {Rmdlines[(.$begin[[1]]:.$end[[1]])]} -> scriptBlock
  scriptBlock %>%
    stringr::str_remove_all("\\s") %>%
    {which(.!="")} %>%
    max() -> whichTargetEnds
  targetBody <- scriptBlock[1:whichTargetEnds]
  targetBody[[whichTargetEnds]] %>%
    stringr::str_remove_all("\\s") -> targetBody[[whichTargetEnds]]
  targetBody
}
