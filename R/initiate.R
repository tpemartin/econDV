# packages_init = c(
#  "dplyr", "lubridate", "stringr", "ggplot2",
#  "plotly", "purrr", "future", "drake", "renv",
#  "svglite", "jsonlite", "readr", "rlang",
#  "tidyr", "showtext", "rprojroot"
# )

installMissingRequires =function(packages_init=econDV::packages_init)
 {
    purrr::map_lgl(packages_init,
                   require, character.only = T) -> lgl_installed
    pkgsMissing <- packages_init[!lgl_installed]

    if(length(pkgsMissing)==0){
      ans=rstudioapi::showPrompt("是否安裝以下套件（Y/N, default=Y）",
                                 paste0(pkgsMissing, collaps="  "),
                                 default="Y")
      if(ans %in% c("y","Y")){
        install.packages(
          pkgsMissing
        )
      }
    }

  purrr::map_lgl(packages_init,
                 require, character.only = T) -> lgl_installed
  pkgsMissing <- packages_init[!lgl_installed]
  if(length(pkgsMissing)!=0){
    warning("以下套件還沒安裝\n",
            paste0(pkgsMissing,"\n"))
  }
}

init_courseSetup = function(packages_init=econDV::packages_init){
  message("課程套件檢查與安裝")
  installMissingRequires(packages_init=packages_init)

  message("安裝google noto sans TC字體")

  rprojroot::is_rstudio_project -> root
  root$make_fix_file() -> root_file
  destfile=file.path(root_file(),"Noto_Sans_TC.zip")
  download.file("https://fonts.google.com/download?family=Noto%20Sans%20TC",
                destfile = destfile)
  message(
    "字形檔已下載至\n", destfile, "\n",
    "Win10: 請依 https://support.microsoft.com/zh-tw/help/314960/how-to-install-or-remove-a-font-in-windows \n",
    "Mac: 請依 https://support.apple.com/zh-tw/HT201749 \n",
    "安裝字體。"
  )
}

dv_setup <- function(){
  if(!require("rprojroot")){
    stop("請先執行 init_courseSetup()")
  }
  data("RprofileLines")
  rprojroot::is_rstudio_project -> root
  root$make_fix_file() -> root_file
  writeLines(
    RprofileLines,
    con=file.path(
      root_file(),".Rprofile"
    )
  )
  message("環境設定完成，請重啟project")

}

# helpers -----------------------------------------------------------------


gen_RprofileTemplateData = function(){
  rprojroot::is_r_package -> pkgroot
  pkgroot$make_fix_file() -> pkgrootfile
  file.path(
    pkgrootfile(),"RprofileTemplate.R"
  ) -> templatefilename
  readLines(
    templatefilename
  ) -> RprofileLines
  usethis::use_data(RprofileLines, overwrite = T)
}

gen_initPackages <- function(){
  rprojroot::is_r_package -> pkgroot
  pkgroot$make_fix_file() -> pkgrootfile
  file.path(
    pkgrootfile(),"RprofileTemplate.R"
  ) -> templatefilename
  source(templatefilename)
  packages_init <- .packages_init
  usethis::use_data(packages_init, overwrite = T)
}
