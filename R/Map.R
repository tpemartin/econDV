#' A fixture to OSM simple feature date name error problem
#'
#' @param sf_object An sf object
#'
#' @return
#' @export
#'
#' @examples none
osm_geom_rename <- function(sf_object){
  sf_object %>%
    st_geometry() -> sfc_sf_object
  for(i in seq_along(sfc_sf_object)){
    names(sfc_sf_object[[i]][[1]]) <-
      1:length(names(sfc_sf_object[[i]][[1]]))
  }

  sf_object %>%
    st_set_geometry(
      sfc_sf_object
    ) -> sf_object2
  return(sf_object2)
}
