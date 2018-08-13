#' Divide data.frames
#'
#' @description Return data.frame sf
#' @param X Character; path to .rds
#' @param name Character; Name of new column.
#' @param geo 'sf'
#' @return Return data.frame or 'sf'
#' @importFrom sf st_set_geometry st_sf
#' @export
#' @examples \dontrun{
#' }
covnox <- function(cov, nox, geo){
  df <- sum_df(cov)/sum_df(nox)
  df[is.na(df)]<- 0
  if(missing(geo)){
    return(df)
  }else{
    df <- sf::st_sf(df, geometry = geo$geometry, crs = sf::st_crs(geo))
    return(df)
  }

}
