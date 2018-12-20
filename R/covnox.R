#' Divide data.frames
#'
#' @description Return data.frame sf
#' @param cov Data-frame of numerics
#' @param nox Data-frame of numerics
#' @param geo 'sf'
#' @return Return data.frame or 'sf'
#' @importFrom sf  st_sf st_crs
#' @export
#' @examples \dontrun{
#' #do not run
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
