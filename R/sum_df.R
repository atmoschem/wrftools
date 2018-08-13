#' Read .rds and sum data.frames returning data.frame or sf
#'
#' @description Return data.framee or sf
#' @param X Character; path to .rds
#' @param geo 'sf'
#' @return Return data.frame or 'sf'
#' @importFrom sf st_set_geometry st_sf
#' @export
#' @examples \dontrun{
#' }
sum_df <- function(X, geo) {
  df <- lapply(1:length(X), function(i){
    x <- sf::st_set_geometry(readRDS(X[i]), NULL)
    x$id <- NULL
    x
  })
  for(i in 2:length(df) ){
    dft <- df[[1]]
    dft <- dft + df[[i]]
  }
  df <- dft
  if(missing(geo)){
    return(df)
  }else{
    df <- sf::st_sf(df, geometry = geo$geometry, crs = sf::st_crs(geo))
    return(df)
  }
}
