#' Calculates rowMeans reading .rds
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
rmeans_df <- function(X, name, geo) {
  df <- as.data.frame(do.call("cbind",lapply(1:length(X), function(i){
    x <- sf::st_set_geometry(readRDS(X[i]), NULL)
    x$id <- NULL
    rowMeans(x)
  })))
  df$new <- rowSums(df)
  if(!missing(name)) names(df)[ncol(df)] <- name
  if(!missing(geo))
    df <- sf::st_sf(df, geometry = geo$geometry, crs = sf::st_crs(geo))
  return(df)
}
