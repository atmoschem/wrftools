#' st_bbox_wrf from wrf files
#'
#' @description \code{\link{st_bbox_wrf}} from wrf files using the corner coordinates
#' \code{\link{st_bbox_wrf_input}} is for wrf inputs
#' \code{\link{st_bbox_wrf_chemi}} is for wrf chemi inputs #TODO CHECK
#' \code{\link{st_bbox_wrf_bdy}} is for wrf boundaries #TODO CHECK
#' \code{\link{st_bbox_wrf_out}} is for wrf outs #TODO CHECK
#' \code{\link{st_bbox_wrf_geo}} is for wrf outs #TODO CHECK
#' @param filewrf wrf file
#' @param crs epsg code number (see http://spatialreference.org/ref/epsg/)
#' @param type Type of wrf file: "input", "geo", "out", "chemi", "bdy"
#'
#' @importFrom ncdf4 ncvar_get
#' @importFrom sf st_crs st_bbox
#' @export
#' @examples {
#' # Do not run
#' wrf <- paste(system.file("extdata", package = "eixport"),
#'                          "/wrfinput_d02", sep="")
#' gwrf  <- st_bbox_wrf_input(wrf)
#' plot(gwrf, axes = TRUE)
#'}
st_bbox_wrf <- function(filewrf, type = "geo", crs = 4326){
  wrf <- ncdf4::nc_open(filewrf)

  if(type %in% c("input", "bdy", "out", "geo")){
    lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT")
    lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG")
  } else if(type == "geo"){
    lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT_M")
    lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG_M")
  }
  b <- sf::st_bbox(c(xmin = min(lon),
                     xmax = max(lon),
                     ymax = max(lat),
                     ymin = min(lat)),
                   crs = sf::st_crs(crs))
  return(b)
}

#' @export
#' @title st_bbox_wrf_input
#' @param filewrf wrf file
#' @param crs epsg code number (see http://spatialreference.org/ref/epsg/)
#' @rdname st_bbox_wrf_input
st_bbox_wrf_input <- function(filewrf, crs = 4326){
  wrf <- ncdf4::nc_open(filewrf)
  lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT")
  lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG")
  b <- sf::st_bbox(c(xmin = min(lon),
                     xmax = max(lon),
                     ymax = max(lat),
                     ymin = min(lat)),
                   crs = sf::st_crs(crs))
  return(b)
}

#' @export
#' @title st_bbox_wrf_chemi
#' @param filewrf wrf file
#' @param crs epsg code number (see http://spatialreference.org/ref/epsg/)
#' @rdname st_bbox_wrf_chemi
st_bbox_wrf_chemi <- function(filewrf, crs = 4326){
  wrf <- ncdf4::nc_open(filewrf)
  lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT")
  lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG")
  b <- sf::st_bbox(c(xmin = min(lon),
                     xmax = max(lon),
                     ymax = max(lat),
                     ymin = min(lat)),
                   crs = sf::st_crs(crs))
  return(b)
}


#' @export
#' @title st_bbox_wrf_bdy
#' @param filewrf wrf file
#' @param crs epsg code number (see http://spatialreference.org/ref/epsg/)
#' @rdname st_bbox_wrf_bdy
st_bbox_wrf_bdy <- function(filewrf, crs = 4326){
  wrf <- ncdf4::nc_open(filewrf)
  lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT")
  lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG")
  b <- sf::st_bbox(c(xmin = min(lon),
                     xmax = max(lon),
                     ymax = max(lat),
                     ymin = min(lat)),
                   crs = sf::st_crs(crs))
  return(b)
}

#' @export
#' @title st_bbox_wrf_out
#' @param filewrf wrf file
#' @param crs epsg code number (see http://spatialreference.org/ref/epsg/)
#' @rdname st_bbox_wrf_out
st_bbox_wrf_out <- function(filewrf, crs = 4326){
  wrf <- ncdf4::nc_open(filewrf)
  lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT")
  lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG")
  b <- sf::st_bbox(c(xmin = min(lon),
                     xmax = max(lon),
                     ymax = max(lat),
                     ymin = min(lat)),
                   crs = sf::st_crs(crs))
  return(b)
}


#' @export
#' @title st_bbox_wrf_geo
#' @param filewrf wrf file
#' @param crs epsg code number (see http://spatialreference.org/ref/epsg/)
#' @rdname st_bbox_wrf_geo
st_bbox_wrf_geo <- function(filewrf, crs = 4326){
  wrf <- ncdf4::nc_open(filewrf)
  lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT_M")
  lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG_M")
  b <- sf::st_bbox(c(xmin = min(lon),
                     xmax = max(lon),
                     ymax = max(lat),
                     ymin = min(lat)),
                   crs = sf::st_crs(crs))
  return(b)
}
