#' DEPRECATED project your wrfout files to any EPSG projection
#'
#' @description Read a wrfout file, project its coordinates and put them back
#'
#' @param file name of file interactively (default) or specified
#' @param map_proj "lambert" or "mercator"
#' @param truelat1 return a raster instead of an array
#' @param truelat2 crs to use if as_raster is TRUE
#' @param stand_lon level for rasters from a 4D variable
#' @param epsg_target level for rasters from a 4D variable
#' @param map_proj_char_target level for rasters from a 4D variable
#' @param backup_path level for rasters from a 4D variable
#' @param verbose level for rasters from a 4D variable
#'
#' @importFrom ncdf4 ncvar_put nc_open ncvar_get  ncatt_put ncvar_put nc_close
#' @importFrom sf st_as_sf st_crs st_transform
#' @export
#'
#' @examples \dontrun{
#' # dont run
#' project_wrf(file = "wrfout_d01_2016-12-31_00:00:00",
#'             truelat1 = -30,
#'             truelat2 = -60,
#'             stand_lo n =-71.626119)
#'}
project_wrf <- function(file = file.choose(),
                        map_proj = "lambert",
                        truelat1,
                        truelat2,
                        stand_lon,
                        epsg_target = "+init=epsg:32719",
                        map_proj_char_target = "UTM",  # really necessary for mmif?
                        backup_path= getwd(),
                        verbose = TRUE){
  .Deprecated("project_wrf")
  # # openning wrf
  # message("I hope you have a back uo your wrf file")
  # wrf <- ncdf4::nc_open(file, write = TRUE)
  #
  # # getting attributs
  # g_atributos  <- ncdf4::ncatt_get(wrf, 0)
  #
  # # Reading parameters
  # if(map_proj == "lambert"){
  #   projection <-paste0("+proj=lcc",
  #                       " +lat_0=", truelat1,                     # truelat1
  #                       " +lon_0=", stand_lon,                    # stand_lon
  #                       " +lat_1=", truelat2 ,                    # truelat2
  #                       " +ellps=WGS84 +no_defs")
  #
  # } else if(map_proj == "mercator"){
  #   projection <-paste0("+proj=merc",
  #                       " lambert +lat_0=", truelat1,                       # truelat1
  #                       " +ellps=WGS84 +no_defs")
  # } else {
  #   stop("currently lambert and mercator only")
  # }
  # if(verbose) cat("\nProjection of wrf file:\n", projection, " \n")
  #
  # lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT")
  # lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG")
  # if(verbose) cat("\nRange original coordinates: \n")
  # if(verbose) cat("XLONG: : ", range(lon), " \n")
  # if(verbose) cat("XLAT: : ", range(lat), " \n")
  #
  # if(verbose) cat("\nMaking back up your original coordinates and \n")
  # lala <- list(lat = lat, lon = lon, attributes_wrf = g_atributos)
  # saveRDS(lala, paste0(backup_path,
  #                      "/",
  #                      strftime(Sys.time(), format = "%Y%m%d_%H_%S"),
  #                      "_backup.rds"))
  # if(verbose) cat(" Back up:",
  #                 paste0(backup_path,
  #                        "/",
  #                        strftime(Sys.time(), format = "%Y%m%d_%H_%S"),
  #                        "_backup.rds"))
  #
  # df <- data.frame(x = as.vector(lon[, , 1]),
  #                  y = as.vector(lat[, , 1]))
  # df <- sf::st_as_sf(df, coords = c("y", "x"), crs = sf::st_crs(projection))
  #
  # #  transforming object and converting into points
  # if(verbose) cat("\n\nTransforming...\n")
  # df2 <- sf::st_coordinates(sf::st_transform(df, epsg_target))
  #
  # if(verbose){
  #   cat("\nYour new coordinates:")
  #   if(verbose) cat("\nXLONG From: ", min(df2[, 1]), ", to", max(df2[, 1]), " \n")
  #   if(verbose) cat("XLAT From: ", min(df2[, 2]), ", to", max(df2[, 2]), " \n")
  #
  # }
  #
  # if(verbose) cat("\n generating arrays with new coordinates\n")
  # # generating arrays with new coordinates
  # new_lon <- array(data = df2[, 1], dim = dim(lon))
  # new_lat <- array(data = df2[, 2], dim = dim(lat))
  #
  #
  # # replacing coordinates
  # if(verbose) cat("\n injecting with new 'XLONG'\n\n")
  # ncdf4::ncvar_put(wrf,varid = "XLONG", new_lon)
  #
  # if(verbose) cat("injecting with new 'XLAT'\n")
  # ncdf4::ncvar_put(wrf,varid = "XLAT", new_lat)
  #
  # # changing MAP_PROJ_CHAR
  # g_atributos$MAP_PROJ_CHAR <- map_proj_char_target
  #
  # # putting new attributes
  # for(i in 1:length(g_atributos)){
  #   ncdf4::ncatt_put(wrf,
  #                    varid = 0,
  #                    attname = names(g_atributos)[i],
  #                    attval = g_atributos[[i]])
  # }
  # ncdf4::nc_close(wrf)
}
