#' Extract several variables from RasterBricks
#'
#' @description Return data.framee or list of raster and df from points. When is data.frame
#' contains cell value, interpolation for lat long based on neighour cells,
#' @param br RasterBrick
#' @param var Character; 1 word
#' @param times Character; UTC time (one time for each layer)
#' @param tz Character; Time zone
#' @param points data.frame, matrix, SpatialPointsDataFrame or sf 'POINTS',
#' with coordinates East-weast (lat), north-south (long) and \strong{Station}.
#' @param crs_points Integer, crs points.
#' @param verbose logical
#' @return Return data.framee or list of raster and df
#' @importFrom eixport wrf_get
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom raster extract brick raster bbox nlayers
#' @importFrom sf st_as_sf st_transform as_Spatial
#' @importFrom methods as
#' @importFrom stats sd
#' @note Based on NCL scrip xtractor from DCA/IAG/USP
#' @export
#' @examples \dontrun{
#' # do not run
#' # br <- brick("brfout")
#' data(cetesb)
#' #use your wrfout
#' df <- xtractor_raster(br = br, vars = "T2", points = cetesb)
#' }
xtractor_raster <- function (br,
                             var,
                             points,
                             times,
                             crs_points = 4326,
                             tz = "America/Sao_Paulo",
                             verbose = TRUE){

  # desde aqui
  if(verbose) cat(paste0("Points to sp\n"))
  if (class(points)[1] == "matrix" | class(points)[1] == "data.frame") {
    points <- as.data.frame(points)
    names(points) <- c("x", "y", "Station")
    points <- sf::st_as_sf(points,
                           coords = c(x = "x", y = "y"),
                           crs = crs_points)
    if (crs_points != 4326)
      points <- sf::st_transform(points, 4326)
  } else if (class(points)[1] == "SpatialPointsDataFrame") {
    names(points) <- c("Station")
    if (crs_points != 4326)
      points <- sf::st_transform(sf::st_as_sf(points),
                                 4326)
    names(points) <- c("Station")
  }
  if(verbose) cat(paste0("Cropping points for wrfout\n"))
  db <- raster::bbox(br)
  points <- sf::st_crop(points,
                        xmin = db[1,1],
                        xmax = db[1,2],
                        ymin = db[2,1],
                        ymax = db[2,2])
  stations <- points[["Station"]]
  # hasta aqui

  if(verbose)   cat(paste0("Extracting data\n"))
  lista <- list()

  for(j in 1:length(stations)) {
    # direct extraction
    df <- raster::extract(x = br,
                          y = sf::as_Spatial(points[j,]),
                          df = TRUE,
                          method = "simple")
    class(df)
    dft <- data.frame(x = unlist(df[, 2:ncol(df)]))
    names(dft) <- var
      if(!missing(times))  dft$Time <- times else dft$Time <- 1:(raster::nlayers(br))
    dft$Station = stations[j]
    dft <- merge(dft, points, by = "Station", all.x = T)
    dft <- sf::st_sf(dft, geometry = dft$geometry)
    lista[[j]] <- dft
  }
  dft = do.call("rbind", lista)

  # times
  dft$Time <- as.POSIXct(x = dft$Time, format = "H%Y-%m-%d_%H:%M:%S",
                         tz = "GMT")
  dft$LT <- dft$Time
  attr(dft$LT, "tzone") <- tz
  dft$Station <- as.character(dft$Station)
  return(dft[, c(var, "Station", "Time", "LT")])
}
