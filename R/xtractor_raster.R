#' Extract several variables from RasterBricks
#'
#' @description Return data.framee or list of raster and df from points. When is data.frame
#' contains cell value, interpolation for lat long based on neighour cells,
#' @param br RasterBrick
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
#' # br <- brick("wrfout")
#' data(cetesb)
#' df <- xtractor_raster(br = br,  points = cetesb)
#' }
xtractor_raster <- function (br,
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
  stations <- as.character(points[["Station"]])
  # hasta aqui

  if(verbose)   cat(paste0("Extracting data\n"))


  df <- raster::extract(x = br,
                        y = sf::as_Spatial(points),
                        df = TRUE,
                        method = "simple")

  if(verbose)   cat(paste0("Creating data.frame\n"))
  df_t2 <- as.data.frame(t(df[, 2:ncol(df)]))
  names(df_t2) <- stations

  dft <- melt.data.table(
    data = as.data.table(df_t2),
    variable.name = "Station",
    value.name = "x")

  if(verbose)   cat(paste0("Adding time\n"))
  if(!missing(times)){
    if(length(times) != raster::nlayers(br)) stop("length times and nlayers are different!")
    if(!inherits(times, "POSIXct")) stop("Times must have class POSIXct in GMT")

    dft$Time <- rep(times, length(stations))
    dft$Time <- as.POSIXct(dft$Time, tz = "GMT")
    dft$LT <- dft$Time
    attr(dft$LT, "tzone") <- tz
    if(verbose)   cat(paste0("Merging\n"))
    dft <- merge(dft, points, by = "Station", all.x = T)

    dft$Station <- as.character(dft$Station)
    if(verbose)   cat(paste0("Transforming in sf\n"))
    dft <- sf::st_sf(dft, geometry = dft$geometry)
    return(dft[, c("x", "Station", "Time", "LT")])

  }  else {
    dft$Time <- seq_along(raster::nlayers(br))
    if(verbose)   cat(paste0("Merging\n"))
    dft <- merge(dft, points, by = "Station", all.x = T)

    dft$Station <- as.character(dft$Station)
    if(verbose)   cat(paste0("Transforming in sf\n"))
    dft <- sf::st_sf(dft, geometry = dft$geometry)
    return(dft[, c("x", "Station", "Time")])

  }
}
