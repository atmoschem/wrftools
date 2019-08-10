#' extractor from atmospheric models
#'
#' @description Return data.framee or list of raster and df from points.
#' @param atmos Character; path to output of model (wrfout)
#' @param vars Character; Variable with has 3 or 4 dimensions.
#' @param level Integer; Which level
#' @param tz Character; Time zone
#' @param points data.frame, matrix, SpatialPointsDataFrame or sf 'POINTS',
#' with coordinates East-weast (lat), north-south (long) and 'Station'.
#' @param stations Character; names of stations for each point.
#' @param crs_points Integer, crs points.
#' @param model Character; Currently, only "WRF"
#' @param return_list Logical; If TRUE, return a list with raster and data.frames
#' if FALSE, only data.frame.
#' @param verbose logical
#' @return Return data.framee or list of raster and df
#' @importFrom eixport wrf_get
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom raster extract brick raster
#' @importFrom sf st_as_sf st_transform as_Spatial st_sf
#' @importFrom methods as
#' @note Based on NCL scrip xtractor from DCA/IAG/USP
#' @export
#' @examples \dontrun{
#' data(cetesb)
#' #use your wrfout
#' #wrf <- "~/Documentos/wrfo/wrfoA.nc"
#' t2 = c("T2", "o3", "no")
#' df <- xtractor(atmos = wrf, vars = t2, points = cetesb,
#' stations = cetesb$Station[1:3])
#' r <- xtractor(atmos = wrf, vars = t2, points = cetesb return_list = T)
#' }
xtractor <- function (atmos,
                       vars,
                       level = 1,
                       points,
                       stations = "CC",
                       crs_points = 4326,
                       model = "WRF",
                       tz = "America/Sao_Paulo",
                       return_list = FALSE,
                       verbose = TRUE){
# desde aqui
  if (class(atmos)[1] == "character") {

    if(verbose) cat(paste0("Reading NetCDF \n"))

    xx <- ncdf4::nc_open(atmos)
    lat <- ncdf4::ncvar_get(xx, "XLAT")
    lon <- ncdf4::ncvar_get(xx, "XLONG")
    times <- ncdf4::ncvar_get(xx, "Times")
    times <- gsub(pattern = " ", replacement = "_", x = times)
    times <- paste0("H", times)

    lr <- list()

    if(verbose) cat(paste0("Converting to raster \n"))

    for (i in 1:length(vars)) {
      x <- eixport::wrf_get(file = atmos, name = vars[i],
                            as_raster = FALSE)
      if (length(dim(x)) == 3) {
        rx <- eixport::wrf_get(file = atmos, name = vars[i],
                               as_raster = TRUE)
      }
      else if (length(dim(x)) == 4) {
        lista <- list()

        for (j in 1:dim(x)[4]) {
          lista[[j]] <- raster::raster(t(x[1:dim(x)[1],
                                           dim(x)[2]:1,
                                           level, j]),
                                       xmn = min(lon),
                                       xmx = max(lon),
                                       ymn = min(lat),
                                       ymx = max(lat),
                                       crs = "+init=epsg:4326")
        }
        rx <- raster::brick(lista)
      }
      lr[[i]] <- rx
    }
    names(lr) <- vars
  }
# hasta aqui

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
  } else if (class(points)[1] == "sf") {
    names(points) <- c("Station", "geometry")
    if (crs_points != 4326)
      points <- sf::st_transform(points, 4326)
  } else if (class(points)[1] == "SpatialPointsDataFrame") {
    names(points) <- c("Station")
    if (crs_points != 4326)
      points <- sf::st_transform(sf::st_as_sf(points),
                                 4326)
    names(points) <- c("Station")
  }
# hasta aqui


  if (class(atmos)[1] == "character") {

    if(verbose)   cat(paste0("Extracting data\n\n"))
 df <- list()
lista <- list()
for(j in 1:length(stations)) {
	 for (i in 1:length(lr)) {
      df[[i]] <- raster::extract(x = lr[[i]],
                                 y = sf::as_Spatial(points[j,]),
                                 df = TRUE)
    }
    dft <- lapply(1:length(df), function(i) {
   df[[i]]$ID <- NULL
      names(df[[i]]) <- times
      dff <- data.frame(x = unlist(df[[i]]))
      dff
    })
    dft = do.call("cbind", dft)
    names(dft) <- c(vars)
    dft$Time <- times
    dft$Station = stations[j]
    dft <- sf::st_sf(dft, geometry = sf::st_sfc(sf::st_as_sf(points)))
    lista[[j]] <- dft
	}
    dft = do.call("rbind", lista)

    # times
    dft$Time <- as.POSIXct(x = dft$Time, format = "H%Y-%m-%d_%H:%M:%S",
                           tz = "GMT")
    dft$LT <- dft$Time
    attr(dft$LT, "tzone") <- tz

    names(lr) <- vars
    r <- list(lr, dft)
    names(r) <- c("raster", "data")
    if (return_list) {
      return(r)
    }
    else {
      return(dft)
    }
  }
}
