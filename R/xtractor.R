#' Extract several variables from WRF outputs
#'
#' @description Return data.framee or list of raster and df from points. When is data.frame
#' contains cell value, interpolation for lat long based on neighour cells,
#' @param atmos Character; path to output of model (wrfout)
#' @param vars Character; Variable with has 3 or 4 dimensions.
#' @param level Integer; Which level
#' @param tz Character; Time zone
#' @param points data.frame, matrix, SpatialPointsDataFrame or sf 'POINTS',
#' with coordinates East-weast (lat), north-south (long) and 'Station'.
#' @param stations Character; names of stations for each point.
#' @param crs_points Integer, crs points.
#' @param fac_res Numeric, expansion factor for buffer. Buffer distance is based on average resolution.
#' Hence, this factor controls the distance of the buffer.
#' @param model Character; Currently, only "WRF"
#' @param return_list Logical; If TRUE, return a list with raster and data.frames
#' if FALSE, only data.frame.
#' @param verbose logical
#' @return Return data.framee or list of raster and df
#' @importFrom eixport wrf_get
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom raster extract brick raster bbox
#' @importFrom sf st_as_sf st_transform as_Spatial
#' @importFrom methods as
#' @importFrom stats sd
#' @note Based on NCL scrip xtractor from DCA/IAG/USP
#' @export
#' @examples \dontrun{
#' # do not run
#' data(cetesb)
#' cetesb <- cetesb[!is.na(cetesb$Station), "Station"]
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
                      stations,
                      crs_points = 4326,
                      fac_res = 1.2,
                      model = "WRF",
                      tz = "America/Sao_Paulo",
                      return_list = FALSE,
                      verbose = TRUE){
  if(!missing(stations)) {
    warning("`stations` are not needed and they are derived from points$Stations")
  }
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
  if(verbose) cat(paste0("Cropping points for wrfout\n"))
  db <- raster::bbox(lr[[1]])
  points <- sf::st_crop(points,
                        xmin = db[1,1],
                        xmax = db[1,2],
                        ymin = db[2,1],
                        ymax = db[2,2])
  stations <- points[["Station"]]
  # hasta aqui

  if(verbose)   cat(paste0("Extracting data\n"))
  lista <- list()

  message("for 3D arrays such as T2, U10 or V10, extractor with buffer returns numeric,\n
          therefore, in this cases they are transformed into matrices, \n
          resulting in sd equals to 0\n")

  for(j in 1:length(stations)) {
    # direct extraction
    df <- lapply(1:length(lr), function(i) {
      raster::extract(x = lr[[i]],
                      y = sf::as_Spatial(points[j,]),
                      df = TRUE, method = "simple")
    })
    dft <- lapply(1:length(df), function(i) {
      df[[i]]$ID <- NULL
      names(df[[i]]) <- times
      dff <- data.frame(x = unlist(df[[i]]))
      dff
    })
    dft = do.call("cbind", dft)
    names(dft) <- vars

    # interpolation from the values of the four nearest raster cells.
    df_nei <- lapply(1:length(lr), function(i) {
      raster::extract(x = lr[[i]],
                      y = sf::as_Spatial(points[j,]),
                      df = TRUE,
                      method = "bilinear")
    })
    dft_nei <- lapply(1:length(df_nei), function(i) {
      df_nei[[i]]$ID <- NULL
      names(df_nei[[i]]) <- times
      dff <- data.frame(x = unlist(df_nei[[i]]))
      dff
    })
    dft_nei = do.call("cbind", dft_nei)
    names(dft_nei) <- paste0(vars, "_nei")

    # number of rows (retorna lista)
    rows <- raster::extract(x = lr[[1]],
                            y = sf::as_Spatial(points[j,]),
                            df = TRUE,
                            buffer = sum(raster::res(lr[[1]]))/2*1.2)
    nrows <- nrow(rows)

    # mean from the values of the four nearest raster cells.
    df_x <- lapply(1:length(lr), function(i) {
      x <- raster::extract(x = lr[[i]],
                           y = sf::as_Spatial(points[j,]),
                           buffer = sum(raster::res(lr[[i]]))/2*1.2)[[1]]
      if(!is.matrix(x)) {
        as.data.frame(matrix(rep(unlist(x), nrows), nrow = nrows, byrow = TRUE))
      } else {
        as.data.frame(x)
      }
    })
    l_mean <- lapply(df_x, colMeans, na.rm = TRUE)
    l_sd <- lapply(1:length(df_x), function(i){
      unlist(lapply(df_x[[i]], sd, na.rm = TRUE))
    })

    dft_mean = as.data.frame(do.call("cbind", l_mean))
    names(dft_mean) <- paste0(vars, "_mean")

    dft_sd = as.data.frame(do.call("cbind", l_sd))
    names(dft_sd) <- paste0(vars, "_sd")

    dft <- cbind(dft, dft_nei, dft_mean, dft_sd)
    df$ncells = rows
    dft$Time <- times
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
