library(wrftools)
data(cetesb)
cetesb <- cetesb[!is.na(cetesb$Station), "Station"]

atmos = "wrfout_d01_2018-04-19_00:00:00"
vars = c("co", "T2")
level =1


points = cetesb[c(11, 15), ]
stations = cetesb$Station[c(11, 15)]
crs_points = 4326
tz = "America/Sao_Paulo"
return_list = FALSE
verbose = TRUE
model = "WRF"


ra <- lr[[1]]
# xtractor <- function (atmos,
#                       vars,
#                       level = 1,
#                       points,
#                       stations = "CC",
#                       crs_points = 4326,
#                       model = "WRF",
#                       tz = "America/Sao_Paulo",
#                       return_list = FALSE,
#                       verbose = TRUE){
#   # desde aqui
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



if(verbose)   cat(paste0("Extracting data\n\n"))
lista <- list()

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
                              buffer = sum(res(lr[[1]]))/2*1.2)
  rows <- nrow(rows)
  # mean from the values of the four nearest raster cells.
  df_mean <- lapply(1:length(lr), function(i) {
    raster::extract(x = lr[[i]],
                    y = sf::as_Spatial(points[j,]),
                    df = TRUE,
                    buffer = sum(res(lr[[i]]))/2*1.2)
  })


  dft_mean <- lapply(1:length(df_nei), function(i) {
    df_mean[[i]]$ID <- NULL
    names(df_mean[[i]]) <- times
    dff <- data.frame(x = unlist(df_mean[[i]]))
    dff
  })
  dft_mean = do.call("cbind", dft_mean)
  names(dft_mean) <- paste0(vars, "_mean")

  # sd from the values of the four nearest raster cells.
  df_sd <- lapply(1:length(lr), function(i) {
    raster::extract(x = lr[[i]],
                    y = sf::as_Spatial(points[j,]),
                    df = TRUE,
                    buffer = sum(res(lr[[i]]))/2*1.2,
                    fun = sd)
  })
  dft_sd <- lapply(1:length(df_nei), function(i) {
    df_sd[[i]]$ID <- NULL
    names(df_sd[[i]]) <- times
    dff <- data.frame(x = unlist(df_sd[[i]]))
    dff
  })
  dft_sd = do.call("cbind", dft_sd)
  names(dft_sd) <- paste0(vars, "_sd")



  dft <- cbind(dft, dft_nei, dft_mean, df_mean)
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
