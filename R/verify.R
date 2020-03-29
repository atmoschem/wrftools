#' Compare observations and simulations
#'
#' @description Return data.frame with Correlation, MeanBias, SD and RSME.
#' @param dfobs data.frame in long format with columns must be Station, x and time
#' @param dfmod data.frame in long format with columns must be Station, x and time
#' @param x Character; name of the column with the parameter
#' @param Station Character; name of the column with the stations
#' @param time Character; name of the time column with class POSIXct.
#' @param time_spinup time object, class POSIXct to exclude observation with that time or before.
#' @importFrom stats cor sd
#' @return Return data.framee or list of raster and df
#' @export
#' @examples \dontrun{
#' # do not run
#' # Columns must be Station, x and time
#' verify(dfobs = df_co[df_co$data == "CETESB", 1:3],
#'        dfmod = df_co[df_co$data == "2016", 1:3],
#'        x = "co",
#'        time_spinup = ISOdatetime(2016,4,8,12,0,0))
#' }
verify <- function(dfobs,
                   dfmod,
                   x,
                   Station = "Station",
                   time = "LT",
                   time_spinup){# = ISOdatetime(2016,4,8,12,0,0),

  if(length(names(dfobs)) > 3) stop("Columns of dfobs must be Station, x and time")
  if(length(names(dfmod)) > 3) stop("Columns of dfmod must be Station, x and time")
  dfobs <- as.data.frame(dfobs)
  dfmod <- as.data.frame(dfmod)
  if(!missing(time_spinup)) {
    dfi <- lapply(1:length(unique(dfobs[[Station]])), function(i){
      obs <- dfobs[dfobs[[Station]] == unique(dfobs[[Station]])[i], ]
      mod <- dfmod[dfmod[[Station]] == unique(dfmod[[Station]])[i], ]

      dx <- merge(obs, mod, by = time, all.x = T)
      dx <- dx[!is.na(dx[paste0(x,".x")]   ), ]
      dx <- dx[!is.na(dx[paste0(x,".y")]   ), ]

      dx <- dx[dx[[time]] > time_spinup, ]

      # selecting time, Station and x
      dx <- dx[, c(time, paste0(x,".x"), paste0(x,".y"))]

      names(dx) <- c("LT", "obs", "mod")

      df <- data.frame(Station = unique(dfobs[[Station]])[i],
                       Correlation = stats::cor(dx$obs, dx$mod),
                       MeanBias = mean(dx$obs - dx$mod),
                       MSE = mean((dx$obs - dx$mod)^2),
                       SD = stats::sd(dx$obs - dx$mod))
      df$RMSE <- round(df$MSE^0.5, 2)
      df$Correlation <- round(df$Correlation, 2)
      df$MeanBias <- round(df$MeanBias, 2)
      df$SD <- round(df$SD, 2)
      df$MSE <- NULL
      df$RMSE <- round(df$RMSE, 2)
      df
    })
  } else {
    dfi <- lapply(1:length(unique(dfobs[[Station]])), function(i){
      obs <- dfobs[dfobs[[Station]] == unique(dfobs[[Station]])[i], ]
      mod <- dfmod[dfmod[[Station]] == unique(dfmod[[Station]])[i], ]

      dx <- merge(obs, mod, by = time, all.x = T)
      dx <- dx[!is.na(dx[paste0(x,".x")]   ), ]
      dx <- dx[!is.na(dx[paste0(x,".y")]   ), ]

      if(!missing(time_spinup))     dx <- dx[dx[[time]] > time_spinup, ]

      # selecting time, Station and x
      dx <- dx[, c(time, paste0(x,".x"), paste0(x,".y"))]

      names(dx) <- c("LT", "obs", "mod")

      df <- data.frame(Station = unique(dfobs[[Station]])[i],
                       Correlation = cor(dx$obs, dx$mod),
                       MeanBias = mean(dx$obs - dx$mod),
                       MSE = mean((dx$obs - dx$mod)^2),
                       SD = sd(dx$obs - dx$mod))
      df$RMSE <- round(df$MSE^0.5, 2)
      df$Correlation <- round(df$Correlation, 2)
      df$MeanBias <- round(df$MeanBias, 2)
      df$SD <- round(df$SD, 2)
      df$MSE <- NULL
      df$RMSE <- round(df$RMSE, 2)
      df
    })

  }

  dfi <- as.data.frame(do.call("rbind", dfi))
  return(dfi)
}
