#' Compare observations and simulations and apply wilcoxon R test
#'
#' @description Return data.frame with Correlation, MeanBias, SD, RSME and wilcoxon p.value
#' @param dfmod data.frame in long format with columns must be Station, x and time
#' @param dfobs data.frame in long format with columns must be Station, x and time
#' @param x Character; name of the column with the parameter
#' @param Station Character; name of the column with the stations
#' @param time Character; name of the time column with class POSIXct.
#' @param time_spinup Character; time to filter
#' @param conf.level.p Pearson confidence level for the returned confidence interval.
#' Currently only used for the Pearson product moment correlation coefficient
#' if there are at least 4 complete pairs of observations. default = 0.95
#' @param conf.level.w Wilcox confidence level of the interval. default = 0.95
#' @importFrom stats cor sd wilcox.test
#' @importFrom data.table rbindlist
#' @return Return data.framee or list of raster and df
#' @export
#' @examples \dontrun{
#' # do not run
#' # Columns must be Station, x and time
#' verify(dfmod = df_co[df_co$data == "CETESB", 1:3],
#'        dfobs = df_co[df_co$data == "2016", 1:3],
#'        x = "co",
#'        time_spinup = ISOdatetime(2016,4,8,12,0,0))
#' }
verify <- function(dfobs,
                   dfmod,
                   x,
                   Station = "Station",
                   time = "LT",
                   time_spinup,
                   conf.level.p = 0.95,
                   conf.level.w = 0.95){# = ISOdatetime(2016,4,8,12,0,0),

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
                       Correlation = stats::cor(dx$mod, dx$obs),
                       cor_pvalue = stats::cor.test(dx$mod, dx$obs, conf.level = conf.level.p)$p.value,
                       MeanBias = mean(dx$mod - dx$obs),
                       MSE = mean((dx$mod - dx$obs)^2),
                       SD = stats::sd(dx$mod - dx$obs),
                       wil = stats::wilcox.test(dx$mod, dx$obs, conf.level = conf.level.w)$p.value)
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

      # selecting time, Station and x
      dx <- dx[, c(time, paste0(x,".x"), paste0(x,".y"))]

      names(dx) <- c("LT", "obs", "mod")

      df <- data.frame(Station = unique(dfobs[[Station]])[i],
                       Correlation = cor(dx$mod, dx$obs),
                       cor_pvalue = stats::cor.test(dx$mod, dx$obs, conf.level = conf.level.p)$p.value,
                       MeanBias = mean(dx$mod - dx$obs),
                       MSE = mean((dx$mod - dx$obs)^2),
                       SD = stats::sd(dx$mod - dx$obs),
                       wil = stats::wilcox.test(dx$mod, dx$obs, conf.level = conf.level.w)$p.value)
      df$RMSE <- round(df$MSE^0.5, 2)
      df$Correlation <- round(df$Correlation, 2)
      df$MeanBias <- round(df$MeanBias, 2)
      df$SD <- round(df$SD, 2)
      df$MSE <- NULL
      df$RMSE <- round(df$RMSE, 2)
      df
    })

  }

  dfi <- data.table::rbindlist("rbind", dfi)
  return(dfi)
}
