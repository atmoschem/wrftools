#' Location some air quality stations CETESB
#'
#' A spatial data-frame with locations of CETESB stations, class sf.
#'
#' @format A spatial data-frame with locations of CETESB stations, class sf.
#' \describe{
#'   \item{red}{type of station}
#'   \item{UGRHI}{integer}
#'   \item{VOCACIONAL}{Character}
#'   \item{UTM_SIRGAS2000}{Character}
#'   \item{fuso}{Integer}
#'   \item{EPSG}{Integer}
#'   \item{ENDERECO}{Character}
#'   \item{OBSERVACOES}{Observations}
#'   \item{id}{Integer for Rpollution or koffing}
#'   \item{X}{Numeric}
#'   \item{Y}{Numeric}
#'   \item{stationname}{Character}
#'   \item{initial_date}{Character}
#'   \item{BEN}{Character}
#'   \item{CO}{Character}
#'   \item{ERT}{Character}
#'   \item{MP10}{Character}
#'   \item{MP2.5}{Character}
#'   \item{NO}{Character}
#'   \item{NO2}{Character}
#'   \item{NOx}{Character}
#'   \item{O3}{Character}
#'   \item{SO2}{Character}
#'   \item{TOL}{Character}
#'   \item{TEMP}{Character}
#'   \item{UR}{Character}
#'   \item{VV}{Character}
#'   \item{address}{Character}
#'   \item{UTM_SIRGAS_2000}{Character}
#'   \item{dados}{Character: CETESB op INMET}
#'   \item{geometry}{Character}
#'   data(cetesb)
#' }
#' @source \url{https://cetesb.sp.gov.br/ar/}
#' @usage data(cetesb)
#' @docType data
"cetesb"
