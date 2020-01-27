#' Location some air quality stations CETESB
#'
#' @format A spatial data-frame with locationbs of CETESB stations, class sf.
#' Extracted from the report of air quality 2018
#' \describe{
#'   \item{red}{type of station}
#'   \item{UGRHI}{integer}
#'   \item{VOCACIONAL}{Character}
#'   \item{UTM_SIRGAS2000}{character}
#'   \item{fuso}{integer}
#'   \item{EPSG}{integer}
#'   \item{ENDERECO}{character}
#'   \item{OBSERVACOES}{Observations}
#'   \item{X}{Numeric}
#'   \item{Y}{Numeric}
#' }
#' @source \url{https://cetesb.sp.gov.br/ar/}
#' @usage data(cetesb)
#' @docType data
"cetesb"
