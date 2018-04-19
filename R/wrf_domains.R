#' plot WRF domains
#'
#' @description \code{\link{wrf_domains}} plots domains on dynamic map.
#'
#' @param max_dom Integer; number of domains
#' @param map_proj Character; currently only 'lambert' and assuming crs 4326.
#' @param ref_lat Real; Latitude coordinate of center of parent domain;
#' @param ref_lon Real; Longitude coordinate of center of parent domain;
#' @param truelat1 Real; the first true latitude for the Lambert conformal
#' projection, or the only true latitude for the Mercator and polar
#' stereographic projections. Not used.
#' @param truelat2 Real; the second true latitude for the Lambert conformal
#' projection, or the only true latitude for the Mercator and polar
#' stereographic projections. Not used.
#' @param stand_lon Real;   real value specifying, for ARW, the longitude
#' that is parallel with the y-axis in the Lambert conformal and polar
#' stereographic projections. For the regular latitude-longitude projection,
#' this value gives the rotation about the earth's geographic poles. Not used.
#' @param parent_id A list of max_dom integers specifying, for each nest,
#' the nesting ratio relative to the domain’s parent. Not used.
#' @param parent_grid_ratio  A list of max_dom integers specifying, for each
#' nest, the nesting ratio relative to the domain’s parent.
#' @param i_parent_start A list of max_dom integers specifying, for each
#' nest, the x-coordinate of the lower-left corner of the nest in the parent
#' unstaggered grid. For the coarsest domain, a value of 1 should be specified.
#' @param j_parent_start A list of max_dom integers specifying, for each
#' nest, the j-coordinate of the lower-left corner of the nest in the parent
#' unstaggered grid. For the coarsest domain, a value of 1 should be specified.
#' @param e_we A list of max_dom integers specifying, for each nest, the nest’s
#'  full west-east dimension. For nested domains, e_we must be one greater
#'  than an integer multiple of the nest's parent_grid_ratio (i.e., e_we =
#'  n*parent_grid_ratio+1 for some positive integer n).
#' @param e_sn A list of max_dom integers which should all be set to 1.
#' @param dx  real value specifying the grid distance in the x-direction where
#' the map scale factor is 1. For ARW, the grid distance is in meters for the
#' 'polar', 'lambert', and 'mercator' projection, and in degrees longitude for
#'  the 'lat-lon' projection; for NMM, the grid distance is in degrees
#'  longitude. Grid distances for nests are determined recursively based on
#'  values specified for parent_grid_ratio and parent_id.
#' @param dy A real value specifying the nominal grid distance in the
#' y-direction where the map scale factor is 1. For ARW, the grid distance
#' is in meters for the 'polar', 'lambert', and 'mercator' projection, and
#' in degrees latitude for the 'lat-lon' projection; for NMM, the grid distance
#'  is in degrees latitude. Grid distances for nests are determined recursively
#'   based on values specified for parent_grid_ratio and parent_id.
#' @param dtm A real value to convert degrees to meters.
#' @param ... ignored
#' @importFrom mapview mapview
#' @importFrom sf st_sfc st_point st_coordinates st_polygon st_sf
#' @return a list of map and sf
#' @export
#' @note This function pltos the domains with mapview. Internally it creates
#' polygons with sf. It currently works with ARW and assuming lambert as WGS84
#' with epsg = 4326. Help me to improve this sergio.ibarra 'at' usp.br
#' The definitions comes mostly from the only WRF manual available at:
#' http://www2.mmm.ucar.edu/wrf/users/docs/user_guide_V3/users_guide_chap3.htm
#' @examples {
#' wrf_domains()
#' }
wrf_domains <- function(max_dom = 3,
                  map_proj = 'lambert',
                  ref_lat   = -21.299,
                  ref_lon   = -46.147,
                  truelat1  = -21.456,
                  truelat2  = -24.550,
                  stand_lon = -46.147,
                  parent_id = c(1,   1,   2),
                  parent_grid_ratio =    c(1,   5,   5),
                  i_parent_start    =    c(1,  41,  25),
                  j_parent_start    =    c(1,  31,  25),
                  e_we              =  c(101, 151, 21),
                  e_sn              =  c(99, 116, 21),
                  dx = 25000,
                  dy = 25000,
                  dtm = (102.42*1000),
                  ...
                  ) {

  dxd01 = dx/dtm # meters
  dyd01 = dy/dtm # meters
  dxd02 = dx/parent_grid_ratio[2]/dtm # meters
  dyd02 = dy/parent_grid_ratio[2]/dtm # meters
  dxd03 = dx/parent_grid_ratio[2]/parent_grid_ratio[3]/dtm # meters
  dyd03 = dy/parent_grid_ratio[2]/parent_grid_ratio[3]/dtm # meters
  if(map_proj == "lambert"){
    crs = 4326
  } else {
    warning("Assuming 4326 for 'lambert'. Help me to improve this 'sergio.ibarra@usp.br'. thx")
  }

  geometry <- st_sfc(st_point(c(ref_lon, ref_lat)))
  centro <- st_sf(a = 1, geometry, crs = crs)
  x <- st_coordinates(centro) [, 1]
  y <- st_coordinates(centro) [, 2]
  if(max_dom == 1){
    # d01
    xmind01 <- x - dxd01*e_we[1]/2
    ymind01 <- y - dxd01*e_sn[1]/2
    xmaxd01 <- x + dxd01*e_we[1]/2
    ymaxd01 <- y + dxd01*e_sn[1]/2
    p01 = st_sf(d = 1, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind01,xmaxd01,xmaxd01,xmind01,xmind01),    # x
                   c(ymind01,ymind01,ymaxd01,ymaxd01,ymind01))))),# y
      crs = crs)
    doms <- rbind(p01)
      print(doms)
      return(mapview(doms, alpha.regions = 0, ...))

  } else if(max_dom == 2){
    # d01
    xmind01 <- x - dxd01*e_we[1]/2
    ymind01 <- y - dxd01*e_sn[1]/2
    xmaxd01 <- x + dxd01*e_we[1]/2
    ymaxd01 <- y + dxd01*e_sn[1]/2
    # d02
    xmind02 <- xmind01 + i_parent_start[2]*dxd01
    ymind02 <- ymind01 + j_parent_start[2]*dxd01
    xmaxd02 <- xmind02 + dxd02*e_we[2]
    ymaxd02 <- ymind02 + dxd02*e_sn[2]

    p01 = st_sf(d = 1, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind01,xmaxd01,xmaxd01,xmind01,xmind01),    # x
                   c(ymind01,ymind01,ymaxd01,ymaxd01,ymind01))))),# y
      crs = crs)
    p02 = st_sf(d = 2, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind02,xmaxd02,xmaxd02,xmind02,xmind02),    # x
                   c(ymind02,ymind02,ymaxd02,ymaxd02,ymind02))))),# y
      crs = crs)

    doms <- rbind(p01, p02)
    print(doms)
    return(list(mapview(doms, alpha.regions = 0, ...), doms))

  } else if(max_dom == 3){
    # d01
    xmind01 <- x - dxd01*e_we[1]/2
    ymind01 <- y - dxd01*e_sn[1]/2
    xmaxd01 <- x + dxd01*e_we[1]/2
    ymaxd01 <- y + dxd01*e_sn[1]/2
    # d02
    xmind02 <- xmind01 + i_parent_start[2]*dxd01
    ymind02 <- ymind01 + j_parent_start[2]*dxd01
    xmaxd02 <- xmind02 + dxd02*e_we[2]
    ymaxd02 <- ymind02 + dxd02*e_sn[2]
    # d03
    xmind03 <- xmind02 + i_parent_start[3]*dxd02
    ymind03 <- ymind02 + j_parent_start[3]*dxd02
    xmaxd03 <- xmind03 + dxd02*e_we[3]
    ymaxd03 <- ymind03 + dxd02*e_sn[3]
    p01 = st_sf(d = 1, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind01,xmaxd01,xmaxd01,xmind01,xmind01),    # x
                   c(ymind01,ymind01,ymaxd01,ymaxd01,ymind01))))),# y
      crs = crs)
    p02 = st_sf(d = 2, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind02,xmaxd02,xmaxd02,xmind02,xmind02),    # x
                   c(ymind02,ymind02,ymaxd02,ymaxd02,ymind02))))),# y
      crs = crs)
    p03 = st_sf(d = 3, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind03,xmaxd03,xmaxd03,xmind03,xmind03),    # x
                   c(ymind03,ymind03,ymaxd03,ymaxd03,ymind03))))), # y
      crs = crs)
    doms <- rbind(p01, p02, p03)
    print(doms)
    return(list(mapview(doms, alpha.regions = 0, ...), doms))

  } else if(max_dom == 4){
    # d01
    xmind01 <- x - dxd01*e_we[1]/2
    ymind01 <- y - dxd01*e_sn[1]/2
    xmaxd01 <- x + dxd01*e_we[1]/2
    ymaxd01 <- y + dxd01*e_sn[1]/2
    # d02
    xmind02 <- xmind01 + i_parent_start[2]*dxd01
    ymind02 <- ymind01 + j_parent_start[2]*dxd01
    xmaxd02 <- xmind02 + dxd02*e_we[2]
    ymaxd02 <- ymind02 + dxd02*e_sn[2]
    # d03
    xmind03 <- xmind02 + i_parent_start[3]*dxd02
    ymind03 <- ymind02 + j_parent_start[3]*dxd02
    xmaxd03 <- xmind03 + dxd02*e_we[3]
    ymaxd03 <- ymind03 + dxd02*e_sn[3]
    # d04
    xmind04 <- xmind03 + i_parent_start[4]*dxd03
    ymind04 <- ymind03 + j_parent_start[4]*dxd03
    xmaxd04 <- xmind04 + dxd03*e_we[4]
    ymaxd04 <- ymind04 + dxd03*e_sn[4]

    p01 = st_sf(d = 1, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind01,xmaxd01,xmaxd01,xmind01,xmind01),    # x
                   c(ymind01,ymind01,ymaxd01,ymaxd01,ymind01))))),# y
      crs = crs)
    p02 = st_sf(d = 2, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind02,xmaxd02,xmaxd02,xmind02,xmind02),    # x
                   c(ymind02,ymind02,ymaxd02,ymaxd02,ymind02))))),# y
      crs = crs)
    p03 = st_sf(d = 3, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind03,xmaxd03,xmaxd03,xmind03,xmind03),    # x
                   c(ymind03,ymind03,ymaxd03,ymaxd03,ymind03))))), # y
      crs = crs)
    p04 = st_sf(d = 4, geometry = st_sfc(
      st_polygon(
        list(cbind(c(xmind04,xmaxd04,xmaxd04,xmind04,xmind04),    # x
                   c(ymind04,ymind04,ymaxd04,ymaxd04,ymind04))))), # y
      crs = crs)
    doms <- rbind(p01, p02, p03, p04)
    print(doms)
    return(list(mapview(doms, alpha.regions = 0, ...), doms))

  }

}
