
<!-- date: "October 29, 2019" -->

## wrftools

This package does 3 things

1.  help you to define domains
2.  extract data as points of spatial features and bricks
3.  returns st\_bbox from wrf files

I’m not an expert in WRF but i need to use it. One of the very
frustating and tricky part was to make a correct namelist.wps. Hours and
hours trying to get the perfect domains. This functions solves this part
while providing a dynamic maps for actually viewing where are your
domains.

## Install

``` r
remotes::install_github("atmoschem/wrftools")
```

## wrf\_domains (better for low latitude regions)

wrf\_domains returns a list of:

1.  the map
2.  the sf object

The arguments of wrf\_domains are the same arguments of namelist.wps. It
include default values but you can change them accordingly.

``` r
library(wrftools)
a <- wrf_domains()
a[[1]] # leaflet
```

![](https://i.imgur.com/7giwGp6.png)

## xtractor

(inspired in NCL from DCA/IAG/USP)

Which returns data.frame ready for ggplot2. The class of Time is POSIXct

*points must have a column named “Stations”*

``` r
data(cetesb)
cetesb <- cetesb[!is.na(cetesb$Station), "Station"]
#use your wrfout
wrf <- "wrfoA.nc"
t2 = c("T2", "o3", "no", "no2", "PM10", "PM2_5_DRY")
df <- xtractor(atmos = wrf, vars = t2, points = cetesb)

head(df)
# Simple feature collection with 6 features and 31 fields
# geometry type:  POINT
# dimension:      XY
# bbox:           xmin: -47.05721 ymin: -22.90252 xmax: -47.05721 ymax: -22.90252
# epsg (SRID):    4326
# proj4string:    +proj=longlat +datum=WGS84 +no_defs
#           Station           o3       T2        co           no         no2
# 1 Campinas-Centro 0.0136891948 291.5070 0.6858493 1.186951e-03 0.017522896
# 2 Campinas-Centro 0.0016979938 291.0912 0.2327648 1.000002e-06 0.010418361
# 3 Campinas-Centro 0.0005303844 290.4462 0.1234838 1.000000e-06 0.006545659
# 4 Campinas-Centro 0.0005190288 290.2115 0.1193006 1.000000e-06 0.006521403
# 5 Campinas-Centro 0.0007871886 289.7922 0.1592883 1.000001e-06 0.008744552
# 6 Campinas-Centro 0.0007235801 289.3625 0.1483000 1.000000e-06 0.008334729
#         U10        V10       o3_nei   T2_nei    co_nei       no_nei     no2_nei
# 1 -3.828670  1.6248466 0.0134919608 291.6059 0.6849796 7.290264e-04 0.017353176
# 2 -3.457098  1.3129230 0.0020016744 291.1794 0.2680614 1.000003e-06 0.011742138
# 3 -3.317390  1.3360039 0.0006680896 290.6286 0.1547405 1.000001e-06 0.008042991
# 4 -3.180607  0.5178746 0.0006177527 290.2608 0.1540482 1.000000e-06 0.008429846
# 5 -2.915890  0.1141155 0.0008107356 289.7930 0.1822766 1.000001e-06 0.010076069
# 6 -2.709186 -0.2479147 0.0008365429 289.3382 0.1891523 1.000000e-06 0.010676160
#     U10_nei    V10_nei      o3_mean  T2_mean   co_mean      no_mean    no2_mean
# 1 -3.785948  1.6207790 0.0134919608 291.6059 0.6849796 7.290264e-04 0.017353176
# 2 -3.376240  1.2453150 0.0020016744 291.1794 0.2680614 1.000003e-06 0.011742138
# 3 -3.203223  1.1476679 0.0006680896 290.6286 0.1547405 1.000001e-06 0.008042991
# 4 -3.003846  0.5310050 0.0006177527 290.2608 0.1540482 1.000000e-06 0.008429846
# 5 -2.734402  0.1705305 0.0008107356 289.7930 0.1822766 1.000001e-06 0.010076069
# 6 -2.506655 -0.1514282 0.0008365429 289.3382 0.1891523 1.000000e-06 0.010676160
#    U10_mean   V10_mean        o3_sd T2_sd       co_sd        no_sd       no2_sd
# 1 -3.785948  1.6207790 8.958741e-04    NA 0.001191089 4.948589e-04 0.0001534891
# 2 -3.376240  1.2453150 2.975019e-04    NA 0.029171536 2.649172e-12 0.0011591850
# 3 -3.203223  1.1476679 1.706330e-04    NA 0.036130629 4.944615e-13 0.0017461727
# 4 -3.003846  0.5310050 1.354053e-04    NA 0.042510566 3.130691e-13 0.0023126512
# 5 -2.734402  0.1705305 5.089485e-05    NA 0.028024180 1.430528e-13 0.0016047246
# 6 -2.506655 -0.1514282 1.193552e-04    NA 0.043639222 2.625485e-13 0.0025344522
#   U10_sd V10_sd                Time                    geometry
# 1     NA     NA 2018-04-19 00:00:00 POINT (-47.05721 -22.90252)
# 2     NA     NA 2018-04-19 01:00:00 POINT (-47.05721 -22.90252)
# 3     NA     NA 2018-04-19 02:00:00 POINT (-47.05721 -22.90252)
# 4     NA     NA 2018-04-19 03:00:00 POINT (-47.05721 -22.90252)
# 5     NA     NA 2018-04-19 04:00:00 POINT (-47.05721 -22.90252)
# 6     NA     NA 2018-04-19 05:00:00 POINT (-47.05721 -22.90252)
#                    LT
# 1 2018-04-18 21:00:00
# 2 2018-04-18 22:00:00
# 3 2018-04-18 23:00:00
# 4 2018-04-19 00:00:00
# 5 2018-04-19 01:00:00
# 6 2018-04-19 02:00:00
```

and raster bricks

``` r
library(cptcity)
r <- xtractor(atmos = wrf, vars = t2, points = cetesb[1, ], return_list = T)
#  A list with raster and data.frame
sp::spplot(r$raster[[2]], "layer.60",
           scales = list(draw = T), col.regions = cpt(1224))
```

![](https://i.imgur.com/j4d5ei5.png)

## Contributing

If you see any errors and know how to improve the function, or add new
functions write an issue and a pull request, please. I will be ver
grateful, for sure, 100%.
