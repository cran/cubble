## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE,
  message = FALSE
)

library(cubble)

## -----------------------------------------------------------------------------
make_cubble(spatial = stations, temporal = meteo,
            key = id, index = date, coords = c(long, lat))

## ----echo = TRUE--------------------------------------------------------------
(res <- make_cubble(spatial = stations_sf, temporal = meteo_ts))
class(res)
class(res$ts[[1]])

## -----------------------------------------------------------------------------
climate_flat |> as_cubble(key = id, index = date, coords = c(long, lat))

## ----echo = TRUE--------------------------------------------------------------
path <- system.file("ncdf/era5-pressure.nc", package = "cubble")
raw <- ncdf4::nc_open(path)
as_cubble(raw)

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  as_cubble(raw, vars = "q",
#            long_range = seq(-180, 180, 1), lat_range = seq(-90, 90, 1))

## -----------------------------------------------------------------------------
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
x <- stars::read_stars(tif)
as_cubble(x, index = band)

## -----------------------------------------------------------------------------
dt <- climate_flat |> 
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("OGC:CRS84")) |> 
  sftime::st_as_sftime()
dt |> as_cubble(key = id, index = date)

